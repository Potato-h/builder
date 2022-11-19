// surpass warning that goes from macro expansion in syn::{parenthesized!, bracketed!, braced!}
#![allow(clippy::mixed_read_write_in_expression)]

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote, ToTokens};
use std::collections::HashSet;
use syn::{
    braced,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    spanned::Spanned,
    token::{Bracket, Pound},
    Attribute, Error, Ident, Type, Visibility,
};

const PARSING_ATTRIBUTE_ERROR: &str = "expected `builder(each = \"...\")`";

#[allow(dead_code)]
#[derive(Debug, Clone)]
struct OneGenericType {
    ident: Ident,
    lt: syn::Token![<],
    inner: Type,
    gt: syn::Token![>],
}

impl Parse for OneGenericType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(OneGenericType {
            ident: input.parse()?,
            lt: input.parse()?,
            inner: input.parse()?,
            gt: input.parse()?,
        })
    }
}

fn extract_generic(ty: Type, ident: &str) -> Option<Type> {
    syn::parse::<OneGenericType>(ty.into_token_stream().into())
        .ok()
        .filter(|ty| ty.ident == ident)
        .map(|ty| ty.inner)
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
struct BuilderAttribute {
    pound_token: Pound,
    bracket_token: Bracket,
    args: BuilderAttributeArgs,
}

#[derive(Debug, Clone)]
struct BuilderAttributeArgs {
    path: syn::Path,
    paren_token: syn::token::Paren,
    ident: syn::Ident,
    eq: syn::Token![=],
    value: syn::LitStr,
}

impl Parse for BuilderAttributeArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;

        Ok(BuilderAttributeArgs {
            path: input.parse()?,
            paren_token: syn::parenthesized!(content in input),
            ident: content.parse()?,
            eq: content.parse()?,
            value: content.parse()?,
        })
    }
}

impl ToTokens for BuilderAttributeArgs {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.path.to_tokens(tokens);
        self.paren_token.surround(tokens, |tokens| {
            self.ident.to_tokens(tokens);
            self.eq.to_tokens(tokens);
            self.value.to_tokens(tokens);
        });
    }
}

impl Parse for BuilderAttribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content_attr;

        let attr = BuilderAttribute {
            pound_token: input.parse()?,
            bracket_token: syn::bracketed!(content_attr in input),
            args: content_attr.parse()?,
        };

        let err = Err(Error::new(attr.args.span(), PARSING_ATTRIBUTE_ERROR));

        Some(attr)
            .filter(|attr| attr.args.ident == "each")
            .filter(|attr| !attr.args.value.value().contains(char::is_whitespace))
            .map(Ok)
            .unwrap_or(err)
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
enum BuilderType {
    Option(Type),
    Vec(Type),
    Other(Type),
}
impl BuilderType {
    fn generic(&self) -> Type {
        match self {
            BuilderType::Option(ty) => ty.clone(),
            BuilderType::Vec(ty) => ty.clone(),
            _ => panic!("Try to get generic from unknown variant"),
        }
    }

    fn inner(&self) -> Type {
        match self {
            BuilderType::Option(ty) => {
                syn::parse(quote! { std::option::Option<#ty> }.into()).unwrap()
            }
            BuilderType::Vec(ty) => syn::parse(quote! { std::vec::Vec<#ty> }.into()).unwrap(),
            BuilderType::Other(ty) => ty.clone(),
        }
    }
}

impl Parse for BuilderType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ty: Type = input.parse()?;

        Ok(if let Some(ty) = extract_generic(ty.clone(), "Option") {
            BuilderType::Option(ty)
        } else if let Some(ty) = extract_generic(ty.clone(), "Vec") {
            BuilderType::Vec(ty)
        } else {
            BuilderType::Other(ty)
        })
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
struct BuilderField {
    attr: Option<BuilderAttribute>,
    vis: Visibility,
    ident: Ident,
    sep_col: syn::Token![:],
    ty: BuilderType,
}

struct AttributeInner {
    path: syn::Path,
    tokens: proc_macro2::TokenStream,
}

impl From<Attribute> for AttributeInner {
    fn from(Attribute { path, tokens, .. }: Attribute) -> Self {
        AttributeInner { path, tokens }
    }
}

impl ToTokens for AttributeInner {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.path.to_tokens(tokens);
        self.tokens.to_tokens(tokens);
    }
}

impl Parse for BuilderField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(BuilderField {
            attr: input
                .call(Attribute::parse_outer)?
                .into_iter()
                .find(|attr| attr.path.is_ident("builder"))
                .map(|attr| {
                    syn::parse(attr.to_token_stream().into()).map_err(|_| {
                        Error::new_spanned(AttributeInner::from(attr), PARSING_ATTRIBUTE_ERROR)
                    })
                })
                .transpose()?,
            vis: input.parse()?,
            ident: input.parse()?,
            sep_col: input.parse()?,
            ty: input.parse()?,
        })
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
struct BuilderSyntax {
    vis: syn::Visibility,
    struct_token: syn::Token![struct],
    struct_name: syn::Ident,
    braces: syn::token::Brace,
    fields: Punctuated<BuilderField, syn::Token![,]>,
}

impl Parse for BuilderSyntax {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;

        Ok(BuilderSyntax {
            vis: input.parse()?,
            struct_token: input.parse()?,
            struct_name: input.parse()?,
            braces: braced!(content in input),
            fields: content.parse_terminated(BuilderField::parse)?,
        })
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as BuilderSyntax);
    let name = input.struct_name;
    let vis = input.vis;
    let builder_name = format_ident!("{name}Builder");

    let (optional_types, optional_idents): (Vec<_>, Vec<_>) = input
        .fields
        .iter()
        .filter_map(|field| {
            if let BuilderType::Option(ref ty) = field.ty {
                Some((ty, field.ident.clone()))
            } else {
                None
            }
        })
        .unzip();

    let (vec_types, (vec_idents, scalar_idents)): (Vec<_>, (Vec<_>, Vec<_>)) = input
        .fields
        .iter()
        .filter_map(|field| field.attr.as_ref().map(|attr| (attr, field)))
        .map(|(attr, field)| {
            (
                Ident::new(&attr.args.value.value(), Span::call_site()),
                field,
            )
        })
        .map(|(scalar, field)| (field.ty.generic(), (field.ident.clone(), scalar)))
        .unzip();

    let special_idents: HashSet<_> = optional_idents
        .iter()
        .chain(vec_idents.iter())
        .chain(scalar_idents.iter())
        .collect();

    let scalar_overlap: HashSet<_> = scalar_idents.iter().collect();

    let (vec_impl_types, vec_impl_idents): (Vec<_>, Vec<_>) = vec_types
        .iter()
        .zip(vec_idents.iter())
        .filter(|(_, ident)| !scalar_overlap.contains(ident))
        .unzip();

    let (other_types, other_idents): (Vec<_>, Vec<_>) = input
        .fields
        .iter()
        .filter(|field| !special_idents.contains(&field.ident))
        .map(|field| (field.ty.inner(), field.ident.clone()))
        .unzip();

    let other_idents_errors = other_idents
        .iter()
        .map(|ident| format!("field {ident} was not set"));

    quote! {
        #vis struct #builder_name {
            #(#optional_idents: std::option::Option<#optional_types>,)*
            #(#vec_idents: std::vec::Vec<#vec_types>,)*
            #(#other_idents: std::option::Option<#other_types>,)*
        }

        impl #name {
            fn builder() -> #builder_name {
                #builder_name {
                    #(#optional_idents: std::option::Option::None,)*
                    #(#vec_idents: std::vec::Vec::new(),)*
                    #(#other_idents: std::option::Option::None,)*
                }
            }
        }

        impl #builder_name {
            #(
                fn #optional_idents(&mut self, #optional_idents: #optional_types) -> &mut Self {
                    self.#optional_idents = std::option::Option::Some(#optional_idents);
                    self
                }
            )*

            #(
                fn #scalar_idents(&mut self, #scalar_idents: #vec_types) -> &mut Self {
                    self.#vec_idents.push(#scalar_idents);
                    self
                }
            )*

            #(
                fn #vec_impl_idents(&mut self, #vec_impl_idents: std::vec::Vec<#vec_impl_types>) -> &mut Self {
                    self.#vec_impl_idents = #vec_impl_idents;
                    self
                }
            )*

            #(
                fn #other_idents(&mut self, #other_idents: #other_types) -> &mut Self {
                    self.#other_idents = std::option::Option::Some(#other_idents);
                    self
                }
            )*

            fn build(&mut self) -> std::result::Result<#name, String> {
                std::result::Result::Ok(#name {
                    #(#optional_idents: self.#optional_idents.clone(),)*
                    #(#vec_idents: self.#vec_idents.clone(),)*
                    #(#other_idents: self.#other_idents.as_ref().map(|field| field.clone()).ok_or(#other_idents_errors)?,)*
                })
            }
        }
    }.into()
}
