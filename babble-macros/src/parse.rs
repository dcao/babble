#![allow(clippy::expl_impl_clone_on_copy)]

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    custom_punctuation,
    parse::{Parse, ParseStream},
    Expr, Ident, LitStr, Token,
};

custom_punctuation!(LeftRightArrow, <=>);

enum Arrow {
    Right(Token![=>]),
    LeftRight(LeftRightArrow),
}

impl Parse for Arrow {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        if let Ok(arr) = input.parse::<Token![=>]>() {
            Ok(Arrow::Right(arr))
        } else {
            let arr = input.parse::<LeftRightArrow>()?;
            Ok(Arrow::LeftRight(arr))
        }
    }
}

impl ToTokens for Arrow {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Arrow::Right(arr) => arr.to_tokens(tokens),
            Arrow::LeftRight(arr) => arr.to_tokens(tokens),
        }
    }
}

struct RewriteRule {
    name: Ident,
    lhs: Expr,
    rhs: Expr,
    arrow: Arrow,
    condition: Option<(Token![if], Expr)>,
}

impl Parse for RewriteRule {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        let lhs: Expr = input.parse()?;
        let arrow: Arrow = input.parse()?;
        let rhs: Expr = input.parse()?;
        let condition;
        if let Ok(if_) = input.parse::<Token![if]>() {
            condition = Some((if_, input.parse()?))
        } else {
            condition = None;
        }
        input.parse::<Token![;]>()?;
        Ok(RewriteRule {
            name,
            lhs,
            rhs,
            arrow,
            condition,
        })
    }
}

impl ToTokens for RewriteRule {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let RewriteRule {
            name,
            lhs,
            rhs,
            arrow,
            condition,
        } = self;
        let name_string = name.to_string();
        let name = LitStr::new(&name_string, self.name.span());
        let output;

        if let Some((if_, cond)) = condition {
            output = quote! {
                ::egg::rewrite!(#name; #lhs #arrow #rhs #if_ #cond)
            };
        } else {
            output = quote! {
                ::egg::rewrite!(#name; #lhs #arrow #rhs)
            };
        }

        output.to_tokens(tokens);
    }
}

pub(crate) struct RewriteRules {
    rules: Vec<RewriteRule>,
}

impl Parse for RewriteRules {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let mut rules = Vec::new();
        while !input.is_empty() {
            rules.push(input.parse()?);
        }
        Ok(RewriteRules { rules })
    }
}

impl ToTokens for RewriteRules {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let rules = &self.rules;
        let output = quote! {
            ::std::vec![ #(#rules,)* ]
        };
        output.to_tokens(tokens);
    }
}
