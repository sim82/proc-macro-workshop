use proc_macro::TokenStream;
use syn::{
    braced,
    parse::{Parse, ParseStream},
    parse_macro_input, DeriveInput, Expr, Ident, LitInt, Token,
};

struct Seq {
    name: Ident,
    min: LitInt,
    max: LitInt,
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![in]>()?;
        let min: LitInt = input.parse()?;
        input.parse::<Token![..]>()?;
        let max: LitInt = input.parse()?;
        let content;
        braced!(content in input);
        Ok(Seq { name, min, max })
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    // let _ = input;
    let seq = parse_macro_input!(input as Seq);
    TokenStream::new()
}
