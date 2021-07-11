use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let _input = parse_macro_input!(input as DeriveInput);
    for attr in _input.attrs {
        println!("attr: {:?}", attr.path.get_ident());
    }

    proc_macro::TokenStream::new()
}
