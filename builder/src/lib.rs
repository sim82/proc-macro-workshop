use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder_name = format_ident!("{}Builder", name);
    let fields = match input.data {
        syn::Data::Struct(ref data) => match data.fields {
            syn::Fields::Named(ref fields) => &fields.named,
            _ => unimplemented!(),
        },
        // &data.fields,
        _ => unimplemented!(),
    };
    let builder_decl = fields.iter().map(|field| {
        let name = field.ident.as_ref().unwrap();
        let ty = &field.ty;
        quote! {
            #name: Option<#ty>
        }
    });
    let builder_init = fields.iter().map(|field| {
        let name = field.ident.as_ref().unwrap();
        quote! {
            #name: None
        }
    });
    let builder_setter = fields.iter().map(|field| {
        let name = field.ident.as_ref().unwrap();
        let ty = &field.ty;
        quote! {
            fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });

    let expanded = quote! {
        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_init),*
                }
            }
        }
        pub struct #builder_name {
            #(#builder_decl),*
        }
        impl #builder_name {
            #(#builder_setter)*
        }
    };
    proc_macro::TokenStream::from(expanded)
}
