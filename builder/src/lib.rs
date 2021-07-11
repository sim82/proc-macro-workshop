use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput, Type};

fn option_inner_type(ty: &Type) -> Option<&Type> {
    match ty {
        syn::Type::Path(ref type_path) => match type_path.path {
            syn::Path {
                ref segments,
                leading_colon: _,
            } if segments.len() == 1 && segments[0].ident == "Option" => {
                match &segments[0].arguments {
                    syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                        args,
                        colon2_token: _,
                        gt_token: _,
                        lt_token: _,
                    }) if args.len() == 1 => match args[0] {
                        syn::GenericArgument::Type(ref ty) => Some(ty),
                        _ => None,
                    },
                    _ => None,
                }
            }

            _ => None,
        },
        _ => None,
    }
}

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
        if let Some(_inner_type) = option_inner_type(ty) {
            quote! {
                #name: #ty
            }
        } else {
            quote! {
                #name: Option<#ty>
            }
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
        if let Some(inner_type) = option_inner_type(&field.ty) {
            quote! {
                pub fn #name(&mut self, #name: #inner_type) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        }
    });

    let inst_init = fields.iter().map(|field| {
        let name = field.ident.as_ref().unwrap();
        if let Some(_) = option_inner_type(&field.ty) {
            quote! {
               #name: self.#name.clone()
            }
        } else {
            quote! {
               #name: self.#name.as_ref().ok_or_else(|| Box::<dyn std::error::Error>::from(format!("{} is not set in builder", stringify!(#name))))?.clone() // meh...
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

            pub fn build(&mut self) -> std::result::Result<#name, Box<dyn std::error::Error>> {

                Ok(#name {
                    #(#inst_init),*
                })
            }
        }
    };
    proc_macro::TokenStream::from(expanded)
}
