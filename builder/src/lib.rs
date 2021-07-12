use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, DeriveInput, Field, Ident, Lit, Meta, MetaNameValue, NestedMeta, Path, Type,
};

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

fn vec_inner_type(ty: &Type) -> Option<&Type> {
    match ty {
        syn::Type::Path(ref type_path) => match type_path.path {
            syn::Path {
                ref segments,
                leading_colon: _,
            } if segments.len() == 1 && segments[0].ident == "Vec" => {
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

fn builder_each_ident(field: &Field) -> Option<Ident> {
    if field.attrs.is_empty() {
        return None;
    }
    match field.attrs[0].parse_meta() {
        Ok(meta) => match meta {
            syn::Meta::List(ref meta_list) if meta_list.path.is_ident("builder") => {
                match meta_list.nested.first() {
                    Some(NestedMeta::Meta(Meta::NameValue(MetaNameValue {
                        lit: Lit::Str(s),
                        path,
                        ..
                    }))) if path.is_ident("each") => {
                        // eprintln!("lit: {:?}", s);
                        // let s = s.parse().unwrap();
                        let path = s.parse::<Path>();
                        match &path {
                            Ok(meta) => meta.get_ident().cloned(),
                            Err(_) => None,
                        }
                    }
                    _ => None,
                }
            }
            _ => None,
        },
        Err(_) => None,
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
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
        } else if let Some(_inner_type) = vec_inner_type(&field.ty) {
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

        if let Some(_inner_type) = vec_inner_type(&field.ty) {
            quote! {
                #name: Vec::new()
            }
        } else {
            quote! {
                #name: None
            }
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
        } else if let Some(inner_type) = vec_inner_type(&field.ty) {
            let each_ident = builder_each_ident(&field);

            match each_ident {
                Some(id) => {
                    if id != *name {
                        quote! {
                            pub fn #id(&mut self, #id: #inner_type) -> &mut Self {
                                self.#name.push(#id);
                                self
                            }
                            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                                self.#name = #name;
                                self
                            }
                        }
                    } else {
                        quote! {
                            pub fn #id(&mut self, #id: #inner_type) -> &mut Self {
                                self.#id.push(#id);
                                self
                            }
                        }
                    }
                }

                None => {
                    quote! {
                        pub fn #name(&mut self, #name: #ty) -> &mut Self {
                            self.#name = #name;
                            self
                        }
                    }
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
        } else if let Some(_) = vec_inner_type(&field.ty) {
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
