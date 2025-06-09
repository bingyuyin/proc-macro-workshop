use syn::Type;
use quote::quote;
use proc_macro::TokenStream;
use syn::{parse_macro_input, Data};

#[proc_macro_derive(Builder)]
pub fn derive_builder(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    let struct_name = &input.ident;
    let builder_name = syn::Ident::new(&format!("{}Builder", struct_name), input.ident.span());

    // parse the fields in the struct
    let fields = match input.data {
        Data::Struct(data_struct) => match data_struct.fields {
            syn::Fields::Named(fields) => fields.named,
            _ => panic!("Builder marco only works on structs with named fields."),
        },
        _ => panic!("Builder marco only works on structs.")
    };

    // construct builder fields, Option to all fields
    let builder_fields = fields.iter().map(|field|{
        let name = &field.ident;
        let ty = &field.ty;
        quote! {#name: Option<#ty>}
    });

    // clone for another move
    let builder_fields_name = fields.iter().map(|field|{
        let name = &field.ident;
        let ty = &field.ty;
        quote! {#name}
    });

    // setter methods for the builder
    // for Non-option member, parameter type is itself
    // for Option member parameter type is nested type
    // vec<Option<u32>>, vec<Option<vec<Option<u32>>>>, Option<u32>, Option<Option<u32>>
    let setter_methods = fields.iter().map(|field| {
        let field_name = &field.ident;
        let field_type = &field.ty;
        if let Type::Path(type_path) = field_type {
            if let Some(seg) = type_path.path.segments.last() {
                if seg.ident == "Option" {
                    if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                        if let Some(syn::GenericArgument::Type(inner_type)) = args.args.first() {
                            // Option member, parameter type is inner type
                            return quote! {
                                pub fn #field_name(&mut self, #field_name: #inner_type) -> &mut Self {
                                    self.#field_name = Some(Some(#field_name));
                                    self
                                }
                            };
                        }
                    }
                }
            }
        }

        // Non-option member, parameter type is itself
        quote! {
            pub fn #field_name(&mut self, #field_name: #field_type) -> &mut Self {
                self.#field_name = Some(#field_name);
                self
            }
        }
    });

    // build method, check if any Non-option in struct is None in builder, then return error.
    // if Option in struct is not set, then set as None
    let build_method = {
        let fields_with_checks = fields.iter().map(|f| {
            let f_name = &f.ident;
            let f_type = &f.ty;
            let is_nested_option = if let Type::Path(type_path) = f_type {
                type_path.path.segments.last().map_or(false, |seg| seg.ident == "Option")
            } else {
                false
            };

            if is_nested_option {
                quote! {
                    #f_name: self.#f_name.take().unwrap_or(None)
                }
            } else {
                quote! {
                    #f_name: self.#f_name.take().ok_or(
                        format!("field '{}' was not set", stringify!(#f_name))
                    )?
                }
            }
        });
        quote! {
            pub fn build(&mut self) -> std::result::Result<#struct_name, String> {
                Ok(#struct_name {
                    #(#fields_with_checks),*
                })
            }
        }
    };

    let expanded = quote! {
        pub struct #builder_name {
            #(#builder_fields),*
        }

        impl #builder_name {
            pub fn new() -> Self {
                Self {
                    #(#builder_fields_name: None),*
                }
            }

            #(#setter_methods)*

            #build_method
        }

        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name::new()
            }
        }
    };

    TokenStream::from(expanded)

}
