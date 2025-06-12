use syn::{Attribute, Type};
use quote::quote;
use proc_macro::TokenStream;
use proc_macro2::Ident;
use syn::{parse_macro_input, Data};

enum ResolvedFieldType {
    Option(Type),
    VecWithEach(Ident, Type),
    Default
}

#[proc_macro_derive(Builder, attributes(builder))]
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

    let mut builder_fields = vec![];
    let mut builder_fields_name_only = vec![];
    let mut setter_methods = vec![];
    let mut fields_with_check = vec![];
    let mut errors = vec![];

    for field in &fields {
        let name = &field.ident;
        let ty = &field.ty;
        // construct builder fields, Option to all fields
        builder_fields.push(quote!{#name:std::option::Option<#ty>});
        // construct builder fields, contains name only
        builder_fields_name_only.push(quote!{#name});

        // setter methods for the builder
        // for Non-option member, parameter type is itself
        // for Option member parameter type is nested type
        // vec<std::option::Option<u32>>, vec<std::option::Option<vec<std::option::Option<u32>>>>, std::option::Option<u32>, std::option::Option<std::option::Option<u32>>

        // build method, check if any Non-option in struct is std::option::Option::None in builder, then return error.
        // if Option in struct is not set, then set as std::option::Option::None

        match resolve_field_type(name, ty, &field.attrs, &mut errors) {
            ResolvedFieldType::Option(inner_type) => {
                setter_methods.push(quote! {
                    pub fn #name(&mut self, #name: #inner_type) -> &mut Self {
                        self.#name = std::option::Option::Some(std::option::Option::Some(#name));
                        self
                    }
                });
                fields_with_check.push(quote! {#name: self.#name.take().unwrap_or(std::option::Option::None)});
            },
            ResolvedFieldType::VecWithEach(each_method_name, inner_type) => {
                setter_methods.push(quote! {
                    pub fn #each_method_name(&mut self, #each_method_name: #inner_type) -> &mut Self {
                        if self.#name.is_none() {
                            self.#name = std::option::Option::Some(vec![]);
                        }
                        let items = self.#name.as_mut().unwrap();
                        items.push(#each_method_name);
                        self
                    }
                });
                fields_with_check.push(quote! {#name: self.#name.take().unwrap_or(vec![])});
            },
            ResolvedFieldType::Default => {
                setter_methods.push(quote! {
                    pub fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = std::option::Option::Some(#name);
                        self
                    }
                });
                fields_with_check.push(quote! {
                    #name: self.#name.take().ok_or(format!("field '{}' was not set", stringify!(#name)))?
                });
            }
        }
    }

    if !errors.is_empty() {
        return errors
            .into_iter()
            .map(syn::Error::into_compile_error)
            .collect::<proc_macro2::TokenStream>()
            .into();
    }

    let build_method = quote! {
        pub fn build(&mut self) -> std::result::Result<#struct_name, String> {
            Ok(#struct_name {
                #(#fields_with_check),*
            })
        }
    };

    let expanded = quote! {
        
        pub struct #builder_name {
            #(#builder_fields),*
        }

        impl #builder_name {
            pub fn new() -> Self {
                Self {
                    #(#builder_fields_name_only: std::option::Option::None),*
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

fn resolve_field_type(name: &std::option::Option<Ident>, ty: &Type, attrs: &Vec<Attribute>, errors: &mut Vec<syn::Error>) -> ResolvedFieldType {
    let mut each_method_name = std::option::Option::None;
    let mut builder_each_attr = std::option::Option::None;
    for attr in attrs {
        if attr.path().is_ident("builder") {
            // Parse #[builder(each = "xyz")]
            each_method_name = parse_builder_attr(&attr, errors);
            if each_method_name.is_some() {
                builder_each_attr = std::option::Option::Some(attr);
                break;
            }
            errors.push(syn::Error::new_spanned(attr, "builder attribute must have each section."));
            return ResolvedFieldType::Default;
        }
    }

    if let Type::Path(type_path) = ty {
        if let std::option::Option::Some(seg) = type_path.path.segments.last() {
            if seg.ident == "Option" {
                if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                    if let std::option::Option::Some(syn::GenericArgument::Type(inner_type)) = args.args.first() {
                        // Option member, parameter type is inner type
                        return ResolvedFieldType::Option(inner_type.clone());
                    }
                }
            } else if seg.ident == "Vec" && each_method_name.is_some() {
                if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                    if let std::option::Option::Some(syn::GenericArgument::Type(inner_type)) = args.args.first() {
                        let each_method_name = syn::Ident::new(
                            &each_method_name.unwrap(),
                            seg.ident.span()
                        );
                        return ResolvedFieldType::VecWithEach(each_method_name, inner_type.clone());
                    }
                }
            } else if seg.ident != "Vec" && each_method_name.is_some() {
                errors.push(syn::Error::new_spanned(builder_each_attr, "`[builder(each=\"..\")] can only be used on Vec."));
            }
        }
    }
    ResolvedFieldType::Default
}

fn parse_builder_attr(attr: &Attribute, errors: &mut Vec<syn::Error>) -> std::option::Option<String> {
    let meta = attr.parse_args().ok()?;
    match meta {
        syn::Meta::NameValue(nv) => {
            if !nv.path.is_ident("each") {
                errors.push(syn::Error::new_spanned(
                    &attr.meta,
                    "expected `builder(each = \"...\")`",
                ));
            }
            if let syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Str(s), .. }) = nv.value {
                std::option::Option::Some(s.value())
            } else {
                std::option::Option::None
            }
        }
        _ => std::option::Option::None
    }

}