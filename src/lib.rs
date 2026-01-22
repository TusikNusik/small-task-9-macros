use proc_macro::TokenStream;
use quote::quote;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();

    implement_builder_macro(&ast)
}

fn builder_each(attr: &syn::Attribute) -> syn::Result<Option<syn::Ident>> {
    if !attr.path.is_ident("builder") {
        return Ok(None);
    }

    let meta = attr.parse_meta()?;

    if let syn::Meta::List(list) = meta {
        for nested in &list.nested {
            if let syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) = nested {
                if nv.path.is_ident("each") {
                    if let syn::Lit::Str(lit) = &nv.lit {
                        return Ok(Some(syn::Ident::new(&lit.value(), lit.span())));
                    }
                } else {
                    return Err(syn::Error::new_spanned(
                        &list,
                        "expected `builder(each = \"...\")`",
                    ));
                }
            }
        }
    }
    Ok(None)
}

fn is_option(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(type_path) = ty {
        if type_path.qself.is_none() {
            let segment = type_path.path.segments.first()?;
            if segment.ident == "Option" {
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    if let Some(syn::GenericArgument::Type(inner_ty)) = args.args.first() {
                        return Some(inner_ty);
                    }
                }
            }
        }
    }
    None
}

fn vec_inner_type(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(tp) = ty {
        let seg = tp.path.segments.first()?;
        if seg.ident == "Vec" {
            if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                if let Some(syn::GenericArgument::Type(inner)) = args.args.first() {
                    return Some(inner);
                }
            }
        }
    }
    None
}

fn implement_builder_macro(ast: &syn::DeriveInput) -> TokenStream {
    let fields = match &ast.data {
        syn::Data::Struct(data) => &data.fields,
        _ => panic!(),
    };

    let mut build_fields = Vec::new();
    let mut setters = Vec::new();

    for field in fields {
        let field_name = field.ident.as_ref().unwrap();
        let field_type = &field.ty;

        let inner_type = if let Some(inner) = is_option(field_type) {
            inner
        } else {
            field_type
        };

        let each_method = match field.attrs.iter().find_map(|a| match builder_each(a) {
            Ok(val) => val.map(Ok),
            Err(e) => Some(Err(e)),
        }) {
            Some(Ok(val)) => Some(val),
            Some(Err(e)) => return e.to_compile_error().into(),
            None => None,
        };

        if let Some(each_name) = &each_method {
            let inner_type = vec_inner_type(field_type).expect("builder used on non-Vec field");

            setters.push(quote! {
                pub fn #each_name(&mut self, value: #inner_type) -> &mut Self {
                    self.#field_name
                        .get_or_insert_with(::std::vec::Vec::new)
                        .push(value);
                    self
                }
            });
        } else {
            setters.push(quote! {
                pub fn #field_name(&mut self, value: #inner_type) -> &mut Self {
                    self.#field_name = ::std::option::Option::Some(value);
                    self
                }
            });
        }

        if each_method.is_some() {
            build_fields.push(quote! {
                #field_name: self.#field_name.take().unwrap_or_default()
            });
        } else if is_option(field_type).is_some() {
            build_fields.push(quote! {
                #field_name: self.#field_name.take()
            });
        } else {
            build_fields.push(quote! {
                #field_name: self.#field_name.take().ok_or_else(|| format!("missing field: {}", stringify!(#field_name)))?
            });
        }
    }

    let generated = quote! {
        use std::error::Error;

        pub struct CommandBuilder {
            executable: ::std::option::Option<String>,
            args: ::std::option::Option<Vec<String>>,
            env: ::std::option::Option<Vec<String>>,
            current_dir: ::std::option::Option<String>,
        }

        impl Command {
            pub fn builder() -> CommandBuilder {
                CommandBuilder {
                    executable: ::std::option::Option::None,
                    args: ::std::option::Option::None,
                    env: ::std::option::Option::None,
                    current_dir: ::std::option::Option::None,
                }
            }
        }

        impl CommandBuilder {
            #(#setters)*

            pub fn build(&mut self) -> ::std::result::Result<Command, ::std::boxed::Box<dyn Error>> {
                Ok(Command {
                    #(#build_fields,)*
                })
            }
        }
    };
    generated.into()
}