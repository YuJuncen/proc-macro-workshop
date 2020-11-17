extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::{quote, quote_spanned};
use syn::{Data, DeriveInput, Error, Field, Fields, GenericArgument, Ident, Lit, Meta, MetaNameValue, PathArguments, Type, export::Span, parse_macro_input, spanned::Spanned};

fn get_fields(data: &Data) -> impl Iterator<Item = &Field> {
    match data {
        Data::Struct(s) => match s.fields {
            Fields::Named(ref fields) => fields.named.iter(),
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

fn get_builder_fields(data: &Data) -> TokenStream {
    let rec = get_fields(data).map(|field| {
        let name = &field.ident;
        let typ = &field.ty;
        if !is_optional_type(&field.ty) && repeated_field_name(field).is_none() {
            quote_spanned! {field.span()=>
                #name: ::core::option::Option<#typ>
            }
        } else {
            quote_spanned! {field.span()=>
                #name: #typ
            }
        }
    });
    TokenStream::from(quote! {
        #(#rec),*
    })
}

fn get_builder_default(data: &Data) -> TokenStream {
    let rec = get_fields(data).map(|field| {
        let name = &field.ident;
        if repeated_field_name(field).is_some() {
            quote_spanned! {field.span()=>
                #name: ::std::vec::Vec::new()
            }
        } else {
            quote_spanned! {field.span()=>
                #name: ::core::option::Option::None
            }
        }
    });
    TokenStream::from(quote! {
        #(#rec),*
    })
}

fn repeated_arg_methods(f: &Field, method_name: Ident) -> TokenStream {
    let name = &f.ident;
    let ty = extract_generic_arg(&f.ty, "Vec").and_then(|xs| xs.get(0).map(|x| *x));
    quote_spanned! {f.span()=>
        fn #method_name(&mut self, #name: #ty) -> &mut Self {
            self.#name.push(#name);
            self
        }
    }
}

fn find_named_value<'a>(meta: &'a Meta, pred: &dyn Fn(&str)->bool) -> Option<&'a MetaNameValue> {
    match meta {
        Meta::NameValue(kv) => {
            let ident = kv.path.get_ident()?;
            if pred(&ident.to_string()) {
                Some(&kv)
            } else {
                None
            }
        }
        Meta::Path(_) => None,
        Meta::List(list) => {
            if list.path.segments.iter()
                .last()
                .map(|last| last.ident.to_string() != "builder")
                .unwrap_or(true) {
                return None;
            }
            list.nested.iter().find_map(|item| match item {
                syn::NestedMeta::Meta(m) => find_named_value(m, pred),
                syn::NestedMeta::Lit(_) => None,
            })
        }
    }
}

fn find_name<'a>(meta: &'a Meta, pred: &dyn Fn(&str)->bool) -> Option<&'a Lit> {
    find_named_value(meta, pred).map(|v| &v.lit)
}

fn repeated_field_name(field: &Field) -> Option<Ident> {
    field.attrs.iter().find_map(|attr| match attr.parse_meta() {
        Ok(meta) => {
            match find_name(&meta, &|s| s == "each") {
                Some(Lit::Str(s)) => Some(Ident::new(s.value().as_str(), Span::call_site())),
                _ => None,
            }
        },
        Err(e) => {
            println!("error during parsing tag: {}", e);
            None
        }
    })
}

fn check_attr(field: &Field) -> Result<(), Error> {
    for attr in field.attrs.iter() {
        match attr.parse_meta() {
            Ok(meta) => {
                if let Some(wrong_tag) = find_named_value(&meta,&|name| name != "each") {
                    return Err(Error::new(wrong_tag.span(), r#"expected `builder(each = "...")`"#))
                }
            }
            _ => return Err(Error::new(attr.span(), "cannot parse attrs"))
        }
    }
    Ok(())
}

fn get_methods(data: &Data) -> TokenStream {
    let methods = get_fields(data).map(|field| {
        if let Some(name) = repeated_field_name(field) {
            return repeated_arg_methods(field, name);
        }
        let err = match check_attr(field) {
            Ok(()) => { quote!{} }
            Err(e) => { e.to_compile_error() }
        };

        let name = &field.ident;
        let ty = if let Some(inner) = extract_option_warped(&field.ty) {
            inner
        } else {
            &field.ty
        };
        quote_spanned! {field.span() =>
            fn #name(&mut self, #name: #ty) -> &mut Self {
                #err
                self.#name = Some(#name);
                self
            }
        }
    });
    quote! {
        #(#methods)*
    }
}

fn object_init_fields(data: &Data) -> TokenStream {
    let pairs = get_fields(data).map(|field| {
        let name = &field.ident;
        if is_optional_type(&field.ty) {
            quote_spanned! {field.span()=>
                #name: self.#name.take()
            }
        } else if repeated_field_name(field).is_some() {
            quote_spanned! {field.span()=>
                #name: self.#name.drain(..).collect()
            }
        } else {
            quote_spanned! {field.span()=>
                #name: match self.#name.take() {
                    Some(any) => any,
                    None => {
                        return Err(format!("field {} not provided.", stringify!(#name)).into());
                    },
                }
            }
        }
    });
    quote! {
        #(#pairs,)*
    }
}

fn is_optional_type(ty: &Type) -> bool {
    extract_option_warped(ty).is_some()
}

fn extract_generic_arg<'a>(ty: &'a Type, type_name: &str) -> Option<Vec<&'a Type>> {
    match ty {
        Type::Path(path) => path
            .path
            .segments
            .iter()
            .find(|item| item.ident.to_string() == type_name)
            .and_then(|opt| match &opt.arguments {
                PathArguments::AngleBracketed(gen) => Some(
                    gen.args
                        .iter()
                        .filter_map(|arg| match arg {
                            GenericArgument::Type(t) => Some(t),
                            _ => None,
                        })
                        .collect(),
                ),
                _ => None,
            }),
        Type::Ptr(p) => extract_generic_arg(&p.elem, type_name),
        Type::Reference(r) => extract_generic_arg(&r.elem, type_name),
        _ => None,
    }
}

fn extract_option_warped(ty: &Type) -> Option<&Type> {
    extract_generic_arg(ty, "Option").and_then(|xs| xs.get(0).map(|x| *x))
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder_name = Ident::new(&format!("{}Builder", name), Span::call_site());
    let fields = get_builder_fields(&input.data);
    let defaults = get_builder_default(&input.data);
    let methods = get_methods(&input.data);
    let init_fields = object_init_fields(&input.data);
    let quoted = quote! {
        struct #builder_name {
            #fields
        }
        impl #name {
            fn builder() -> self::#builder_name {
                #builder_name {
                    #defaults
                }
            }
        }
        impl #builder_name {
            #methods
        }
        impl #builder_name {
            fn build(&mut self) -> ::std::result::Result<#name, ::std::boxed::Box<dyn ::std::error::Error>> {
                Ok(#name {
                    #init_fields
                })
            }
        }
    };
    proc_macro::TokenStream::from(quoted)
}
