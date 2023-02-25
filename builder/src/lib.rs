// ignore unused variables
#![allow(unused_variables)]

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput};

struct BuilderField<'a> {
    name: &'a syn::Ident,
    ty: &'a syn::Type,
    optional: bool,
}

impl<'a> BuilderField<'a> {
    fn is_optional(&self) -> bool {
        self.optional
    }
    fn nt(&self) -> (&'a syn::Ident, &'a syn::Type) {
        (self.name, self.ty)
    }
    fn storage(&self) -> proc_macro2::TokenStream {
        let (name, ty) = self.nt();
        quote!(
            #name: Option<#ty>
        )
    }

    fn setter(&self) -> proc_macro2::TokenStream {
        let (name, ty) = self.nt();
        quote!(
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = #name;
                self
            }
        )
    }

  fn init(&self) -> proc_macro2::TokenStream {
    proc_macro2::TokenStream::new()
  }
}

fn field_to_builder_field<'a>(field: &'a syn::Field) -> Option<BuilderField<'a>> {
    let name = field.ident.as_ref()?;
    let ty = &field.ty;

    let type_as_path = if let syn::Type::Path(type_as_path) = ty {
        Some(type_as_path)
    } else {
        None
    }?;

    let first_segment = type_as_path.path.segments.first()?;
    let is_option = first_segment.ident == "Option";

    if is_option {
        // Pull out what's inside the angle brackets of the option
        let args = &first_segment.arguments;
        let angle_bracketed = if let syn::PathArguments::AngleBracketed(angle_bracketed) = args {
            angle_bracketed
        } else {
            return None;
        };

        let first_arg = angle_bracketed.args.first()?;
        let inner_type = if let syn::GenericArgument::Type(inner_type) = first_arg {
            inner_type
        } else {
            return None;
        };

        Some(BuilderField {
                name: name,
                ty: inner_type,
                optional:true
            })
    } else {
        Some(BuilderField {
            name: name,
            ty: ty,
            optional:false
        })
    }
}

fn settable_fields(fields: &syn::FieldsNamed) -> Vec<BuilderField> {
    let values = fields
        .named
        .iter()
        .map(|f| field_to_builder_field(f))
        .filter(|f| f.is_some())
        .map(|f| f.unwrap())
        .collect::<Vec<_>>();
    values
}

fn settable_fields_from_struct_data<'a>(struct_data: &'a syn::DataStruct) -> Vec<BuilderField<'a>> {
    let struct_fields = match &struct_data.fields {
        syn::Fields::Named(f) => f,
        _ => return vec![],
    };
    let fields = settable_fields(&struct_fields);
    fields
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let _ = input;

    let deriveinput = parse_macro_input!(input as DeriveInput);

    let struct_data = match deriveinput.data {
        syn::Data::Struct(s) => s,
        _ => panic!("Only structs can be derived with this macro"),
    };

    let fields = settable_fields_from_struct_data(&struct_data);

    let ident = &deriveinput.ident;
    let builder_name = format_ident!("{}Builder", ident);

    let builder_fields = fields.iter().map(|f| f.storage()).collect::<Vec<_>>();

    let setters = fields.iter().map(|f| f.setter()).collect::<Vec<_>>();
    let init_fields = fields.iter().map(|f| f.init()).collect::<Vec<_>>();

    let builder_struct_def = quote! {
        #[derive(Default)]
        pub struct #builder_name {
            #(#builder_fields),*
        }
    };
    
    eprintln!("{}", builder_struct_def);

    let out = quote!(
        #builder_struct_def

        impl #ident {
            pub fn builder() -> #builder_name {
                #builder_name::default()
            }
        }

        // impl #builder_name {
        //     #(#setters)*

        //     pub fn build(self) -> Result<#ident, Box<dyn std::error::Error>> {
        //         Ok(#ident {
        //             #(#init_fields),*
        //         })
        //     }
        // }
    );
    eprintln!("{}", out);
    out.into()
    //TokenStream::new()
}

#[cfg(test)]
mod tests {
 
    use super::*;

    fn test_struct() -> syn::DataStruct {
        let test_struct_str = r#"
            struct Test {
                required: String,
                optional: Option<String>,
            }
        "#;

        let parsed_str: syn::Item = syn::parse_str(test_struct_str).unwrap();

        let s = if let syn::Item::Struct(s) = parsed_str {
            s
        } else {
            panic!("This should be a struct");
        };

        
        
        if let syn::Data::Struct(s) = s.data {
            s
        } else {
            panic!("This should be a struct");
        }
    }

    #[test]
    fn test_field_to_optional_field() {
        let s = test_struct();

        let datastruct = match s.fields {
            syn::Fields::Named(f) => f,
            _ => panic!("This should be a named struct"),
        };

        let fields = settable_fields_from_struct_data(&s);

        assert_eq!(fields.len(), 2);
        let first_field = fields.first().unwrap();

    }
}
