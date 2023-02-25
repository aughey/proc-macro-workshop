// ignore unused variables
#![allow(unused_variables)]

use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{parse_macro_input, DeriveInput};

struct BuilderField<'a> {
    name: &'a syn::Ident,
    ty: &'a syn::Type,
    optional: bool,
}

impl<'a> BuilderField<'a> {
    #[allow(dead_code)] // used for testing
    fn is_optional(&self) -> bool {
        self.optional
    }
    #[allow(dead_code)] // used for testing
    fn name_string(&self) -> String {
        self.name.to_string()
    }
    #[allow(dead_code)] // used for testing
    fn type_string(&self) -> String {
        self.ty.to_token_stream().to_string()
    }
    fn nt(&self) -> (&'a syn::Ident, &'a syn::Type) {
        (self.name, self.ty)
    }
    fn storage(&self) -> proc_macro2::TokenStream {
        let (name, ty) = self.nt();
        if self.optional {
            quote!(
                #name: Option<#ty>
            )
        } else {
            quote!(
                #name: Option<#ty>
            )
        }
    }

    fn setter(&self) -> proc_macro2::TokenStream {
        let (name, ty) = self.nt();
        
        let set_statement = if self.optional {
            quote!(
                self.#name = Some(#name);
            )
        } else {
            quote!(
                self.#name = Some(#name);
            )
        };

        quote!(
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                #set_statement
                self
            }
        )
    }

  fn init(&self) -> proc_macro2::TokenStream {
    let (name, ty) = self.nt();
    if self.optional {
      quote!(
        #name: self.#name.clone()
      )
    } else {
      quote!(
        #name: self.#name.clone().ok_or("required field is not set")?
      )
    }
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

fn make_builder_struct(name: &syn::Ident, fields: &Vec<BuilderField>) -> proc_macro2::TokenStream {
    let builder_fields = fields.iter().map(|f| f.storage()).collect::<Vec<_>>();

    let builder_struct_def = quote! {
        #[derive(Default)]
        pub struct #name {
            #(#builder_fields),*
        }
    };
    builder_struct_def
}

fn make_builder_impl(builder_name: &proc_macro2::Ident, fields: &[BuilderField]) -> proc_macro2::TokenStream {
    let setters = fields.iter().map(|f| f.setter()).collect::<Vec<_>>();

    let out = quote! {
           impl #builder_name {
            #(#setters)*

            // pub fn build(self) -> Result<#ident, Box<dyn std::error::Error>> {
            //     Ok(#ident {
            //         #(#init_fields),*
            //     })
            // }
        }
    };
    out
}

fn make_buidler_build_fn(original_ident: &proc_macro2::Ident, fields: &[BuilderField]) -> proc_macro2::TokenStream {
    let init_fields = fields.iter().map(|f| f.init()).collect::<Vec<_>>();

    let out = quote! {
        pub fn build(&mut self) -> Result<#original_ident, Box<dyn std::error::Error>> {
            Ok(#original_ident {
                #(#init_fields),*
            })
        }
    };
    out
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

    let builder_struct_def = make_builder_struct(&builder_name, &fields);

    let builder_impl = make_builder_impl(&builder_name, &fields);

    let build_impl = make_buidler_build_fn(&ident, &fields);

    let out = quote!(
        #builder_struct_def

         #builder_impl

        impl #builder_name {
            #build_impl
        }

        impl #ident {
            pub fn builder() -> #builder_name {
                #builder_name::default()
            }
        }

    );
   // eprintln!("{}", out);
    out.into()
    //TokenStream::new()
}


#[cfg(test)]
mod tests {
 
    use super::*;

    fn test_struct() -> syn::ItemStruct {
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

        s   
    }

    fn test_fields() -> syn::FieldsNamed {
        let s = test_struct();

        let datastruct = match s.fields {
            syn::Fields::Named(f) => f,
            _ => panic!("This should be a named struct"),
        };

        datastruct
    }

    #[test]
    fn test_field_to_optional_field() {
        let fields = test_fields();
        let builder_fields = settable_fields(&fields);

        assert_eq!(builder_fields.len(), 2);
        let first_field = builder_fields.first().unwrap();

        assert_eq!(first_field.name_string(), "required");
        assert_eq!(first_field.type_string(), "String");

        let second_field = builder_fields.last().unwrap();
        assert_eq!(second_field.name_string(), "optional");
        assert_eq!(second_field.type_string(), "String");
    }

    #[test]
    fn test_struct_output() {
        let fields = test_fields();
        let builder_fields = settable_fields(&fields);

        let builder_name = format_ident!("TestBuilder");
        let builder_struct_def = make_builder_struct(&builder_name, &builder_fields);

        let expected = r#"
            #[derive(Default)]
            pub struct TestBuilder {
                required: Option<String> ,
                optional: Option<String>
            }
        "#;

        let expected = syn::parse_str::<proc_macro2::TokenStream>(expected).unwrap();
        assert_eq!(builder_struct_def.to_string(), expected.to_string());
    }

    #[test]
    fn test_impl_output() {
        let fields = test_fields();
        let builder_fields = settable_fields(&fields);

        let builder_name = format_ident!("TestBuilder");
        let builder_impl = make_builder_impl(&builder_name, &builder_fields);

        let expected = r#"
            impl TestBuilder {
                pub fn required(&mut self, required: String) -> &mut Self {
                    self.required = Some(required);
                    self
                }

                pub fn optional(&mut self, optional: String) -> &mut Self {
                    self.optional = Some(optional);
                    self
                }
            }
        "#;

        let expected = syn::parse_str::<proc_macro2::TokenStream>(expected).unwrap();
        assert_eq!(builder_impl.to_string(), expected.to_string());
    }

    #[test]
    fn test_build_fn_output() {
        let fields = test_fields();
        let builder_fields = settable_fields(&fields);

        let original_name = format_ident!("Test");
        let build_fn = make_buidler_build_fn(&original_name, &builder_fields);

        let expected = r#"
            pub fn build(& mut self) -> Result<Test, Box<dyn std::error::Error>> {
                Ok(Test {
                    required: self.required.clone().ok_or("required field is not set")? ,
                    optional: self.optional.clone()
                })
            }
        "#;

        let expected = syn::parse_str::<proc_macro2::TokenStream>(expected).unwrap();
        assert_eq!(build_fn.to_string(), expected.to_string());
    }
}
