// ignore unused variables
#![allow(unused_variables)]

use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{parse_macro_input, DeriveInput, spanned::Spanned };

struct BuilderField<'a> {
    field: &'a syn::Field,
    name: &'a syn::Ident,
    ty: &'a syn::Type,
    optional: bool,
    one_at_a_time: Option<(syn::Ident,syn::Type)>,
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
            quote!(
                #name: std::option::Option<#ty>
            )
    }

    fn setter(&self) -> proc_macro2::TokenStream {
        let (name, ty) = self.nt();
        
        let set_statement = 
            quote!(
                self.#name = std::option::Option::Some(#name);
            );
        
        let each_setter = if let Some(lit) = &self.one_at_a_time {
            let (lit,setty) = lit;
            let (name, ty) = self.nt();
            quote!(
                pub fn #lit(&mut self, #lit : #setty) -> &mut Self {
                    self.#name.get_or_insert_with(Vec::new).push(arg);
                    self
                }
            )
        } else {
            quote!()
        };

        quote!(
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                #set_statement
                self
            }
            #each_setter
        )
    }

  fn init(&self) -> proc_macro2::TokenStream {
    let (name, ty) = self.nt();
    if self.optional {
      quote!(
        #name: self.#name.clone()
      )
    } else {
        let err_message_lit = syn::LitStr::new(&format!("required field {} is not set", name), name.span());
      quote!(
        #name: self.#name.clone().ok_or(#err_message_lit)?
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

    let bf = if is_option {
        // Pull out what's inside the angle brackets of the option
        let args = &first_segment.arguments;

        let inner_type = get_inner_type_from_pathargs(args)?;

        BuilderField {
            field,
                name: name,
                ty: inner_type,
                optional:true,
                one_at_a_time: get_each_arg_from_field(field, &inner_type)
            }
    } else {
        BuilderField {
            field,
            name: name,
            ty: ty,
            optional:false,
            one_at_a_time: get_each_arg_from_field(field, &ty)
        }
    };

    // XXX need to clean up this
    let bf = if let Some(oaat) = &bf.one_at_a_time {
        let myname = bf.name;
        let oaatname = &oaat.0;

        if myname == oaatname {
            // Clear out the one at a time
            BuilderField {
                one_at_a_time: None,
                ..bf
            }
        } else {
            bf
        }
    } else {
        bf
    };

    Some(bf)
}

fn get_inner_type(ty: &syn::Type) -> Option<&syn::Type> {
    let type_as_path = if let syn::Type::Path(type_as_path) = ty {
        Some(type_as_path)
    } else {
        None
    }?;

    let first_segment = type_as_path.path.segments.first()?;

    get_inner_type_from_pathargs(&first_segment.arguments)
}

fn get_inner_type_from_pathargs(args: &syn::PathArguments) -> Option<&syn::Type> {  
    let inner_type = if let syn::PathArguments::AngleBracketed(inner_type) = args {
        Some(inner_type)
    } else {
        None
    }?;

    let inner_type = inner_type.args.first()?;
    let inner_type = if let syn::GenericArgument::Type(inner_type) = inner_type {
        Some(inner_type)
    } else {
        None
    }?;

    Some(inner_type)
}

fn get_attrib_pairs_for_attribute(attr: syn::Attribute) -> Vec<AttribPair> {

    let builder_meta = attr.parse_meta();
    let builder_meta = if let Ok(bm) = builder_meta {
        bm
    } else {
        return vec![];
    };
    let meta_list = if let syn::Meta::List(ml) = builder_meta {
        ml
    } else {
        return vec![];
    };

    let nested = &meta_list.nested;
    let nested_name_values = nested.iter().map(|n| {
        if let syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) = n {
            Some(nv)
        } else {
            None
        }
    }).filter(|nv| nv.is_some()).map(|nv| nv.unwrap());

    let ret = nested_name_values.map(|nv| Some(
        AttribPair {
            span: meta_list.span(),
            name: nv.path.get_ident()?.clone(),
            value: nv.lit.clone()
        }
        ))
        .filter(|v| v.is_some()).map(|v| v.unwrap());
    let ret = ret.collect::<Vec<_>>();
    ret
}

struct AttribPair {
    name: syn::Ident,
    #[allow(dead_code)]
    value: syn::Lit,
    span: proc_macro2::Span
}

fn get_attrib_pairs_for_field(myident: &str, field: &syn::Field) -> Vec<AttribPair> {
    let attrs = &field.attrs;
    let builder_attrs = attrs.iter().filter(|a| a.path.is_ident(myident));

    builder_attrs.map(|a| get_attrib_pairs_for_attribute(a.clone())).flatten().collect::<Vec<_>>()
}

fn get_each_arg_from_field<'a>(field: &'a syn::Field, ty : &syn::Type) -> Option<(syn::Ident,syn::Type)> {   
    let attrs = &field.attrs;
    let builder_attrs = attrs.iter().filter(|a| a.path.is_ident("builder"));

    let first_builder_attr = *builder_attrs.collect::<Vec<_>>().get(0)?;

    let builder_meta = first_builder_attr.parse_meta().ok()?;
    let meta_list = if let syn::Meta::List(ml) = builder_meta {
        ml
    } else {
        return None;
    };

    let nested = &meta_list.nested;
    let nested_name_values = nested.iter().map(|n| {
        if let syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) = n {
            Some(nv)
        } else {
            None
        }
    }).filter(|nv| nv.is_some()).map(|nv| nv.unwrap());

    let each_name_value = nested_name_values.filter(|nv| nv.path.is_ident("each")).map(|nv| nv.lit.clone());

    let lit = each_name_value.take(1).collect::<Vec<_>>().get(0)?.clone();

    let lit = if let syn::Lit::Str(lit) = lit {
        lit
    } else {
        return None;
    };

    let inner_type = get_inner_type(ty)?;


    Some((format_ident!("{}", lit.value()), inner_type.clone()))
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
        pub fn build(&mut self) -> std::result::Result<#original_ident, std::boxed::Box<dyn std::error::Error>> {
            std::result::Result::Ok(#original_ident {
                #(#init_fields),*
            })
        }
    };
    out
}

fn check_valid_attrs(fields: &[BuilderField]) -> syn::Result<()> {
    let valid_attributes = &["each"];
    for field in fields {
        let attribs = get_attrib_pairs_for_field("builder", field.field);
        for attrib in attribs {
            let name = attrib.name.to_string();
            if !valid_attributes.contains(&name.as_str()) {
                return Err(syn::Error::new(attrib.span, format!("expected `builder(each = \"...\")`")));
            }
        }
    }
    Ok(())
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let _ = input;

    let deriveinput = parse_macro_input!(input as DeriveInput);

    let struct_data = match deriveinput.data {
        syn::Data::Struct(s) => s,
        _ => panic!("Only structs can be derived with this macro"),
    };

    let fields = settable_fields_from_struct_data(&struct_data);

    // compile error check the field internal builder attributes
    // Valid attributes
    {
        let valid = check_valid_attrs(&fields);
        if let Err(e) = valid {
            return e.to_compile_error().into();
        }
    }

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

    fn test_struct_from_string(test_struct_str: &str) -> syn::ItemStruct {
        let parsed_str: syn::Item = syn::parse_str(test_struct_str).unwrap();

        let s = if let syn::Item::Struct(s) = parsed_str {
            s
        } else {
            panic!("This should be a struct");
        };

        s   
    }


    fn test_struct() -> syn::ItemStruct {
        let test_struct_str = r#"
            struct Test {
                required: String,
                optional: Option<String>,
            }
        "#;
        test_struct_from_string(test_struct_str)
    }

    fn buidler_fields_from_item_struct<'a>(s: &'a syn::ItemStruct) -> Vec<BuilderField<'a>> {
        match &s.fields {
            syn::Fields::Named(f) => settable_fields(&f),
            _ => vec![]
        }
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
                required: std::option::Option<String> ,
                optional: std::option::Option<String>
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
                    self.required = std::option::Option::Some(required);
                    self
                }

                pub fn optional(&mut self, optional: String) -> &mut Self {
                    self.optional = std::option::Option::Some(optional);
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
            pub fn build(& mut self) -> std::result::Result<Test, std::boxed::Box<dyn std::error::Error>> {
                std::result::Result::Ok(Test {
                    required: self.required.clone().ok_or("required field required is not set")? ,
                    optional: self.optional.clone()
                })
            }
        "#;

        let expected = syn::parse_str::<proc_macro2::TokenStream>(expected).unwrap();
        assert_eq!(build_fn.to_string(), expected.to_string());
    }

    #[test]
    fn test_field_attr_behavior() {
        let test_struct_str = r#"
        pub struct Command {
            #[builder(each = "arg")]
            args: Vec<String>,
        }
        "#;

        let s = test_struct_from_string(test_struct_str);
        let fields = match &s.fields {
            syn::Fields::Named(f) => f,
            _ => panic!("This should be a named struct"),
        };

        let first_field = fields.named.first().unwrap();
        let field_attrs = &first_field.attrs;
        assert_eq!(field_attrs.len(), 1);
        let attr = field_attrs.first().unwrap();
        assert!(attr.path.is_ident("builder"));
        let meta = attr.parse_meta().unwrap();
        let meta_list = match meta {
            syn::Meta::List(l) => l,
            _ => panic!("This should be a list"),
        };

    }

    #[test]
    fn test_one_at_a_time_builder() {
        let test_struct_str = r#"
        pub struct Command {
            #[builder(each = "arg")]
            args: Vec<String>,
        }
        "#;

        let s = test_struct_from_string(test_struct_str);
        let builder_fields = buidler_fields_from_item_struct(&s);

        assert_eq!(builder_fields.len(), 1);

        let first_field = builder_fields.first().unwrap();

        assert!(first_field.one_at_a_time.is_some());

        let field_setter = first_field.setter();

        let expected = r#"
            pub fn args(&mut self, args: Vec<String>) -> &mut Self {
                self.args = std::option::Option::Some(args);
                self
            }
            pub fn arg(&mut self, arg: String) -> &mut Self {
                self.args.get_or_insert_with(Vec::new).push(arg);
                self
            }
        "#;
        let expected = syn::parse_str::<proc_macro2::TokenStream>(expected).unwrap();
        assert_eq!(field_setter.to_string(), expected.to_string());
    }

    #[test]
    fn test_get_attrib_pairs_for_field() {
        let test_struct_str = r#"
        pub struct Command {
            #[builder(each = "arg")]
            args: Vec<String>,
        }
        "#;

        let s = test_struct_from_string(test_struct_str);
        let fields = match &s.fields {
            syn::Fields::Named(f) => f,
            _ => panic!("This should be a named struct"),
        };

        let first_field = fields.named.first().unwrap();
        let attrib_pairs = get_attrib_pairs_for_field("builder", first_field);

        assert_eq!(attrib_pairs.len(), 1);
        let attrib_pair = attrib_pairs.first().unwrap();
        assert_eq!(attrib_pair.name, "each");
    }
}
