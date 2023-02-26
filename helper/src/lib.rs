use syn::{DeriveInput, Data::Struct, Ident, NestedMeta};

pub struct SynPathCollector<'a> {
    pub paths: Vec<&'a syn::Path>
}

impl<'a> syn::visit::Visit<'a> for SynPathCollector<'a> {
    fn visit_path(&mut self, i: &'a syn::Path) {
        self.paths.push(i);
        for segment in &i.segments {
            syn::visit::visit_path_segment(self, segment);
        }
    }
}

impl<'a> SynPathCollector<'a> {
    pub fn visit(ty: &'a syn::Type) -> Vec<&'a syn::Path> {
        let mut collector = SynPathCollector { paths: Vec::new() };
        syn::visit::visit_type(&mut collector, ty);
        collector.paths
    }
}

pub struct DeriveInputWrapper {
    input: DeriveInput
}

pub struct NamedFieldWrapper<'a> {
    field: &'a syn::FieldsNamed
}

pub struct DataStructWrapper<'a> {
    data: &'a syn::DataStruct
}

pub struct AttributeWrapper<'a> {
    attr: &'a syn::Attribute
}

pub struct MetaWrapper {
    meta: syn::Meta
}

impl MetaWrapper {
    
    pub fn key_value(&self) -> Option<syn::MetaNameValue> {
        match &self.meta {
            syn::Meta::NameValue(nv) => Some(nv.clone()),
            _ => None
        }
    }
    pub fn key_value_string(&self) -> Option<(String,String)> {
        let kv = self.key_value()?;
        match &kv.lit {
            syn::Lit::Str(s) => Some((kv.path.get_ident().unwrap().to_string(), s.value())),
            _ => None
        }
    }
}

pub struct MetaListWrapper {
    list: syn::MetaList
}

impl MetaListWrapper {
    pub fn iter(&self) -> impl Iterator<Item = &NestedMeta> {
        self.list.nested.iter()
    }
}

impl AttributeWrapper<'_> {
    pub fn like_fn(&self) -> Option<(String, (String,String))> {
        // I'm looking for an attribute that looks like foo(bar = blech)
        let meta = self.attr.parse_meta().ok()?;

        let metalist = match meta {
            syn::Meta::List(list) => list,
            _ => return None
        };

        let first_ret = metalist.path.get_ident()?.to_string();

        let first_nested = metalist.nested.first()?;

        let name_value = match first_nested {
            NestedMeta::Meta(syn::Meta::NameValue(nv)) => nv,
            _ => return None
        };

        let second_ret = name_value.path.get_ident()?.to_string();
        let third_ret = match &name_value.lit {
            syn::Lit::Str(s) => s.value().to_string(),
            _ => return None
        };
        Some((first_ret, (second_ret, third_ret)))
    }
}

impl<'a> AttributeWrapper<'a> {
    pub fn meta(&self) -> Option<MetaWrapper> {
        match self.attr.parse_meta() {
            Ok(m) => Some(MetaWrapper { meta: m }),
            _ => None
        }
    }
    pub fn get(self) -> &'a syn::Attribute {
        self.attr
    }
    pub fn meta_list(&self) -> Option<MetaListWrapper> {
        match self.attr.parse_meta() {
            Ok(syn::Meta::List(l)) => Some( MetaListWrapper { list: l }),
            _ => None
        }
    }
}

pub struct FieldWrapper<'a> {
    field: &'a syn::Field
}

impl<'a> FieldWrapper<'a> {
    pub fn ident(&self) -> Option<&Ident> {
        self.field.ident.as_ref()
    }
    pub fn ty(&self) -> &syn::Type {
        &self.field.ty
    }
    pub fn attributes(&self) -> impl Iterator<Item = AttributeWrapper<'a>> {
        self.field.attrs.iter().map(|a| AttributeWrapper { attr: a })
    }

    pub fn metas(&self) -> impl Iterator<Item = MetaWrapper> + 'a {
        let ret = self.attributes().filter_map(|a| a.meta());
        ret
    }

    pub fn associated_types(&'a self, generics : &Vec<Ident>) -> Vec<&'a syn::Path> {
         // Get all the path segements in this type
         let paths = SynPathCollector::visit(self.ty());
         // Find paths that match the pattern
         let paths = paths.into_iter().filter_map(|p| {
             // We're looking for segments that have a generic
             // parameter followed by anything else
             let mut i = p.segments.iter();
             let first = i.next()?;
             if generics.contains(&first.ident) && i.next().is_some() {
                 return Some(p);
             }
             None
         });
         paths.collect::<Vec<_>>()
    }

    pub fn type_path(&self) -> Option<&syn::TypePath> {
        if let syn::Type::Path(p) = self.ty() {
            return Some(p);
        }
        None
    }

    pub fn phantom_data_type(&self) -> Option<syn::Type> {
        if let syn::Type::Path(p) = self.ty() {
            if let Some(segment) = p.path.segments.first() {
                if segment.ident == "PhantomData" {
                    // We found a PhantomData field
                    // pull out path arguments from this path
                        if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                            // We found angle bracketed arguments
                            // pull out the first one
                            if let Some(arg) = args.args.first() {
                                if let syn::GenericArgument::Type(ty) = arg {
                                    // We found a type argument
                                    // return it
                                    return Some(ty.clone());
                                }
                            }
                        }
                    }
            }
        }
        None
    }

    pub fn attribute_as_string(&self, key: &str) -> Option<syn::Lit> {
        let kvs = self.metas().filter_map(|m| m.key_value());
        for kv in kvs {
            if kv.path.get_ident().unwrap().to_string() == key {
                return Some(kv.lit);
            }
        }
        None
    }
}

impl<'a> NamedFieldWrapper<'a> {
    pub fn iter(&self) -> impl Iterator<Item = FieldWrapper<'a>> {
        self.field.named.iter().map(|f| FieldWrapper { field: f })
    }
}

impl<'a> DataStructWrapper<'a> {
    pub fn named_fields(&self) -> Option<NamedFieldWrapper<'a>> {
        match &self.data.fields {
            syn::Fields::Named(f) => Some(NamedFieldWrapper { field: f }),
            _ => return None
        }
    }
}

impl DeriveInputWrapper {
    pub fn new(input: DeriveInput) -> Self {
        Self { input }
    }

    pub fn add_trait_bounds(&mut self, bounds: syn::TypeParamBound, except: &Vec<syn::Ident>) {
        for param in self.input.generics.type_params_mut() {
            if except.contains(&param.ident) {
                continue;
            }
            param.bounds.push(bounds.clone());
        }
    }

    pub fn generics(&self) -> &syn::Generics {
        &self.input.generics
    }

    pub fn generics_mut(&mut self) -> &mut syn::Generics {
        &mut self.input.generics
    }

    pub fn name<'a>(&'a self) -> &'a Ident {
        &self.input.ident
    }

    pub fn as_data_struct<'a>(&'a self) -> Option<DataStructWrapper<'a>> {
        match &self.input.data {
            Struct(s) => Some(DataStructWrapper { data: s }),
            _ => None
        }
    }
}

#[cfg(test)]
mod tests {
    use quote::format_ident;

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

    #[test]
    fn test_simple_struct() {
        let test_struct_str = r#"
        pub struct Command {
            #[each = "arg"]
            args: Vec<String>,
            #[builder(each = "arg")]
            args: Vec<String>,
        }
        "#;

        let test_struct = test_struct_from_string(test_struct_str);

        let fields = test_struct.fields.iter().map(|f| FieldWrapper { field: &f }).collect::<Vec<_>>();

        assert_eq!(fields.len(), 2);

        let first_field = fields.first().unwrap();

        let field_attrs = first_field.attributes().collect::<Vec<_>>();

        assert_eq!(field_attrs.len(), 1);

        let first_attr = field_attrs.first().unwrap();

        assert_eq!(first_attr.meta().unwrap().key_value_string().unwrap(),("each".to_string(),"arg".to_string()));

        let second_field = fields.last().unwrap();

        let field_attrs = second_field.attributes().collect::<Vec<_>>();

        assert_eq!(field_attrs.len(), 1);

        let first_attr = field_attrs.first().unwrap();

        let likefn = first_attr.like_fn();

        assert!(likefn.is_some());
    }

    #[test]
    fn test_phantom_data_type() {
        let test_struct_str = r#"
        pub struct Field<T> {
            marker: PhantomData<T>,
            string: S,
            #[debug = "0b{:08b}"]
            bitmask: u8,
        }
        "#;

        let test_struct = test_struct_from_string(test_struct_str);

        let fields = test_struct.fields.iter().map(|f| FieldWrapper { field: &f }).collect::<Vec<_>>();

        assert_eq!(fields.len(), 3);

        let f = fields.last().unwrap();
        let phantom_data_type = f.phantom_data_type();
        assert!(phantom_data_type.is_none());

        let f = fields.first().unwrap();
        let phantom_data_type = f.phantom_data_type();
        assert!(phantom_data_type.is_some());

        let t = phantom_data_type.unwrap();
        if let syn::Type::Path(p) = t {
            assert_eq!(p.path.segments.first().unwrap().ident, "T");
        } else {
            panic!("This should be a path");
        }
    }

    #[test]
    fn test_associated_types() {
        let test_struct_str = r#"
        pub struct Field<T> {
            marker: Asdf<T::Value>
        }
        "#;

        let test_struct = test_struct_from_string(test_struct_str);

        let fields = test_struct.fields.iter().map(|f| FieldWrapper { field: &f }).collect::<Vec<_>>();

        assert_eq!(fields.len(), 1);

        let f = fields.first().unwrap();

        let generics = vec![format_ident!("T")];

        let associated_types = f.associated_types(&generics);

        assert_eq!(associated_types.len(), 1);

        let t = associated_types.first().unwrap();

        assert_eq!(t.segments.len(), 2);

        let segments = t.segments.iter().map(|s| s.ident.to_string()).collect::<Vec<_>>();

        assert_eq!(segments, vec!["T".to_string(), "Value".to_string()]);
    }

    #[test]
    fn test_trait_behaviour() {
        let test_struct_str = r#"
        pub struct Field<T: Trait> {
            values: Vec<T::Value>,
        }
        "#;

        let test_struct = test_struct_from_string(test_struct_str);

        let params = test_struct.generics.type_params().collect::<Vec<_>>();

        assert_eq!(params.len(), 1);

        let first_param = params.first().unwrap();

        let ident = first_param.ident.to_string();

        assert_eq!(ident, "T");

        let has_trait_bound = first_param.bounds.iter().find(|b| {
            if let syn::TypeParamBound::Trait(_) = b {
                true
            } else {
                false
            }
        });

        assert!(has_trait_bound.is_some());
     
    }
}
