use syn::{DeriveInput, Data::Struct, Ident, NestedMeta};

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

    pub fn add_trait_bounds(&mut self, bounds: syn::TypeParamBound, except: &Vec<syn::Type>) {
        for param in &mut self.input.generics.type_params_mut() {
            if except.contains(&param) {
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
}
