use syn::{DeriveInput, Data::Struct, Ident};

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

impl<'a> AttributeWrapper<'a> {
    pub fn key_value(&self) -> Option<syn::MetaNameValue> {
        match &self.attr.parse_meta() {
            Ok(syn::Meta::NameValue(nv)) => Some(nv.clone()),
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
    pub fn attributes(&self) -> impl Iterator<Item = AttributeWrapper> {
        self.field.attrs.iter().map(|a| AttributeWrapper { attr: a })
    }
    pub fn attribute(&self, name: &str) -> Option<syn::Lit> {
        if let Some(a) = self.attributes().find(|a| a.attr.path.is_ident(name)) {
            a.key_value().map(|kv| kv.lit)
        } else {
            None
        }
    }
    pub fn attribute_as_string(&self, name: &str) -> Option<String> {
        self.attribute(name).and_then(|lit| {
            match lit {
                syn::Lit::Str(s) => Some(s.value()),
                _ => None
            }
        })
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

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
