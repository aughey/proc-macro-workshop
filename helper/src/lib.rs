use syn::{DeriveInput, Data::Struct, Ident};

pub struct DeriveInputWrapper {
    input: DeriveInput
}

pub struct DataStructWrapper<'a> {
    data: &'a syn::DataStruct
}

impl<'a> DataStructWrapper<'a> {
    pub fn named_fields(&self) -> Option<&'a syn::FieldsNamed> {
        match &self.data.fields {
            syn::Fields::Named(f) => Some(f),
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
