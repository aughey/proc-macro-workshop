use helper::DeriveInputWrapper;
use proc_macro::TokenStream;
use quote::{quote};


fn do_work(input: syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream>
{
    let input = DeriveInputWrapper::new(input);

    let name = input.name().clone();

    let fields = input.as_data_struct()
        .and_then(|s| s.named_fields())
        .ok_or_else(|| syn::Error::new_spanned(&name, "Only structs with named fields are supported"))?;
   
    // Look for fields with PhantomData type
    let phantom_data_types = fields.iter().filter_map(|f| {
        f.phantom_data_type()
    });

    // pull out the ones with a path
    let phantom_data_types = phantom_data_types.filter_map(|p| {
        if let syn::Type::Path(p) = p {
            let p = p.path;
            let ident = p.get_ident();
            if let Some(ident) = ident {
                return Some(ident.clone());
            }
        }
        None
    }).collect::<Vec<_>>();

    let generics = input.generics();
    let mut generics = generics.clone();

    let bounds : syn::TypeParamBound = syn::parse_quote!(std::fmt::Debug);
    for param in generics.type_params_mut() {
        if phantom_data_types.contains(&param.ident) {
            continue;
        }
        param.bounds.push(bounds.clone());
    }
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let field_write = fields.iter()
        .map(|f| {
            let name = f.ident().unwrap();

            let value = if let Some(debugstr) = f.attribute_as_string("debug") {
                quote! { &format_args!(#debugstr, &self.#name) }
            } else {
                quote! { &self.#name }
            };

            quote! {
                .field(stringify!(#name), #value)
            }
        });

 
    let impl_debug = quote! {
        impl #impl_generics std::fmt::Debug for #name #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                f.debug_struct(stringify!(#name))
                #(#field_write)*
                    .finish()
            }
        }
    };

    Ok(impl_debug)
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {

    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    match do_work(input) {
        Ok(output) => output.into(),
        Err(err) => err.to_compile_error().into(),
    }
}
