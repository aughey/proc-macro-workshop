use helper::DeriveInputWrapper;
use proc_macro::TokenStream;
use quote::quote;

fn do_work(input: syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream>
{
    let input = DeriveInputWrapper::new(input);

    let name = input.name();

    let fields = input.as_data_struct()
        .and_then(|s| s.named_fields())
        .ok_or_else(|| syn::Error::new_spanned(name, "Only structs with named fields are supported"))?;

    let field_write = fields.named.iter()
        .map(|f| {
            let name = &f.ident;
            quote! {
                .field(stringify!(#name), &self.#name)
            }
        });

 
    let impl_debug = quote! {
        impl std::fmt::Debug for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                f.debug_struct(stringify!(#name))
                #(#field_write)*
                    .finish()
            }
        }
    };

    Ok(impl_debug)
}

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {

    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    match do_work(input) {
        Ok(output) => output.into(),
        Err(err) => err.to_compile_error().into(),
    }
}
