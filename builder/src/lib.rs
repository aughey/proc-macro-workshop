use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};
use quote::{quote, format_ident};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let _ = input;

    let deriveinput =  parse_macro_input!(input as DeriveInput);

    let ident = &deriveinput.ident;
    let builder_name = format_ident!("{}Builder", ident);

    let out = quote!(
        pub struct #builder_name {
            executable: Option<String>,
            args: Option<Vec<String>>,
            env: Option<Vec<String>>,
            current_dir: Option<String>,
        }
        impl #ident {
            pub fn builder() -> #builder_name {
                #builder_name {
                    executable: None,
                    args: None,
                    env: None,
                    current_dir: None,
                }
            }
        }
        impl #builder_name {
            fn executable(&mut self, executable: String) -> &mut Self {
                self.executable = Some(executable);
                self
            }
            fn args(&mut self, args: Vec<String>) -> &mut Self {
                self.args = Some(args);
                self
            }
            fn env(&mut self, env: Vec<String>) -> &mut Self {
                self.env = Some(env);
                self
            }
            fn current_dir(&mut self, current_dir: String) -> &mut Self {
                self.current_dir = Some(current_dir);
                self
            }

            pub fn build(&mut self) -> Result<#ident, Box<dyn std::error::Error>> {
                Ok(#ident {
                    executable: self.executable.clone().ok_or("missing executable")?,
                    args: self.args.clone().ok_or("missing args")?,
                    env: self.env.clone().ok_or("missing env")?,
                    current_dir: self.current_dir.clone().ok_or("missing current_dir")?,
                })
            }
        }
    );
    out.into()
}
