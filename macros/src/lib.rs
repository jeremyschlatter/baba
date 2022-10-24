use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};
use quote::quote;

#[proc_macro_derive(BabaProps, attributes(props))]
pub fn baba_probs_derive(input: TokenStream) -> TokenStream {
    fn get_props(attrs: Vec<syn::Attribute>, ident: &syn::Ident) -> syn::punctuated::Punctuated<syn::NestedMeta, syn::token::Comma> {
        for a in attrs {
            if let syn::Meta::List(l) = a.parse_meta().unwrap() {
                if l.path.is_ident("props") {
                    return l.nested;
                }
            }
        }
        panic!("missing props attribute for {ident}");
    }
    let ast = parse_macro_input!(input as DeriveInput);
    let mut arms: Vec<proc_macro2::TokenStream> = vec![];
    let t = ast.ident;
    if let syn::Data::Enum(e) = ast.data {
        for v in e.variants {
            let id = v.ident;
            let props = get_props(v.attrs, &id);
            match v.fields {
                syn::Fields::Unit => arms.push(quote!{ #t::#id => (#props) }),
                syn::Fields::Unnamed(_) => arms.push(quote!{ #t::#id(..) => (#props) }),
                syn::Fields::Named(_) => (),
            }
        }
    }
    let p = get_props(ast.attrs, &t);
    TokenStream::from(quote!{
        impl #t {
            fn props(&self) -> (#p) {
                match self {
                    #(#arms, )*
                }
            }
        }
    })
}
