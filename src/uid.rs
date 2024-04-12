use std::{
    hash::{DefaultHasher, Hash, Hasher},
    time::SystemTime,
};

use syn::{spanned::Spanned, Ident};

pub fn uid<T: Spanned>(from: &T) -> Ident {
    let span = from.span();
    let mut hasher = DefaultHasher::new();
    span.source_text().hash(&mut hasher);
    SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .hash(&mut hasher);
    let id = hasher.finish();
    Ident::new(&format!("__uid_{}", id), span)
}
