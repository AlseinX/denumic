use proc_macro2::Span;
use syn::{punctuated::Punctuated, spanned::Spanned, *};

pub fn generic_params_to_args(params: &Generics) -> PathArguments {
    let Generics {
        lt_token: Some(lt_token),
        params,
        gt_token: Some(gt_token),
        ..
    } = params
    else {
        return PathArguments::None;
    };

    PathArguments::AngleBracketed(AngleBracketedGenericArguments {
        args: params
            .iter()
            .map(|param| match param {
                GenericParam::Type(TypeParam { ident, .. }) => {
                    GenericArgument::Type(Type::Path(TypePath {
                        qself: None,
                        path: ident.clone().into(),
                    }))
                }
                GenericParam::Lifetime(LifetimeParam { lifetime, .. }) => {
                    GenericArgument::Lifetime(lifetime.clone())
                }
                GenericParam::Const(ConstParam { ident, .. }) => {
                    GenericArgument::Const(Expr::Path(ExprPath {
                        attrs: Default::default(),
                        qself: None,
                        path: ident.clone().into(),
                    }))
                }
            })
            .collect(),
        colon2_token: None,
        lt_token: *lt_token,
        gt_token: *gt_token,
    })
}

pub fn method_params_to_args(
    this: &Ident,
    params: &mut Punctuated<FnArg, Token![,]>,
) -> Punctuated<Expr, Token![,]> {
    let mut pid = 0;
    params
        .iter_mut()
        .map(|param| {
            Expr::Path(ExprPath {
                attrs: vec![],
                qself: None,
                path: match param {
                    FnArg::Receiver(_) => this.clone(),
                    FnArg::Typed(pat) => {
                        if let Pat::Ident(pat) = pat.pat.as_ref() {
                            pat.ident.clone()
                        } else {
                            let ident = Ident::new(format!("__arg_{}", pid).as_str(), pat.span());
                            pid += 1;
                            *pat.pat = Pat::Ident(PatIdent {
                                attrs: vec![],
                                by_ref: None,
                                mutability: None,
                                ident: ident.clone(),
                                subpat: None,
                            });
                            ident
                        }
                    }
                }
                .into(),
            })
        })
        .collect()
}

pub fn impl_generics(tr: &Generics, ty: &Generics) -> Generics {
    let mut result = ty.clone();
    for param in tr.params.iter() {
        match param {
            GenericParam::Type(
                tr @ TypeParam {
                    attrs,
                    ident,
                    colon_token,
                    bounds,
                    ..
                },
            ) => {
                if let Some(ty) = result.params.iter_mut().find_map(|p| match p {
                    GenericParam::Type(t) if t.ident == *ident => Some(t),
                    _ => None,
                }) {
                    ty.attrs.extend_from_slice(attrs);
                    ty.colon_token = ty.colon_token.or(*colon_token);
                    ty.bounds.extend(bounds.iter().cloned());
                    ty.eq_token = None;
                    ty.default = None;
                } else {
                    let mut tr = tr.clone();
                    tr.eq_token = None;
                    tr.default = None;
                    result.params.push(GenericParam::Type(tr));
                }
            }
            GenericParam::Lifetime(
                tr @ LifetimeParam {
                    attrs,
                    lifetime,
                    colon_token,
                    bounds,
                },
            ) => {
                if let Some(ty) = result.params.iter_mut().find_map(|p| match p {
                    GenericParam::Lifetime(t) if t.lifetime == *lifetime => Some(t),
                    _ => None,
                }) {
                    ty.attrs.extend_from_slice(attrs);
                    ty.colon_token = ty.colon_token.or(*colon_token);
                    ty.bounds.extend(bounds.iter().cloned());
                } else {
                    result.params.push(GenericParam::Lifetime(tr.clone()));
                }
            }
            GenericParam::Const(tr @ ConstParam { attrs, ident, .. }) => {
                if let Some(ty) = result.params.iter_mut().find_map(|p| match p {
                    GenericParam::Const(t) if t.ident == *ident => Some(t),
                    _ => None,
                }) {
                    ty.attrs.extend_from_slice(attrs);
                    ty.eq_token = None;
                    ty.default = None;
                } else {
                    let mut tr = tr.clone();
                    tr.eq_token = None;
                    tr.default = None;
                    result.params.push(GenericParam::Const(tr));
                }
            }
        }
    }
    if let Some(tr) = &tr.where_clause {
        if let Some(ty) = &mut result.where_clause {
            ty.predicates.extend(tr.predicates.iter().cloned());
        } else {
            result.where_clause = Some(tr.clone());
        }
    }
    result
}

pub fn remove_defaults(generics: &mut Generics) {
    for p in &mut generics.params {
        match p {
            GenericParam::Type(p) => {
                p.eq_token = None;
                p.default = None
            }
            GenericParam::Const(p) => {
                p.eq_token = None;
                p.default = None
            }
            _ => {}
        }
    }
}

pub fn merge_cfgs<'a>(attrs: impl IntoIterator<Item = &'a Vec<Attribute>>) -> Vec<Attribute> {
    attrs
        .into_iter()
        .flatten()
        .filter(|a| matches!(a.style, AttrStyle::Outer) && a.meta.path().is_ident("cfg"))
        .cloned()
        .collect()
}

pub fn self_expr(span: Span) -> Expr {
    Expr::Path(ExprPath {
        attrs: Default::default(),
        qself: None,
        path: Token![self](span).into(),
    })
}
