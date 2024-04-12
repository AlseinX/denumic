use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    parse2,
    punctuated::Punctuated,
    spanned::Spanned,
    token::{Brace, Bracket, Paren},
    Arm, AttrStyle, Attribute, Block, Error, Expr, ExprCall, ExprLit, ExprMatch, ExprMethodCall,
    ExprPath, FieldPat, FnArg, GenericParam, Ident, ImplItem, ImplItemConst, ImplItemFn,
    ImplItemType, Index, ItemEnum, ItemImpl, ItemTrait, MacroDelimiter, Member, Meta, MetaList,
    Pat, PatIdent, PatRest, PatStruct, Path, PathSegment, Result, Signature, Stmt, Token,
    TraitItem, TraitItemFn, Type, TypePath, Variant, Visibility,
};

use crate::utils::{
    generic_params_to_args, impl_generics, merge_cfgs, method_params_to_args, remove_defaults,
    self_expr,
};

pub struct Args {
    tr: Path,
    tr_def: ItemTrait,
}

impl Parse for Args {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut tr = input.parse::<Path>()?;
        let tr_def = input.parse::<ItemTrait>()?;
        let tr_args = generic_params_to_args(&tr_def.generics);
        tr.segments.last_mut().unwrap().arguments = tr_args;
        Ok(Args { tr, tr_def })
    }
}

pub fn handle(Args { tr, tr_def }: Args, mut ty_def: ItemEnum) -> Result<TokenStream> {
    let ty_args = generic_params_to_args(&ty_def.generics);
    let ty = Box::new(Type::Path(TypePath {
        qself: None,
        path: PathSegment {
            ident: ty_def.ident.clone(),
            arguments: ty_args.clone(),
        }
        .into(),
    }));
    let r#impl = handle_impl(&tr, &ty, &tr_def, &mut ty_def)?;
    let mut result = quote! {
        #ty_def
        #r#impl
    };
    generate_conv(&ty_def, &mut result);
    Ok(result)
}

fn handle_impl(
    tr: &Path,
    ty: &Type,
    tr_def: &ItemTrait,
    ty_def: &mut ItemEnum,
) -> Result<ItemImpl> {
    let mut generics = impl_generics(&tr_def.generics, &ty_def.generics);
    remove_defaults(&mut generics);
    let mut items = Vec::new();
    handle_items(tr, tr_def, ty_def, &mut items)?;
    Ok(ItemImpl {
        attrs: merge_cfgs([&tr_def.attrs, &ty_def.attrs]),
        defaultness: None,
        unsafety: tr_def.unsafety,
        impl_token: Token![impl](tr_def.ident.span().resolved_at(Span::mixed_site())),
        generics,
        trait_: Some((
            None,
            tr.clone(),
            Token![for](tr_def.ident.span().resolved_at(Span::mixed_site())),
        )),
        self_ty: Box::new(ty.clone()),
        brace_token: Brace(
            ty_def
                .brace_token
                .span
                .span()
                .resolved_at(Span::mixed_site()),
        ),
        items,
    })
}

fn handle_items(
    tr: &Path,
    tr_def: &ItemTrait,
    ty_def: &mut ItemEnum,
    result: &mut Vec<ImplItem>,
) -> Result<()> {
    for item in &tr_def.items {
        match item {
            TraitItem::Fn(item) => handle_method(tr, item, ty_def, result)?,
            TraitItem::Const(item) => {
                let expr = extract_impls(ty_def, &item.ident)?;
                result.push(ImplItem::Const(ImplItemConst {
                    attrs: merge_cfgs([&item.attrs]),
                    vis: Visibility::Inherited,
                    defaultness: None,
                    const_token: item.const_token,
                    ident: item.ident.clone(),
                    generics: item.generics.clone(),
                    colon_token: item.colon_token,
                    ty: item.ty.clone(),
                    eq_token: Token![=](item.semi_token.span),
                    expr,
                    semi_token: item.semi_token,
                }))
            }
            TraitItem::Type(item) => {
                let expr = extract_impls(ty_def, &item.ident)?;
                let ty = if let Expr::Lit(ExprLit {
                    lit: syn::Lit::Str(lit),
                    ..
                }) = expr
                {
                    lit.parse()?
                } else {
                    parse2(expr.to_token_stream())?
                };
                result.push(ImplItem::Type(ImplItemType {
                    attrs: merge_cfgs([&item.attrs]),
                    vis: Visibility::Inherited,
                    defaultness: None,
                    type_token: item.type_token,
                    ident: item.ident.clone(),
                    generics: item.generics.clone(),
                    eq_token: Token![=](item.semi_token.span),
                    ty,
                    semi_token: item.semi_token,
                }))
            }
            _ => {}
        }
    }
    Ok(())
}

fn extract_impls(ty_def: &mut ItemEnum, ident: &Ident) -> Result<Expr> {
    if let Some((i, expr)) = ty_def.attrs.iter().enumerate().find_map(|(i, a)| {
        if let Meta::NameValue(m) = &a.meta {
            if m.path.is_ident(ident) {
                return Some((i, m.value.clone()));
            }
        }
        None
    }) {
        ty_def.attrs.remove(i);
        Ok(expr)
    } else {
        Err(Error::new(
            ty_def.ident.span(),
            format_args!(
                "no value for the const {} found within the attributes",
                ident
            ),
        ))
    }
}

fn handle_method(
    tr: &Path,
    item: &TraitItemFn,
    ty_def: &mut ItemEnum,
    result: &mut Vec<ImplItem>,
) -> Result<()> {
    if !check_receiver(item)? {
        return Ok(());
    }

    let span = item
        .default
        .as_ref()
        .map(Spanned::span)
        .unwrap_or_else(|| item.semi_token.span());
    let mut make_call = make_call(tr, &item.sig);
    let mut arms = Vec::with_capacity(ty_def.variants.len());
    for v in &mut ty_def.variants {
        arms.push(handle_delegate_calls(&mut make_call, v)?);
    }

    result.push(ImplItem::Fn(ImplItemFn {
        attrs: item
            .attrs
            .iter()
            .cloned()
            .chain([Attribute {
                pound_token: Token![#](span),
                style: AttrStyle::Outer,
                bracket_token: Bracket(span),
                meta: Meta::List(MetaList {
                    path: Ident::new("inline", span).into(),
                    delimiter: MacroDelimiter::Paren(Paren(span)),
                    tokens: quote!(always),
                }),
            }])
            .collect(),
        vis: syn::Visibility::Inherited,
        defaultness: None,
        sig: item.sig.clone(),
        block: Block {
            brace_token: Brace(span),
            stmts: vec![Stmt::Expr(
                Expr::Match(ExprMatch {
                    attrs: Vec::new(),
                    match_token: Token![match](span),
                    expr: Box::new(self_expr(span)),
                    brace_token: Brace(span),
                    arms,
                }),
                None,
            )],
        },
    }));
    Ok(())
}

fn check_receiver(item: &TraitItemFn) -> Result<bool> {
    if let Some(FnArg::Receiver(_)) = item.sig.inputs.first() {
        Ok(true)
    } else if item.default.is_some() {
        Ok(false)
    } else {
        Err(Error::new(
                item.span(),
                format_args!(
                    "denumic trait method {} should either take a self receiver or have a default implementation",
                    item.sig.ident
                ),
            ))
    }
}

fn make_call(tr: &Path, sig: &Signature) -> impl FnMut(&Ident) -> Box<Expr> {
    let mut inputs = sig.inputs.clone();
    let func = Box::new(Expr::Path(ExprPath {
        attrs: vec![],
        qself: None,
        path: {
            let mut path = tr.clone();
            path.segments.push(PathSegment {
                ident: sig.ident.clone(),
                arguments: generic_params_to_args(&sig.generics),
            });
            path
        },
    }));
    move |this: &Ident| {
        let args = method_params_to_args(this, &mut inputs);
        let span = this.span();
        Box::new(Expr::MethodCall(ExprMethodCall {
            attrs: vec![],
            receiver: Box::new(Expr::Call(ExprCall {
                attrs: vec![],
                func: func.clone(),
                paren_token: Paren(span),
                args,
            })),
            dot_token: Token![.](span),
            method: Ident::new("into", span),
            turbofish: None,
            paren_token: Paren(span),
            args: Punctuated::new(),
        }))
    }
}

fn handle_delegate_calls(
    mut call: impl FnMut(&Ident) -> Box<Expr>,
    v: &mut Variant,
) -> Result<Arm> {
    if v.fields.is_empty() {
        return Err(Error::new_spanned(
            v,
            "denumic variant should have a field as the implementation",
        ));
    }
    let span = v.span();
    let member = v
        .fields
        .iter_mut()
        .enumerate()
        .find_map(|(i, f)| {
            f.attrs
                .iter()
                .enumerate()
                .find_map(|(j, a)| {
                    if let Ok(path) = a.meta.require_path_only() {
                        if path.is_ident("denumic") {
                            return Some(j);
                        }
                    }
                    None
                })
                .map(|j| {
                    f.attrs.remove(j);
                    f.ident
                        .clone()
                        .map(Member::Named)
                        .unwrap_or(Member::Unnamed(Index {
                            index: i as _,
                            span: f.span(),
                        }))
                })
        })
        .unwrap_or(Member::Unnamed(Index {
            index: 0,
            span: v.fields.iter().next().span(),
        }));
    let this = Ident::new("__this", member.span());
    let call = call(&this);

    Ok(Arm {
        attrs: merge_cfgs([&v.attrs]),
        pat: Pat::Struct(PatStruct {
            attrs: Vec::new(),
            qself: None,
            path: Path {
                leading_colon: None,
                segments: Punctuated::from_iter([
                    PathSegment::from(Token![Self](span)),
                    v.ident.clone().into(),
                ]),
            },
            brace_token: Brace(span),
            fields: Punctuated::from_iter([FieldPat {
                attrs: vec![],
                member,
                colon_token: Some(Token![:](span)),
                pat: Box::new(Pat::Ident(PatIdent {
                    attrs: vec![],
                    by_ref: None,
                    mutability: None,
                    ident: this,
                    subpat: None,
                })),
            }]),
            rest: Some(PatRest {
                attrs: Vec::new(),
                dot2_token: Token![..](span),
            }),
        }),
        guard: None,
        fat_arrow_token: Token![=>](span),
        body: call,
        comma: Some(Token![,](span)),
    })
}

fn generate_conv(ty_def: &ItemEnum, result: &mut TokenStream) {
    let mut generics = ty_def.generics.clone();
    remove_defaults(&mut generics);
    let where_clause = &generics.where_clause;
    let g_args = generic_params_to_args(&generics);
    let ty = &ty_def.ident;
    for v in &ty_def.variants {
        let mut fields = v.fields.iter();
        let (Some(field), None) = (fields.next(), fields.next()) else {
            continue;
        };
        let v_ty = &field.ty;
        let vid = &v.ident;
        if let Type::Path(TypePath { path, .. }) = v_ty {
            if generics.params.iter().any(|x| {
                if let GenericParam::Type(t) = x {
                    path.is_ident(&t.ident)
                } else {
                    false
                }
            }) {
                continue;
            }
        }
        result.extend(quote! {
            impl #generics From<#v_ty> for #ty #g_args #where_clause
            {
                #[inline(always)]
                fn from(v: #v_ty) -> Self {
                    Self::#vid(v)
                }
            }

            impl #generics TryFrom<#ty #g_args> for #v_ty #where_clause
            {
                type Error = #ty #g_args;

                #[inline(always)]
                fn try_from(v: #ty #g_args) -> Result<Self, Self::Error> {
                    if let #ty::#vid(v) = v {
                        Ok(v)
                    } else {
                        Err(v)
                    }
                }
            }
        });
    }
}
