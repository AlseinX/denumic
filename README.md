
# denumic

`denumic` macro is to create enum-based runtime dispatched traits.

When applied to a `trait`, it exports the definition of the trait that would be passed to other `denumic` attributes on `enum`s.

When applied to an `enum`, it generates a macro that can be used to dispatch to the appropriate implementation of the trait for each variant of the `enum`.

`From` and `TryFrom` are also automatically implemented for the `enum`, as long as it does not break coherence rules, so that it can be converted to and from the variants.

# Example

```rust
use denumic::denumic;

// Generic parameters of the trait and the enum could be finly merged matching by names.
#[denumic]
pub trait Trait {
    const MY_CONST: i32;
    type MyType;
    fn foo(self) -> i32;
}

// Note that the identifiers `used` before the trait definition also have to be `use`d before the enum definition.
#[denumic(Trait)]
// Associated types and consts can be specified using attributes:
#[MY_CONST = 1]
// As for types that are not a valid `expr`, quote them instead of raw types:
#[MyType = "&'static str"]
pub enum Impl<Other = ()>
where
    Other: Trait,
{
    // `From` and `TryFrom` are only generated if the enum has only a single field.
    A(A),
    // If multiple fields are found, the one with `#[denumic]` otherwise the first one is dispatched.
    Other(Other, String),
}

pub struct A;

impl Trait for A {
    const MY_CONST: i32 = 2;

    type MyType = ();

    fn foo(self) -> i32 {
        10
    }
}

impl Trait for () {
    const MY_CONST: i32 = 0;

    type MyType = String;

    fn foo(self) -> i32 {
        20
    }
}

fn call_foo(v: A) -> i32 {
    let v: Impl = v.into();
    v.foo()
}

```
