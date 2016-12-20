# sext: lists, Texts, ByteStrings with statically encoded length

[![Travis CI build status](https://travis-ci.org/dzhus/sext.svg)](https://travis-ci.org/dzhus/sext)
[![Hackage](https://img.shields.io/hackage/v/sext.svg)](https://hackage.haskell.org/package/sext)
[![Hackage deps](https://img.shields.io/hackage-deps/v/sext.svg)](http://packdeps.haskellers.com/feed?needle=sext)

## Introduction

Sext (static text) provides type-level safety for basic operations on
string-like types (finite lists of elements). Use it when you need
static guarantee on lengths of strings produced in your code.

## Goals

We need a set of combinators to build strings/lists which support
static length-checking.

Sext is a list-like datatype with length encoded in its type. We'd
also like to support several of concrete representations for strings
like Text or ByteString so that an appropriate type may be used when a
problem demands it.

We also want to support native GHC type-level natural numbers
(provided by GHC.TypeLits) to encode length instead of any other
type-level number representation: now that a basic solver is available
for type-level naturals in GHC 7.8, the notation provided by them is
particularly expressive and the implementation does not rely on any TH
hacks.

The library must introduce as little run-time overhead as possible.
Thus we aim to implement a thin type-level wrapper over operations
already provided by representation types. Operations like take or drop
still require run-time access to type information (in form of Nat
singletons representing container size).

## Sextable class

Sextable class maps operations for concrete types over constructor.
Combinators are polymorphic and support any instance of Sextable
class:

- lists

- Text

- ByteString

Type safety is provided by means of an associated type Sext. Sextable
methods map basic type operations over this wrapper.

## String literals

We want to be able to include sext values in our programs in the form
of string literals, similar to what's possible for Text or ByteString
using OverloadedStrings extension.

We could easily implement a sext splice, used as follows:

    $(sext "Foobar")

it would expand to this:

    Sext "Foobar" :: Sext 6 a

where a is a type variable restricted to instances of both Sextable
and IsString. Actual literal length is encoded in the type of the
produced expression. We use untyped Template Haskell since types
cannot be spliced.

We could also use one of type-safe operations like
`createLeft`/`createRight` to make our Sexts instances of `IsString`.
A sane choice is to be picked. Probably in most of cases `(createLeft
' ')` would be expected.

## Constructing Sexts

We also provide `create` and `unsafeCreate` methods in Sextable class
to enable interfacing Sext with run-time IO sources. They do not
provide as much compile-time safety as the sext macro does. `create`
induces an extra run-time check while `unsafeCreate` simply applies
the constructor to any source value.

If we combine (untyped) create, take and padLeft we can define a
type-safe constructor for producing `Sext n` from source strings of
length m. If m > n, then we drop extra characters. If m < n, we pad
source up to target length. This needs to be done before wrapping the
obtained string in Sext constructor. `createLeft`/`createRight`
implement this approach.

## Type ambiguity

An expression like

    (take $ padLeft ' ' sext) :: Sext n t

won't typecheck. Both combinators have no knowledge about the exact
amount characters which need to be dropped/added to sext.

## Solver limitations

Currently GHC solver cannot deduce that `((n + m) - m) ~ n`, thus we
encode it in method signatures.
