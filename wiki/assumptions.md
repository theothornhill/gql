# Assumptions

There are some assumptions I have had to make during the development, and when
trying to understand how things fit together, to avoid confusion, this document
is provided.

## Casing

In general we follow lisp style casing.  That entails using kebab-case rather
than PascalCase or camelCase used in the spec.  As often as possible, we copy
the name of a function or variable verbatim, but converting to kebab.  To be
super specific, that means:

- `SelectionSet` -> `selection-set`
- `objectType` -> `object-type`

As a general rule, when naming differs from the spec outside of this reason,
that qualifies as a bug, unless it is super clear what is happening in the
translation from spec to lisp.

## Linking to the spec

It is a good rule to link to the definition of a function inside the body.  We
do that where available, and this also makes it a little clearer what is
directly from the spec, and what are utilities and helpers.  If no link to a
spec is provided, that is also considered a bug.

## Classes also acts as interfaces

GraphQL uses interfaces to abstract away some information.  CLOS doesn't really
have interfaces the way we see in java and c#, so we will just by convention try
to create classes that look like interfaces, then use inheritance to gain the
properties.  Use this mindfully.

## `type-name` as a property

To simplify with inferencing of interfaces to classes we add our own object,
`gql-object` to use as a parent class when defining types to resolve.  This is
very naive at the moment, and we really should learn clos to see how we can deal
with this better.
