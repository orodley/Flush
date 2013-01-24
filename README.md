# Flush #
Flush is an esoteric, stack-based programming language that works
by constructing function calls on the stack, and calling them
by "flushing" the stack.

## Literals ##
Literals are simply read, and pushed onto the stack, like so:

    1 2 3 4              => (1 2 3 4)
    .52 3.14 15/60       => (0.52 3.14 1/4)
    "foo" "bar"          => ("foo" "bar")
    (1 "foo" 3.14 15/60) => ((1 "foo" 3.14 1/4))

As you can see, integers, floats, rationals, strings, and
(heterogeneous) lists are supported types.

Functions operate in a identical way; when not explicitly called
they are simply pushed onto the stack:

    + - / * % => (+ - / * %)

## Operators ##
There are three operators, `;` for "big flush", `,` for
"little flush", and `@` for stack rotation.

`;` "flushes" the whole stack; calls the function at the bottom
of the stack with every stack element above it as arguments, like so:

    + 1 2 3; => (6)
    - 2 4;   => (-2)

`,` only calls the topmost function on the stack, with every stack
element above it as arguments, leaving the rest of the stack
unchanged, like so:

    + - / * 2 3,      => (+ - / 6)
    + * 2 4, / 10 5,; => (16)


`@` rotates the stack, moving the topmost element to the bottom, and
pushing every other element up, like so:

    1 2 3 4 @ => (4 1 2 3)
    1 2 + @   => (+ 1 2)
    1 2 + @;  => (3)

## Builtin functions ##
Only arithmetic operators are supported at the moment

Flush has been put on (probably indefinite) hold due to issues in
the design, but I thought the concept was interesting.
