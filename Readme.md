## Context

I chose [lalrpop](https://github.com/lalrpop/lalrpop), an LR(1) parser generator. I decided to use this one since it both explicitly uses an external lexer and constructs an LR(1) parser. I am familiar with Rust.

`lalrpop` offers a wide variety of great features like macros, abbreviations, Kleene star-like operators, etc. I didn't take advantage of most of these, as it was easier to follow the provided grammar without them. If one were inclined to optimize this project, they should probably start there. As in Lab 1, I use the external [logos](https://github.com/maciejhirsz/logos) lexer.

### Language Selection

I chose to parse the original C89 standard. This decision was mostly driven by curiosity about C, after quickly hacking together a simple while-language and realizing that it would be a doable challenge.

In the end, I learned a lot about C itself and a little bit about how smart people write grammars for programming languages. It was interesting to see how a C file is structured internally.

### Notes About the Process

Most of the actual work involved figuring out `lalrpop`, setting up basic functionality, and getting it to parse something. This worked surprisingly well, with a few caveats that were mostly related to recursive `enums`, which Rust doesn't allow. Introducing redirection via the `Box` concept solved this.

The error handling of `lalrpop` and type checking of Rust are amazing. I didn't need to actually parse examples while extending the grammar (my first iteration of a basic while-language was done step-by-step by adding new language features, testing by parsing an example, and debugging the grammar). Instead, the `rust-analyzer` (which checked for types) combined with the grammar analyzer included with `lalrpop` (which checked for grammar mistakes) allowed me to write the complete project without having to parse a single test example in between. I had a number of compile-time errors, but after fixing those, everything miraculously worked on the first run with the provided example.

I think this is a strong endorsement for Rust and the `lalrpop` crate.

### Notes About the Language and Grammar

While going through the C89 grammar, I found a number of interesting idiosyncrasies:

```c
declaration
    : declaration_specifiers ';'
    | declaration_specifiers init_declarator_list ';'
    ;

declaration_specifiers
    : storage_class_specifier
    | storage_class_specifier declaration_specifiers
    | type_specifier
    | type_specifier declaration_specifiers
    | type_qualifier
    | type_qualifier declaration_specifiers
    ;
```

This part of the grammar allows for:
`int int c = 10;`
Which is an error will be caught at compile time. (Hopefully)
This was introduced to allow fun data types like:
`signed long long int`
(Which is a valid C data Type!) without overcomplicating the grammar by encoding all possible combinations of these data types.


Furthermore C89 doesn't have booleans and allows for horrors like these:
```
   if (u = 6, "a") {
       e ++;
   }
```



Another interesting aspect is that declarations are only allowed either at top level via:
translation_unit -> external_declaration -> function_definition -> [...] compount_statement -> { declaration_list statement_list }
With declaration_list being a list of declarations (Not really surprising).

The same is possible at the top of a function:
function_definition -> [...] compound_statement -> ... -> { declaration_list statement_list }
but not once the code goes into further statements. If you want a declaration inside there, you'll have to put it into braces:
```

   int main () {
       a++;
       {
           int b = 10; // This is allowed.
       }
       int c = 5; // This isn't.
   }
```

you can also confirm this with clang/gcc and the -std=c89 flag.

## Grammar and Lexing
The lexer itself is configured inside /src/tokens.rs. The lexing was done on the basis of this (lex)[https://www.lysator.liu.se/c/ANSI-C-grammar-l.html] grammar.
The grammar is defined inside /src/grammar.lalrpop and corresponds to this (Yacc)[https://www.lysator.liu.se/c/ANSI-C-grammar-y.html] grammar. 

## Missing features/theoretical complexities
There are two major issues with the C89 grammar. Firstly the `typedef` functionality, which requires the lexer to use a symbol table or something similar, to lex these new types as keywords and not as identifiers. While this is probably possible with Logos, I chose not to implement this (Sanity and time constraints). (See)[https://calculist.blogspot.com/2009/02/c-typedef-parsing-problem.html] here for more information.
Furthermore the C89 doesn't seem to care about (dangling)[https://en.wikipedia.org/wiki/Dangling_else] else, which leads to a shift/reduce conflict. By naively implementing it lalrpop provided this helpful error:

  processing file `/home/hanno/Desktop/parser_c_89/src/grammar.lalrpop`
  /home/hanno/Desktop/parser_c_89/src/grammar.lalrpop:954:3: 955:54: Ambiguous grammar detected

    The following symbols can be reduced in two ways:
      "if" "(" Expression ")" "if" "(" Expression ")" Statement "else" Statement

    They could be reduced like so:
      "if" "(" Expression ")" "if" "(" Expression ")" Statement "else" Statement
      │                       ├─SelectionStatement────────────┤                │
      │                       └─Statement─────────────────────┘                │
      └─SelectionStatement─────────────────────────────────────────────────────┘

    Alternatively, they could be reduced like so:
      "if" "(" Expression ")" "if" "(" Expression ")" Statement "else" Statement
      │                       ├─SelectionStatement─────────────────────────────┤
      │                       └─Statement──────────────────────────────────────┤
      └─SelectionStatement─────────────────────────────────────────────────────┘

    LALRPOP does not yet support ambiguous grammars. See the LALRPOP manual for advice on
    making your grammar unambiguous.

I forced curly braces around the if/else conditionals by forcing a compound_statement instead of a statement, which solves this. In theory we could also follow the C89 standard (Which apparently suggests "that an else block is associated with the nearest if"), but this was an easy and reasonable solution since lalrpop doesn't allow ambiguous grammars.

=> The implemented grammar in this project is a unambiguous version of the (slightly altered) C89 specification.

## Errror Handling
Some error handling is provided by the parser-generator and the lexer. It is able to give hints regarding wrong syntax and tokens.

## Generated Grammar
lalrpop only builds LR(1) parsers, as a result the generated has a $k=1$.

### Running the code

This is a simple rust project. Install rust as described by the (documentation)[https://www.rust-lang.org/tools/install]. Move inside the folder parser_c_89. Execute `cargo run` to parse the `main.c` file which is included inside the folder. Adjust the source code to your liking and try again.
Note that the generated parser is located inside the target directory, either in
`calculator/target/debug/build/parser_c_89-*/out`
or 
`calculator/target/release/build/parser_c_89-*/out`

It is quite possible that there are a number of bugs here (This is quite complicated), but the basic example which I provided runs on my machine.
Furthermore, the compilation can take quite some time. It generates a grammar.rs file which is around 900_000 lines long.

Additionally use the following arguments: 
`cargo run -- --lex-print`
to print the result of the lexing and
`cargo run -- --pretty-print`
to try to print an actual tree (Probably won't fit on your screen).
