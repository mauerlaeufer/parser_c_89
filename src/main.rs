use parser_c_89::grammar::TranslationUnitParser;
use parser_c_89::lexer::Lexer;
use std::env;
use std::fmt::Debug;
use std::fs;
use std::io;

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let use_pretty_print = args.contains(&String::from("--pretty-print"));
    let use_lex_print = args.contains(&String::from("--lex-print"));

    let source_code = fs::read_to_string("main.c")?;

    // Create a lexer for printing if you want to.
    if use_lex_print {
        let lexer = Lexer::new(&source_code);
        for lex in lexer {
            println!("Lexer: {:?}", lex.unwrap());
        }
    }

    // Re-create lexer for parsing
    let lexer = Lexer::new(&source_code);
    let parser = TranslationUnitParser::new();
    let ast = parser.parse(lexer).unwrap();

    // Determine which print method to use based on the argument
    if use_pretty_print {
        println!("{}", pretty_print(&ast, 0));
    } else {
        println!("{:?}", ast);
    }
    
    Ok(())
}

// The pretty-print function with indentation
fn pretty_print<T: Debug>(item: &T, level: usize) -> String {
    format!("{:indent$}{:#?}", "", item, indent=level)
}