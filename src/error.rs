/*
 *
 * Copyright (c) Jérémy Audiger.
 * All rights reserved.
 *
 */

use std::fmt::Display;
use std::fmt::Formatter;

use crate::lexer::LexerError;
use crate::parser::ParserError;

/// Global error type for the interpreter.
#[derive(Debug, PartialEq, Eq)]
pub enum InterpreterError {
    Lexer(LexerError),
    Parser(ParserError),
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Lexer(error) => error.fmt(f),
            Self::Parser(error) => error.fmt(f),
        }
    }
}

impl std::error::Error for InterpreterError {}

/// Easily convert a `LexerError` into an `InterpreterError`.
impl From<LexerError> for InterpreterError {
    fn from(error: LexerError) -> Self {
        Self::Lexer(error)
    }
}

/// Easily convert a `ParserError` into an `InterpreterError`.
impl From<ParserError> for InterpreterError {
    fn from(error: ParserError) -> Self {
        Self::Parser(error)
    }
}
