/*
 *
 * Copyright (c) Jérémy Audiger.
 * All rights reserved.
 *
 */

use std::error::Error;
use std::fmt::Display;
use std::fmt::Formatter;

/// String representation of a token in the Brainfuck language.
const INCREMENT_POINTER: char = '>';
const DECREMENT_POINTER: char = '<';
const INCREMENT_BYTE: char = '+';
const DECREMENT_BYTE: char = '-';
const OUTPUT_BYTE: char = '.';
const INPUT_BYTE: char = ',';
const LOOP_START: char = '[';
const LOOP_END: char = ']';

/// Specific error type for the lexer.
#[derive(Debug, PartialEq, Eq)]
pub enum LexerError {
    UnexpectedToken(char),
    NoLoopStartForLoopEnd,
    UnexpectedLoopStartTokenForLoopEnd,
    UnmatchedLoop,
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken(c) => write!(f, "Unexpected token: {c}"),
            Self::NoLoopStartForLoopEnd => write!(f, "No loop start for loop end"),
            Self::UnexpectedLoopStartTokenForLoopEnd => {
                write!(f, "Unexpected loop start token for loop end")
            }
            Self::UnmatchedLoop => write!(f, "Unmatched loop"),
        }
    }
}

impl Error for LexerError {}

/// Store the boundary position of a loop:
/// - If it's start token, it's the index of the corresponding end token
/// - If it's end token, it's the index of the corresponding start token
pub type LoopMetadata = usize;

/// Representation of a token in the Brainfuck language.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LexerToken {
    IncrementPointer,
    DecrementPointer,
    IncrementByte,
    DecrementByte,
    OutputByte,
    InputByte,
    LoopStart(LoopMetadata),
    LoopEnd(LoopMetadata),
}

/// Structure holding the state of the lexer.
#[derive(Debug)]
pub(crate) struct Lexer {
    instructions: String,
}

impl Lexer {
    /// Create a new lexer instance.
    pub fn new(instructions: impl Into<String>) -> Self {
        Self {
            instructions: instructions.into(),
        }
    }

    /// Parse the instructions into a vector of tokens.
    pub fn parse(&self) -> Result<Vec<LexerToken>, LexerError> {
        let mut program = Vec::with_capacity(self.instructions.len());
        let mut loop_starts = Vec::new();

        // First, loop on each character in the instructions
        for (index, instruction) in self.instructions.chars().enumerate() {
            let character = match instruction {
                INCREMENT_POINTER => LexerToken::IncrementPointer,
                DECREMENT_POINTER => LexerToken::DecrementPointer,
                INCREMENT_BYTE => LexerToken::IncrementByte,
                DECREMENT_BYTE => LexerToken::DecrementByte,
                OUTPUT_BYTE => LexerToken::OutputByte,
                INPUT_BYTE => LexerToken::InputByte,
                LOOP_START => {
                    // Store the index of each loop start token
                    loop_starts.push(LexerToken::LoopStart(index));

                    LexerToken::LoopStart(Default::default())
                }
                LOOP_END => {
                    if let LexerToken::LoopStart(start_index) =
                        loop_starts.pop().ok_or(LexerError::NoLoopStartForLoopEnd)?
                    {
                        // Also update loop start token to set the end loop index
                        // Overwriting the loop start token is safe here
                        program[start_index] = LexerToken::LoopStart(index);

                        LexerToken::LoopEnd(start_index)
                    } else {
                        return Err(LexerError::UnexpectedLoopStartTokenForLoopEnd);
                    }
                }
                _ => {
                    return Err(LexerError::UnexpectedToken(instruction));
                }
            };

            program.push(character);
        }

        if !loop_starts.is_empty() {
            return Err(LexerError::UnmatchedLoop);
        }

        Ok(program)
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::lexer::LexerError;
    use crate::lexer::LexerToken;

    #[test]
    fn test_lexer_with_simple_program() {
        let brainfuck_code = "++[--]>";
        let lexer = Lexer::new(brainfuck_code);

        let lexer_result = lexer.parse();
        assert!(lexer_result.is_ok());

        let tokens = lexer_result.unwrap();
        assert_eq!(
            tokens,
            vec![
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::LoopStart(5),
                LexerToken::DecrementByte,
                LexerToken::DecrementByte,
                LexerToken::LoopEnd(2),
                LexerToken::IncrementPointer,
            ]
        );
    }

    #[expect(clippy::too_many_lines)]
    #[test]
    fn test_lexer_with_hello_world_program() {
        let brainfuck_code = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
        let lexer = Lexer::new(brainfuck_code);

        let lexer_result = lexer.parse();
        assert!(lexer_result.is_ok());

        let tokens = lexer_result.unwrap();
        assert_eq!(
            tokens,
            vec![
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::LoopStart(48),
                LexerToken::IncrementPointer,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::LoopStart(33),
                LexerToken::IncrementPointer,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementPointer,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementPointer,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementPointer,
                LexerToken::IncrementByte,
                LexerToken::DecrementPointer,
                LexerToken::DecrementPointer,
                LexerToken::DecrementPointer,
                LexerToken::DecrementPointer,
                LexerToken::DecrementByte,
                LexerToken::LoopEnd(14),
                LexerToken::IncrementPointer,
                LexerToken::IncrementByte,
                LexerToken::IncrementPointer,
                LexerToken::IncrementByte,
                LexerToken::IncrementPointer,
                LexerToken::DecrementByte,
                LexerToken::IncrementPointer,
                LexerToken::IncrementPointer,
                LexerToken::IncrementByte,
                LexerToken::LoopStart(45),
                LexerToken::DecrementPointer,
                LexerToken::LoopEnd(43),
                LexerToken::DecrementPointer,
                LexerToken::DecrementByte,
                LexerToken::LoopEnd(8),
                LexerToken::IncrementPointer,
                LexerToken::IncrementPointer,
                LexerToken::OutputByte,
                LexerToken::IncrementPointer,
                LexerToken::DecrementByte,
                LexerToken::DecrementByte,
                LexerToken::DecrementByte,
                LexerToken::OutputByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::OutputByte,
                LexerToken::OutputByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::OutputByte,
                LexerToken::IncrementPointer,
                LexerToken::IncrementPointer,
                LexerToken::OutputByte,
                LexerToken::DecrementPointer,
                LexerToken::DecrementByte,
                LexerToken::OutputByte,
                LexerToken::DecrementPointer,
                LexerToken::OutputByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::OutputByte,
                LexerToken::DecrementByte,
                LexerToken::DecrementByte,
                LexerToken::DecrementByte,
                LexerToken::DecrementByte,
                LexerToken::DecrementByte,
                LexerToken::DecrementByte,
                LexerToken::OutputByte,
                LexerToken::DecrementByte,
                LexerToken::DecrementByte,
                LexerToken::DecrementByte,
                LexerToken::DecrementByte,
                LexerToken::DecrementByte,
                LexerToken::DecrementByte,
                LexerToken::DecrementByte,
                LexerToken::DecrementByte,
                LexerToken::OutputByte,
                LexerToken::IncrementPointer,
                LexerToken::IncrementPointer,
                LexerToken::IncrementByte,
                LexerToken::OutputByte,
                LexerToken::IncrementPointer,
                LexerToken::IncrementByte,
                LexerToken::IncrementByte,
                LexerToken::OutputByte
            ]
        );
    }

    #[test]
    fn test_lexer_parsing_with_invalid_character() {
        let brainfuck_code = "a";
        let lexer = Lexer::new(brainfuck_code);

        let lexer_result = lexer.parse();
        assert!(lexer_result.is_err());

        let err = lexer_result.unwrap_err();
        assert_eq!(err, LexerError::UnexpectedToken('a'));
    }

    #[test]
    fn test_lexer_parsing_with_all_valid_characters() {
        let brainfuck_code = "><+-.,[]";
        let lexer = Lexer::new(brainfuck_code);

        let lexer_result = lexer.parse();
        assert!(lexer_result.is_ok());

        let tokens = lexer_result.unwrap();
        assert_eq!(
            tokens,
            vec![
                LexerToken::IncrementPointer,
                LexerToken::DecrementPointer,
                LexerToken::IncrementByte,
                LexerToken::DecrementByte,
                LexerToken::OutputByte,
                LexerToken::InputByte,
                LexerToken::LoopStart(7),
                LexerToken::LoopEnd(6),
            ]
        );
    }

    #[test]
    fn test_lexer_parsing_with_unmatched_brackets() {
        let brainfuck_code = "[[]";
        let lexer = Lexer::new(brainfuck_code);

        let lexer_result = lexer.parse();
        assert!(lexer_result.is_err());

        let err = lexer_result.unwrap_err();
        assert_eq!(err, LexerError::UnmatchedLoop);
    }

    #[test]
    fn test_lexer_parsing_with_complex_brackets() {
        let brainfuck_code = "+[[-][+[-[+[<>]-]+]-][+]]-";
        let lexer = Lexer::new(brainfuck_code);

        let lexer_result = lexer.parse();
        assert!(lexer_result.is_ok());

        let tokens = lexer_result.unwrap();
        assert_eq!(
            tokens,
            vec![
                LexerToken::IncrementByte,
                LexerToken::LoopStart(24),
                LexerToken::LoopStart(4),
                LexerToken::DecrementByte,
                LexerToken::LoopEnd(2),
                LexerToken::LoopStart(20),
                LexerToken::IncrementByte,
                LexerToken::LoopStart(18),
                LexerToken::DecrementByte,
                LexerToken::LoopStart(16),
                LexerToken::IncrementByte,
                LexerToken::LoopStart(14),
                LexerToken::DecrementPointer,
                LexerToken::IncrementPointer,
                LexerToken::LoopEnd(11),
                LexerToken::DecrementByte,
                LexerToken::LoopEnd(9),
                LexerToken::IncrementByte,
                LexerToken::LoopEnd(7),
                LexerToken::DecrementByte,
                LexerToken::LoopEnd(5),
                LexerToken::LoopStart(23),
                LexerToken::IncrementByte,
                LexerToken::LoopEnd(21),
                LexerToken::LoopEnd(1),
                LexerToken::DecrementByte,
            ]
        );
    }
}
