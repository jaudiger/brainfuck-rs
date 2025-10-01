/*
 *
 * Copyright (c) Jérémy Audiger.
 * All rights reserved.
 *
 */

use std::error::Error;
use std::fmt::Display;
use std::fmt::Formatter;
use std::io::Read;
use std::io::Write;

use crate::error::InterpreterError;
use crate::lexer::Lexer;
use crate::lexer::LexerToken;
use crate::lexer::LexerTokenMode;

/// The minimum memory address.
const MEMORY_ADDRESS_MIN: usize = 0;
/// The default maximum memory address.
const MEMORY_ADDRESS_DEFAULT_MAX: usize = 30000;

/// The minimum memory value.
const MEMORY_VALUE_MIN: u8 = 0;
/// The maximum memory value.
const MEMORY_VALUE_MAX: u8 = 255;

/// Specific error type for the parser.
#[derive(Debug, PartialEq, Eq)]
pub enum ParserError {
    InputReadingFailure,
    OutputWritingFailure,
    MemoryAddressHighLimit,
    MemoryAddressLowLimit,
    MemoryValueHighLimit,
    MemoryValueLowLimit,
    ProgramFinished,
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::InputReadingFailure => write!(f, "Failed to read input"),
            Self::OutputWritingFailure => write!(f, "Failed to write output"),
            Self::MemoryAddressHighLimit => write!(f, "Out of bounds high memory address"),
            Self::MemoryAddressLowLimit => write!(f, "Out of bounds low memory address"),
            Self::MemoryValueHighLimit => write!(f, "Out of bounds high memory value"),
            Self::MemoryValueLowLimit => write!(f, "Out of bounds low memory value"),
            Self::ProgramFinished => write!(f, "Program finished"),
        }
    }
}

impl Error for ParserError {}

/// The boundness mode of the parser. It determines how the parser handles memory address and value out of bounds.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub enum ParserBoundnessMode {
    /// Return an error when the memory address or value is out of bounds.
    #[default]
    Strict,

    /// Wrap the memory address or value when it is out of bounds.
    Wrap,
}

/// Structure holding the state of the parser.
#[derive(Debug)]
pub struct Parser<const LENGTH: usize = MEMORY_ADDRESS_DEFAULT_MAX> {
    lexer: Lexer,
    program: Vec<LexerToken>,
    program_counter: usize,
    memory: [u8; LENGTH],
    memory_address: usize,
    boundness_mode: ParserBoundnessMode,
}

impl<const LENGTH: usize> Default for Parser<LENGTH> {
    fn default() -> Self {
        Self {
            lexer: Lexer::default(),
            program: Vec::new(),
            program_counter: 0,
            memory: [0; LENGTH],
            memory_address: 0,
            boundness_mode: ParserBoundnessMode::default(),
        }
    }
}

impl<const LENGTH: usize> Parser<LENGTH> {
    /// Creates a new parser instance.
    #[must_use]
    pub const fn new(token_mode: LexerTokenMode, boundness_mode: ParserBoundnessMode) -> Self {
        Self {
            lexer: Lexer::new(token_mode),
            program: Vec::new(),
            program_counter: 0,
            memory: [0; LENGTH],
            memory_address: 0,
            boundness_mode,
        }
    }

    /// Loads a program into the parser.
    pub fn load_program(
        &mut self,
        instructions: impl Into<String>,
    ) -> Result<(), InterpreterError> {
        self.program = self.lexer.parse(instructions)?;
        self.reset();

        Ok(())
    }

    /// Run one step of the program.
    pub fn step<R, W>(&mut self, input: &mut R, output: &mut W) -> Result<(), InterpreterError>
    where
        R: Read,
        W: Write,
    {
        if self.program_counter >= self.program.len() {
            return Err(InterpreterError::Parser(ParserError::ProgramFinished));
        }

        match self.program[self.program_counter] {
            LexerToken::IncrementPointer => self.increment_pointer()?,
            LexerToken::DecrementPointer => self.decrement_pointer()?,
            LexerToken::IncrementByte => self.increment_byte()?,
            LexerToken::DecrementByte => self.decrement_byte()?,
            LexerToken::OutputByte => self.output_byte(output)?,
            LexerToken::InputByte => self.input_byte(input)?,
            LexerToken::LoopStart(index) => self.loop_start(index),
            LexerToken::LoopEnd(index) => self.loop_end(index),
        }

        self.program_counter += 1;

        Ok(())
    }

    /// Runs the program until completion.
    pub fn run<R, W>(&mut self, input: &mut R, output: &mut W) -> Result<(), InterpreterError>
    where
        R: Read,
        W: Write,
    {
        while self.program_counter < self.program.len() {
            self.step(input, output)?;
        }

        Ok(())
    }

    /// Resets the interpreter state. Only resets the program counter and memory.
    /// It does not reset the program already loaded.
    pub fn reset(&mut self) {
        self.program_counter = 0;
        self.memory_address = 0;
        // Fill memory with zero values
        self.memory.fill(0);
    }

    /// Returns the current program counter.
    #[must_use]
    pub const fn program_counter(&self) -> usize {
        self.program_counter
    }

    /// Returns the program instruction at the given program counter.
    #[must_use]
    pub fn program_instruction(&self, pc: usize) -> Option<&LexerToken> {
        self.program.get(pc)
    }

    /// Returns the current memory address.
    #[must_use]
    pub const fn memory_address(&self) -> usize {
        self.memory_address
    }

    /// Returns the memory value at the given memory address.
    #[must_use]
    pub fn memory_value(&self, address: usize) -> Option<&u8> {
        self.memory.get(address)
    }

    /// Returns the current boundness mode of the parser.
    #[must_use]
    pub const fn boundness_mode(&self) -> ParserBoundnessMode {
        self.boundness_mode
    }

    const fn increment_pointer(&mut self) -> Result<(), ParserError> {
        if self.memory_address == LENGTH - 1 {
            match self.boundness_mode {
                ParserBoundnessMode::Strict => {
                    return Err(ParserError::MemoryAddressHighLimit);
                }
                ParserBoundnessMode::Wrap => self.memory_address = 0,
            }
        } else {
            self.memory_address += 1;
        }

        Ok(())
    }

    const fn decrement_pointer(&mut self) -> Result<(), ParserError> {
        if self.memory_address == MEMORY_ADDRESS_MIN {
            match self.boundness_mode {
                ParserBoundnessMode::Strict => {
                    return Err(ParserError::MemoryAddressLowLimit);
                }
                ParserBoundnessMode::Wrap => self.memory_address = LENGTH - 1,
            }
        } else {
            self.memory_address -= 1;
        }

        Ok(())
    }

    const fn increment_byte(&mut self) -> Result<(), ParserError> {
        if self.memory[self.memory_address] == MEMORY_VALUE_MAX {
            match self.boundness_mode {
                ParserBoundnessMode::Strict => {
                    return Err(ParserError::MemoryValueHighLimit);
                }
                ParserBoundnessMode::Wrap => self.memory[self.memory_address] = 0,
            }
        } else {
            self.memory[self.memory_address] += 1;
        }

        Ok(())
    }

    const fn decrement_byte(&mut self) -> Result<(), ParserError> {
        if self.memory[self.memory_address] == MEMORY_VALUE_MIN {
            match self.boundness_mode {
                ParserBoundnessMode::Strict => return Err(ParserError::MemoryValueLowLimit),
                ParserBoundnessMode::Wrap => self.memory[self.memory_address] = MEMORY_VALUE_MAX,
            }
        } else {
            self.memory[self.memory_address] -= 1;
        }

        Ok(())
    }

    fn output_byte<W>(&self, output: &mut W) -> Result<(), ParserError>
    where
        W: Write,
    {
        output
            .write_all(&[self.memory[self.memory_address]])
            .map_err(|_| ParserError::OutputWritingFailure)?;

        Ok(())
    }

    fn input_byte<R>(&mut self, input: &mut R) -> Result<(), ParserError>
    where
        R: Read,
    {
        let mut read_input: [u8; 1] = Default::default();
        input
            .read_exact(&mut read_input)
            .map_err(|_| ParserError::InputReadingFailure)?;

        self.memory[self.memory_address] = read_input[0];

        Ok(())
    }

    const fn loop_start(&mut self, end_loop_index: usize) {
        if self.memory[self.memory_address] == 0 {
            self.program_counter = end_loop_index;
        }
    }

    const fn loop_end(&mut self, start_loop_index: usize) {
        if self.memory[self.memory_address] != 0 {
            self.program_counter = start_loop_index;
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use crate::error::InterpreterError;
    use crate::lexer::LexerTokenMode;
    use crate::parser::MEMORY_ADDRESS_DEFAULT_MAX;
    use crate::parser::Parser;
    use crate::parser::ParserBoundnessMode;
    use crate::parser::ParserError;

    #[test]
    fn test_parsing_hello_world_program() {
        let mut input_buffer = Cursor::new(Vec::new());
        let mut output_buffer = Vec::new();

        let brainfuck_code = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
        let mut parser: Parser = Parser::new(LexerTokenMode::Strict, ParserBoundnessMode::Strict);

        let load_result = parser.load_program(brainfuck_code);
        assert!(load_result.is_ok());

        let run_result = parser.run(&mut input_buffer, &mut output_buffer);
        assert!(run_result.is_ok());

        assert_eq!(output_buffer.as_slice(), b"Hello World!\n");
    }

    #[test]
    fn test_parsing_strict_mode_with_memory_address_bound() {
        let mut input_buffer = Cursor::new(Vec::new());
        let mut output_buffer = Vec::new();

        let brainfuck_code = "<";
        let mut parser: Parser = Parser::new(LexerTokenMode::Strict, ParserBoundnessMode::Strict);

        let load_result = parser.load_program(brainfuck_code);
        assert!(load_result.is_ok());

        let step_result = parser.step(&mut input_buffer, &mut output_buffer);
        assert!(step_result.is_err());

        let err = step_result.unwrap_err();
        assert_eq!(
            err,
            InterpreterError::Parser(ParserError::MemoryAddressLowLimit)
        );
    }

    #[test]
    fn test_parsing_wrap_mode_with_memory_address_bound() {
        let mut input_buffer = Cursor::new(Vec::new());
        let mut output_buffer = Vec::new();

        let brainfuck_code = "<";
        let mut parser: Parser = Parser::new(LexerTokenMode::Strict, ParserBoundnessMode::Wrap);

        let load_result = parser.load_program(brainfuck_code);
        assert!(load_result.is_ok());

        let step_result = parser.step(&mut input_buffer, &mut output_buffer);
        assert!(step_result.is_ok());

        let memory_address = parser.memory_address();
        assert_eq!(MEMORY_ADDRESS_DEFAULT_MAX - 1, memory_address);
    }

    #[test]
    fn test_parsing_strict_mode_with_memory_value_bound() {
        let mut input_buffer = Cursor::new(Vec::new());
        let mut output_buffer = Vec::new();

        let brainfuck_code = "-";
        let mut parser: Parser = Parser::new(LexerTokenMode::Strict, ParserBoundnessMode::Strict);

        let load_result = parser.load_program(brainfuck_code);
        assert!(load_result.is_ok());

        let step_result = parser.step(&mut input_buffer, &mut output_buffer);
        assert!(step_result.is_err());

        let err = step_result.unwrap_err();
        assert_eq!(
            err,
            InterpreterError::Parser(ParserError::MemoryValueLowLimit)
        );
    }

    #[test]
    fn test_parsing_wrap_mode_with_memory_value_bound() {
        let mut input_buffer = Cursor::new(Vec::new());
        let mut output_buffer = Vec::new();

        let brainfuck_code = "-";
        let mut parser: Parser = Parser::new(LexerTokenMode::Strict, ParserBoundnessMode::Wrap);

        let load_result = parser.load_program(brainfuck_code);
        assert!(load_result.is_ok());

        let step_result = parser.step(&mut input_buffer, &mut output_buffer);
        assert!(step_result.is_ok());

        let memory_value = parser.memory_value(parser.memory_address);
        assert!(memory_value.is_some());
        assert_eq!(memory_value.unwrap(), &255);
    }
}
