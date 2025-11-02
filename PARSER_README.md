# Parser Implementation

This is a recursive descent parser for Program 2, implementing the grammar specified in README.md.

## Files

- `parser.rkt` - Main parser implementation
- `test-parser.rkt` - Test file to run all test cases

## Usage

### In DrRacket

1. Open `parser.rkt` in DrRacket
2. Click "Run" to load the module
3. In the interactions pane, test individual files:
   ```racket
   (parse "Correct01.txt")
   ```

### Running All Tests

1. Open `test-parser.rkt` in DrRacket
2. Click "Run" to execute all tests

## How It Works

The parser consists of two main components:

### 1. Tokenizer/Lexer

- Removes comments (including multi-line comments)
- Tokenizes the input into a stream of tokens
- Handles context-sensitive signed numbers (distinguishes `B-A` from `B * -7`)
- Tracks line numbers for error reporting

### 2. Recursive Descent Parser

Implements each grammar rule as a function:

- `parse-program` - program → stmt-list EOF
- `parse-stmt-list` - stmt-list → stmt stmt-list | epsilon
- `parse-stmt` - Dispatches to specific statement parsers
- `parse-if-stmt` - if statement
- `parse-while-stmt` - while statement
- `parse-assign-stmt` - assignment statement
- `parse-read-stmt` - read statement
- `parse-print-stmt` - print statement
- `parse-expr` - expressions (handles +, -)
- `parse-term` - terms (handles *, /)
- `parse-factor` - factors (id, num, parenthesized expressions)
- `parse-comp-op` - comparison operators

### Key Features

1. **Line tracking**: Every token has a line number, enabling accurate error reporting
2. **Expression vs Statement line breaks**:
   - Expressions CANNOT cross line breaks (enforced with `check-no-newline!`)
   - Statements CAN cross line breaks (using `skip-newlines!`)
3. **Semicolon handling**: Semicolons separate multiple statements on the same physical line
4. **Comment handling**: Multi-line comments are supported; nested comment markers are treated as part of the comment content
5. **Signed numbers**: Context-aware tokenization correctly distinguishes binary operators from signed number literals

## Output

### Successful Parse
```
Accept
(program ...)
```

### Syntax Error
```
Syntax error on line XX
```

## Implementation Notes

- Does NOT use the brag parser generator
- Pure recursive descent implementation
- All grammar rules are implemented as separate functions
- Token stream includes newlines to enforce expression line-break rules
