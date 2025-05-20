# CompailerConstruction
This project is part of the Compiler Construction course and follows the LLVM Kaleidoscope tutorial. However, instead of implementing the Kaleidoscope language, it adapts the tutorial to the F# programming language syntax.

In Chapter 1, a lexer was implemented to break input text into tokens—an essential first step in compiler construction.

In Chapter 2, a parser was created using recursive descent parsing and operator precedence rules to build an Abstract Syntax Tree (AST) that represents the logical structure of the code.

In Chapter 3, code generation was performed, translating the AST into LLVM Intermediate Representation (IR), which facilitates further optimization and compilation.

In Chapter 4, Just-In-Time (JIT) compilation and LLVM optimization passes were introduced, enabling immediate execution of compiled code.

In Chapter 5, control flow constructs such as if statements and for loops were added, along with an introduction to Static Single Assignment (SSA) form used by LLVM.

In Chapter 6, support for custom operators with user-defined precedence was added, enhancing the language's flexibility.

In Chapter 7, mutable variables and assignment capabilities were introduced, increasing the expressiveness of the language.

In Chapter 8, techniques were explored for compiling LLVM IR to object files, aligning the project with traditional compiler behavior.

In Chapter 9, debug information was incorporated, enabling the use of breakpoints, variable inspection, and other debugging tools.

In Chapter 10, additional language extensions were examined, including garbage collection, error handling, and other runtime features, with the goal of supporting a more complete and robust language design.

