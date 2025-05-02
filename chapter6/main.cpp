#include "../include/KaleidoscopeJIT.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;
using namespace llvm::orc;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token
{
    TOK_EOF = -1,

    // commands
    TOK_LET = -2,
    TOK_LET_VAR = -3,
    TOK_OPEN = -4,
    TOK_LIST = -5,

    // primary
    TOK_IDENT = -6,
    TOK_INT = -7,
    TOK_FLOAT = -8,
    TOK_ARG_TYPE = -9,

    // control
    TOK_IF = -10,
    TOK_THEN = -11,
    TOK_ELSE = -12,
    TOK_FOR = -13,
    TOK_TO = -14,
    TOK_DO = -15,
    TOK_IN = -16,

    
    TOK_UNARY = -17,
    TOK_BINARY = -18,
    
};

static std::string IdentifierStr; // Filled in if TOK_IDENT
static int intVal;
static double dblVal;

/// gettok - Return the next token from standard input.
static int gettok()
{
    static int LastChar = ' ';

    // Skip any whitespace.
    while (isspace(LastChar))
        LastChar = getchar();

     // list: [1;2;3]
     if (LastChar == '[')
     {
         std::string listStr;
         LastChar = getchar();
         while(LastChar != ']' && LastChar != EOF)
         {
             listStr += LastChar;
 
             LastChar = getchar();
         }
       if(LastChar == ']')
       LastChar = getchar();
       return TOK_LIST;
     }
 

    if (isalpha(LastChar))
    { // identifier: [a-zA-Z][a-zA-Z0-9]*
        IdentifierStr = LastChar;
        while (isalnum((LastChar = getchar())))
            IdentifierStr += LastChar;

        if (IdentifierStr == "let")
            return TOK_LET;
        if (IdentifierStr == "open")
            return TOK_OPEN;
        if (IdentifierStr == "Let")
            return TOK_LET_VAR;
        if (IdentifierStr == "Int")
            return TOK_ARG_TYPE;
        if (IdentifierStr == "Double")
            return TOK_ARG_TYPE;
        if (IdentifierStr == "if")
            return TOK_IF;
        if (IdentifierStr == "then")
            return TOK_THEN;
        if (IdentifierStr == "else")
            return TOK_ELSE;
        if (IdentifierStr == "for")
            return TOK_TO;
        if (IdentifierStr == "to")
            return TOK_DO;
        if (IdentifierStr == "do")
            return TOK_IN;
        if (IdentifierStr == "in")
            return TOK_IN;
        if (IdentifierStr == "unary")
            return TOK_UNARY;
        if (IdentifierStr == "binary")
            return TOK_BINARY;

        return TOK_IDENT;
    }

    // Integer: [0-9]+ or Float: [0-9]+.[0-9]+
    if (isdigit(LastChar) || (LastChar == '.'))
    {
        std::string numStr;
        bool isFloat = false;

        do
        {
            numStr += LastChar;

            if (LastChar == '.')
            {
                if (isFloat)
                {
                    break;
                }

                isFloat = true;
            }

            LastChar = getchar();
        } while (isdigit(LastChar) || (!isFloat && LastChar == '.'));

        if (isFloat)
        {
            dblVal = strtod(numStr.c_str(), nullptr);
            return TOK_FLOAT;
        }
        else
        {
            intVal = strtol(numStr.c_str(), nullptr, 10);
            return TOK_INT;
        }
    }

    // Comments : //
      if (LastChar == '/' && ((LastChar = getchar()) == '/'))
      {
          // Comment until end of line.
          // single-line comment
          do
              LastChar = getchar();
          while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');
  
          if (LastChar != EOF)
              return gettok();
      }
      //Comments: """ """
      else if (LastChar == '"' && ((LastChar = getchar()) == '"') && ((LastChar = getchar() == '"')))
      {
          // Comment until the closing '"""'.
          // multi-line comment
          do
              LastChar = getchar();
          while (LastChar != EOF && LastChar != '"' && LastChar != '"' && LastChar != '"');
  
          if (LastChar != EOF)
              return gettok();
      }
  
    // Check for end of file.  Don't eat the EOF.
    if (LastChar == EOF)
        return TOK_EOF;

    // Otherwise, just return the character as its ascii value.
    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

namespace
{

    /// ExprAST - Base class for all expression nodes.
    class ExprAST
    {
    public:
        virtual ~ExprAST() = default;
        virtual Value *codegen() = 0;
    };

    // IntegerExprAST - Expression class for integer literals like "1".
    class IntegerExprAST : public ExprAST
    {
        int Val;

    public:
        IntegerExprAST(int Val) : Val(Val) {}
        Value *codegen() override;
    };

    // DoubleExprAST - Expression class for float literals like "1.0".
    class DoubleExprAST : public ExprAST
    {
        double Val;

    public:
        DoubleExprAST(double Val) : Val(Val) {}
        Value *codegen() override;
    };

    
    class VariableExprAST : public ExprAST
    {
        std::string Name;

    public:
        VariableExprAST(const std::string &Name) : Name(Name) {}
        Value *codegen() override;
        const std::string &getName() const { return Name; }
    };

    /// UnaryExprAST - Expression class for a unary operator.
    class UnaryExprAST : public ExprAST
    {
        char Opcode;
        std::unique_ptr<ExprAST> Operand;

    public:
        UnaryExprAST(char Opcode, std::unique_ptr<ExprAST> Operand)
            : Opcode(Opcode), Operand(std::move(Operand)) {}

        Value *codegen() override;
    };

    /// BinaryExprAST - Expression class for a binary operator.
    class BinaryExprAST : public ExprAST
    {
        char Op;
        std::unique_ptr<ExprAST> LHS, RHS;

    public:
        BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
                      std::unique_ptr<ExprAST> RHS)
            : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

        Value *codegen() override;
    };

    /// CallExprAST - Expression class for function calls.
    class CallExprAST : public ExprAST
    {
        std::string Callee;
        std::vector<std::unique_ptr<ExprAST>> Args;

    public:
        CallExprAST(const std::string &Callee,
                    std::vector<std::unique_ptr<ExprAST>> Args)
            : Callee(Callee), Args(std::move(Args)) {}
        Value *codegen() override;
    };

    /// IfExprAST - Expression class for if/else.
    class IfExprAST : public ExprAST
    {
        std::unique_ptr<ExprAST> Cond;
        std::vector<std::unique_ptr<ExprAST>> IfBlock;
        std::vector<std::unique_ptr<ExprAST>> ElseBlock;

    public:
        IfExprAST(std::unique_ptr<ExprAST> Cond, std::vector<std::unique_ptr<ExprAST>> IfBlock,
                  std::vector<std::unique_ptr<ExprAST>> ElseBlock)
            : Cond(std::move(Cond)), IfBlock(std::move(IfBlock)), ElseBlock(std::move(ElseBlock)) {}

        Value *codegen() override;
    };

    /// ForExprAST - Expression class for for.
    class ForExprAST : public ExprAST
    {
        std::string VarName;
        std::unique_ptr<ExprAST> Start, End, Step;
        std::vector<std::unique_ptr<ExprAST>> Body;

    public:
        ForExprAST(const std::string &VarName,
                   std::unique_ptr<ExprAST> Start,
                   std::unique_ptr<ExprAST> End,
                   std::unique_ptr<ExprAST> Step,
                   std::vector<std::unique_ptr<ExprAST>> Body)
            : VarName(VarName), Start(std::move(Start)), End(std::move(End)),
              Step(std::move(Step)), Body(std::move(Body)) {}

        Value *codegen() override;
    };
    /// PrototypeAST - This class represents the "prototype" for a function,
    /// which captures its name, and its argument names (thus implicitly the number
    /// of arguments the function takes).
    class PrototypeAST
    {
        std::string Name;
        std::vector<std::string> Args; // Argument name and type pairs.
        bool isOperator;
        unsigned Precedence; // Precedence if a binary op.

    public:
        PrototypeAST(const std::string &name, std::vector<std::string> args,
                     bool isOperator = false, unsigned Prec = 0)
            : Name(name), Args(std::move(args)), isOperator(isOperator), Precedence(Prec) {}

        Function *codegen();
        const std::string &getName() const { return Name; }

        bool isUnaryOp() const { return isOperator && Args.size() == 1; }
        bool isBinaryOp() const { return isOperator && Args.size() == 2; }

        char getOperatorName() const
        {
            assert(isUnaryOp() || isBinaryOp());
            return Name[Name.size() - 1];
        }

        unsigned getBinaryPrecedence() const { return Precedence; }
    };
   

    /// FunctionAST - This class represents a function definition itself.
    class FunctionAST
    {
        std::unique_ptr<PrototypeAST> Proto;
        std::unique_ptr<ExprAST> Body;

    public:
        FunctionAST(std::unique_ptr<PrototypeAST> Proto,
                    std::unique_ptr<ExprAST> Body)
            : Proto(std::move(Proto)), Body(std::move(Body)) {}
        Function *codegen();
    };

} // end anonymous namespace

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static int CurTok;
static int getNextToken() { return CurTok = gettok(); }

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<char, int> BinopPrecedence;

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence()
{
    if (!isascii(CurTok))
        return -1;

    // Make sure it's a declared binop.
    int TokPrec = BinopPrecedence[CurTok];
    if (TokPrec <= 0)
        return -1;
    return TokPrec;
}

/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char *Str)
{
    fprintf(stderr, "Error: %s\n", Str);
    return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str)
{
    LogError(Str);
    return nullptr;
}

static std::unique_ptr<ExprAST> ParseExpression();

// integer
static std::unique_ptr<ExprAST> ParseIntegerExpr()
{
    auto Result = std::make_unique<IntegerExprAST>(intVal);
    getNextToken(); // consume the number
    return std::move(Result);
}

// double
static std::unique_ptr<ExprAST> ParseDoubleExpr()
{
    auto Result = std::make_unique<DoubleExprAST>(dblVal);
    getNextToken(); // consume the number
    return std::move(Result);
}

/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr()
{
    getNextToken(); // eat (.
    auto V = ParseExpression();
    if (!V)
        return nullptr;

    if (CurTok != ')')
        return LogError("expected ')'");
    getNextToken(); // eat ).
    return V;
}



static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;
  getNextToken(); // eat identifier

  std::vector<std::unique_ptr<ExprAST>> Args;

  if (CurTok != TOK_INT && CurTok != TOK_FLOAT && CurTok != TOK_IDENT && CurTok != '(') {
      return std::make_unique<VariableExprAST>(IdName);
  }

  while (true) {
      if (auto Arg = ParseExpression())
          Args.push_back(std::move(Arg));
      else
          break;

      if (CurTok == ';' || CurTok == ')' || CurTok == TOK_EOF)
          break;
  }

  return std::make_unique<CallExprAST>(IdName, std::move(Args));
}


static std::unique_ptr<ExprAST> ParseIfExpr()
{
    getNextToken(); // eat the if.

    // condition.
    auto Cond = ParseExpression();
    if (!Cond)
        return nullptr;

    std::vector<std::unique_ptr<ExprAST>> IfBlock;

    if (CurTok != TOK_THEN )
        return LogError("expected 'then' after if condition");

    getNextToken(); // eat 'then'

    // if block
    while (CurTok != TOK_ELSE)
    {
        if (auto If = ParseExpression())
            IfBlock.push_back(std::move(If));

    }

    getNextToken(); // eat 

    std::vector<std::unique_ptr<ExprAST>> ElseBlock;

    // else block
    while (CurTok != ';')
    {
        if (auto Else = ParseExpression())
            ElseBlock.push_back(std::move(Else));
    }

    return std::make_unique<IfExprAST>(std::move(Cond), std::move(IfBlock),
                                       std::move(ElseBlock));
}


static std::unique_ptr<ExprAST> ParseForExpr() 
{
    getNextToken(); // eat 'for'

    if (CurTok != TOK_IDENT) {
        LogError("expected identifier after 'for'");
        return nullptr;
    }

    std::string IdName = IdentifierStr; // 'i'
    getNextToken(); // eat identifier

    if (CurTok != '=') {
        LogError("expected '=' after loop variable");
        return nullptr;
    }

    getNextToken(); // eat '='

    auto Start = ParseExpression(); 
    if (!Start)
        return nullptr;

    if (CurTok != TOK_TO) {
        LogError("expected 'to' after start expression");
        return nullptr;
    }

    getNextToken(); // eat 'to'

    auto End = ParseExpression(); 
    if (!End)
        return nullptr;

    
    std::unique_ptr<ExprAST> Step = nullptr;

    if (CurTok != TOK_DO) {
        LogError("expected 'do' after end expression");
        return nullptr;
    }

    getNextToken(); // eat 'do'
    std::vector<std::unique_ptr<ExprAST>> Body;

    auto BodyExpr = ParseExpression();
    if (!BodyExpr)
        return nullptr;

    Body.push_back(std::move(BodyExpr));

    return std::make_unique<ForExprAST>(
        IdName, std::move(Start), std::move(End),
        std::move(Step), std::move(Body));
}
/// primary
///   ::= identifierexpr
static std::unique_ptr<ExprAST> ParsePrimary()
{
    switch (CurTok)
    {
    case TOK_IDENT:
        return ParseIdentifierExpr();
    case TOK_INT:
        return ParseIntegerExpr();
    case TOK_FLOAT:
        return ParseDoubleExpr();
    case TOK_IF:
        return ParseIfExpr();
    case TOK_FOR:
        return ParseForExpr();
    case '(':
        return ParseParenExpr();
    default:
        LogError("unknown token when expecting an expression");
        return nullptr;
        break;
    }
}


static std::unique_ptr<ExprAST> ParseUnary() 
{
    
    if (!isascii(CurTok) || CurTok == '(' || CurTok == ',') {
        return ParsePrimary();
    }

    int Operator = CurTok;
    getNextToken();

    
    std::unique_ptr<ExprAST> Operand = ParseUnary();
    if (!Operand) {
        return nullptr;
    }

    return std::make_unique<UnaryExprAST>(Operator, std::move(Operand));
}

/// binoprhs
///   ::= ('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS)
{
    // If this is a binop, find its precedence.
    while (true)
    {
        int TokPrec = GetTokPrecedence();

        // If this is a binop that binds at least as tightly as the current binop,
        // consume it, otherwise we are done.
        if (TokPrec < ExprPrec)
            return LHS;

        // Okay, we know this is a binop.
        int BinOp = CurTok;
        getNextToken(); // eat binop

        // Parse the primary expression after the binary operator.
        auto RHS = ParseUnary();
        if (!RHS)
            return nullptr;

        // If BinOp binds less tightly with RHS than the operator after RHS, let
        // the pending operator take RHS as its LHS.
        int NextPrec = GetTokPrecedence();
        if (TokPrec < NextPrec)
        {
            RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
            if (!RHS)
                return nullptr;
        }

        // Merge LHS/RHS.
        LHS =
            std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
    }
}

/// expression
///   ::= primary binoprhs
///
static std::unique_ptr<ExprAST> ParseExpression()
{
    auto LHS = ParseUnary();
    if (!LHS)
        return nullptr;

    return ParseBinOpRHS(0, std::move(LHS));
}


static std::unique_ptr<PrototypeAST> ParsePrototype()
{
    std::string FnName;
    unsigned Kind = 0; // 0 = identifier, 1 = unary, 2 = binary
    unsigned BinaryPrecedence = 30;

    switch (CurTok)
    {
    default:
        return LogErrorP("Expected function name in prototype");

    case TOK_IDENT:
        FnName = IdentifierStr;
        Kind = 0;
        getNextToken();
        break;

    case TOK_UNARY:
        getNextToken();
        if (!isascii(CurTok))
            return LogErrorP("Expected unary operator");
        FnName = "unary";
        FnName += static_cast<char>(CurTok);
        Kind = 1;
        getNextToken();
        break;

    case TOK_BINARY:
        getNextToken();
        if (!isascii(CurTok))
            return LogErrorP("Expected binary operator");
        FnName = "binary";
        FnName += static_cast<char>(CurTok);
        Kind = 2;
        getNextToken();

        if (CurTok == TOK_INT)
        {
            if (intVal < 1 || intVal > 100)
                return LogErrorP("Invalid precedence: must be 1 to 100");
            BinaryPrecedence = static_cast<unsigned>(intVal);
            getNextToken();
        }
        break;
    }

    // Parse argument names without parentheses
    std::vector<std::string> ArgNames;
    while (CurTok == TOK_IDENT)
    {
        ArgNames.push_back(IdentifierStr);
        getNextToken();

        // Optional: allow stop condition if needed, e.g., newline or something else
        if (CurTok != TOK_IDENT && CurTok != TOK_EOF)
            break;
    }

    if (Kind && ArgNames.size() != Kind)
        return LogErrorP("Invalid number of operands for operator");

    return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames), Kind != 0, BinaryPrecedence);
}


static std::unique_ptr<FunctionAST> ParseFunctionDefinition()
{
    getNextToken(); // eat let.

    auto Proto = ParsePrototype();

    if (!Proto)
        return nullptr;

    if (CurTok != '=')
        return nullptr;

    getNextToken(); 

    auto Body = ParseExpression();

    if (!Body)
        return nullptr;

    if (CurTok != ';')
        return nullptr;

    getNextToken(); 

    return std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
}

/// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
  if (auto E = ParseExpression()) {
    // Make an anonymous proto.
    auto Proto = std::make_unique<PrototypeAST>("__anon_expr",
                                                 std::vector<std::string>());
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  }
  return nullptr;
}


/// external ::= 'import' prototype
static std::unique_ptr<PrototypeAST> ParseImport()
{
    getNextToken(); // eat open.
    return ParsePrototype();
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;
static std::map<std::string, Value *> NamedValues;
static std::unique_ptr<KaleidoscopeJIT> TheJIT;
static std::unique_ptr<FunctionPassManager> TheFPM;
static std::unique_ptr<LoopAnalysisManager> TheLAM;
static std::unique_ptr<FunctionAnalysisManager> TheFAM;
static std::unique_ptr<CGSCCAnalysisManager> TheCGAM;
static std::unique_ptr<ModuleAnalysisManager> TheMAM;
static std::unique_ptr<PassInstrumentationCallbacks> ThePIC;
static std::unique_ptr<StandardInstrumentations> TheSI;
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
static ExitOnError ExitOnErr;

Value *LogErrorV(const char *Str)
{
    LogError(Str);
    return nullptr;
}

Function *getFunction(std::string Name)
{
    // First, see if the function has already been added to the current module.
    if (auto *F = TheModule->getFunction(Name))
        return F;

    // If not, check whether we can codegen the declaration from some existing
    // prototype.
    auto FI = FunctionProtos.find(Name);
    if (FI != FunctionProtos.end())
        return FI->second->codegen();

    // If no existing prototype exists, return null.
    return nullptr;
}

Value *IntegerExprAST::codegen()
{
    // 32-bitni int; signed int
    return ConstantInt::get(*TheContext, APInt(32, Val, true));
}

Value *DoubleExprAST::codegen()
{
    return ConstantFP::get(*TheContext, APFloat(Val));
}

Value *VariableExprAST::codegen()
{
    // Look this variable up in the function.
    Value *V = NamedValues[Name];
    if (!V)
        return LogErrorV("Unknown variable name");
    return V;
}

Value *UnaryExprAST::codegen()
{
    Value *OperandV = Operand->codegen();
    if (!OperandV)
        return nullptr;

    Function *F = getFunction(std::string("unary") + Opcode);
    if (!F)
        return LogErrorV("Unknown unary operator");

    return Builder->CreateCall(F, OperandV, "unop");
}

Value *BinaryExprAST::codegen()
{

     // Special case '=' because we don't want to emit the LHS as an expression.
    if (Op == '=')
    {
        // Assignment requires the LHS to be an identifier.
        // This assume we're building without RTTI because LLVM builds that way by
        // default.  If you build LLVM with RTTI this can be changed to a
        // dynamic_cast for automatic error checking.
        VariableExprAST *LHSE = static_cast<VariableExprAST *>(LHS.get());
        if (!LHSE)
            return LogErrorV("destination of '=' must be a variable");
        // Codegen the RHS.
        Value *Val = RHS->codegen();
        if (!Val)
            return nullptr;

        // Look up the name.
        Value *Variable = NamedValues[LHSE->getName()];
        if (!Variable)
            return LogErrorV("Unknown variable name");

        Builder->CreateStore(Val, Variable);
        return Val;
    }

    Value *L = LHS->codegen();
    Value *R = RHS->codegen();

    if (!L || !R)
        return nullptr;

    switch (Op)
    {
    case '+':
        if (L->getType()->isIntegerTy() && R->getType()->isDoubleTy())
        {
            // Convert L to double and then perform the addition.
            L = Builder->CreateSIToFP(L, Type::getDoubleTy(*TheContext));
            return Builder->CreateFAdd(L, R, "addtmp");
        }
        else if (L->getType()->isDoubleTy() && R->getType()->isIntegerTy())
        {
            // Convert R to double and then perform the addition.
            R = Builder->CreateSIToFP(R, Type::getDoubleTy(*TheContext));
            return Builder->CreateFAdd(L, R, "addtmp");
        }
        else if (L->getType()->isDoubleTy() && R->getType()->isDoubleTy())
        {
            // Both operands are already double, perform the addition.
            return Builder->CreateFAdd(L, R, "addtmp");
        }
        else if (L->getType()->isIntegerTy() && R->getType()->isIntegerTy())
        {
            L = Builder->CreateSIToFP(L, Type::getDoubleTy(*TheContext));
            R = Builder->CreateSIToFP(R, Type::getDoubleTy(*TheContext));
            return Builder->CreateFAdd(L, R, "addtmp");
        }
    case '-':
        if (L->getType()->isIntegerTy() && R->getType()->isDoubleTy())
        {
            // Convert L to double and then perform the subtraction.
            L = Builder->CreateSIToFP(L, Type::getDoubleTy(*TheContext));
            return Builder->CreateFSub(L, R, "subtmp");
        }
        else if (L->getType()->isDoubleTy() && R->getType()->isIntegerTy())
        {
            // Convert R to double and then perform the subtraction.
            R = Builder->CreateSIToFP(R, Type::getDoubleTy(*TheContext));
            return Builder->CreateFSub(L, R, "subtmp");
        }
        else if (L->getType()->isDoubleTy() && R->getType()->isDoubleTy())
        {
            // Both operands are already double, perform the subtraction.
            return Builder->CreateFSub(L, R, "subtmp");
        }
        else if (L->getType()->isIntegerTy() && R->getType()->isIntegerTy())
        {
            L = Builder->CreateSIToFP(L, Type::getDoubleTy(*TheContext));
            R = Builder->CreateSIToFP(R, Type::getDoubleTy(*TheContext));
            return Builder->CreateFSub(L, R, "subtmp");
        }

    case '*':
        if (L->getType()->isIntegerTy() && R->getType()->isDoubleTy())
        {
            // Convert L to double and then perform the multiplication.
            L = Builder->CreateSIToFP(L, Type::getDoubleTy(*TheContext));
            return Builder->CreateFMul(L, R, "multmp");
        }
        else if (L->getType()->isDoubleTy() && R->getType()->isIntegerTy())
        {
            // Convert R to double and then perform the multiplication.
            R = Builder->CreateSIToFP(R, Type::getDoubleTy(*TheContext));
            return Builder->CreateFMul(L, R, "multmp");
        }
        else if (L->getType()->isDoubleTy() && R->getType()->isDoubleTy())
        {
            // Both operands are already double, perform the multiplication.
            return Builder->CreateFMul(L, R, "multmp");
        }
        else if (L->getType()->isIntegerTy() && R->getType()->isIntegerTy())
        {
            L = Builder->CreateSIToFP(L, Type::getDoubleTy(*TheContext));
            R = Builder->CreateSIToFP(R, Type::getDoubleTy(*TheContext));
            return Builder->CreateFMul(L, R, "multmp");
        }

    case '<':
        L = Builder->CreateFCmpULT(L, R, "cmptmp");
        // Convert bool 0/1 to double 0.0 or 1.0
        return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
    default:
        break;
    }

    // If it wasn't a builtin binary operator, it must be a user defined one. Emit
    // a call to it.
    Function *F = getFunction(std::string("binary") + Op);
    assert(F && "binary operator not found!");

    Value *Ops[] = {L, R};
    return Builder->CreateCall(F, Ops, "binop");
}

Value *CallExprAST::codegen()
{
    // Look up the name in the global module table.
    Function *CalleeF = getFunction(Callee);
    if (!CalleeF)
        return LogErrorV("Unknown function referenced");

    // If argument mismatch error.
    if (CalleeF->arg_size() != Args.size())
        return LogErrorV("Incorrect # arguments passed");

    std::vector<Value *> ArgsV;
    for (unsigned i = 0, e = Args.size(); i != e; ++i)
    {
        Value *ArgValue = Args[i]->codegen();
        if (!ArgValue)
            return nullptr;

        Type *ExpectedType = CalleeF->getFunctionType()->getParamType(i);

        // Perform type conversion if the argument type doesn't match the expected type.
        if (ArgValue->getType() != ExpectedType)
        {
            if (ArgValue->getType()->isIntegerTy() && ExpectedType->isDoubleTy())
            {
                // Convert integer to double.
                ArgValue = Builder->CreateSIToFP(ArgValue, Type::getDoubleTy(*TheContext));
            }
            else if (ArgValue->getType()->isDoubleTy() && ExpectedType->isIntegerTy())
            {
                // Convert double to integer.
                ArgValue = Builder->CreateFPToSI(ArgValue, ExpectedType);
            }
        }

        ArgsV.push_back(ArgValue);
    }

    return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

Value *IfExprAST::codegen()
{
    Value *CondV = Cond->codegen();
    if (!CondV)
        return nullptr;

    // Convert condition to a bool by comparing non-equal to 0.0.
    CondV = Builder->CreateFCmpONE(
        CondV, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond");

    Function *TheFunction = Builder->GetInsertBlock()->getParent();

    // Create blocks for the then and else cases.  Insert the 'then' block at the
    // end of the function.
    BasicBlock *IfBB = BasicBlock::Create(*TheContext, "if", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else");
    BasicBlock *MergeBB = BasicBlock::Create(*TheContext, "ifcont");

    Builder->CreateCondBr(CondV, IfBB, ElseBB);

    // Emit then value.
    Builder->SetInsertPoint(IfBB);

    std::vector<Value *> IfVs(IfBlock.size());
    for (auto &expr : IfBlock)
    {
        Value *IfV = expr->codegen();
        if (!IfV)
        {
            return nullptr;
        }
        IfVs.push_back(IfV);
    }

    Builder->CreateBr(MergeBB);
    // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
    IfBB = Builder->GetInsertBlock();

    // Emit else block.
    TheFunction->insert(TheFunction->end(), ElseBB);
    Builder->SetInsertPoint(ElseBB);

    std::vector<Value *> ElseVs(ElseBlock.size());
    for (auto &expr : ElseBlock)
    {
        Value *ElseV = expr->codegen();
        if (!ElseV)
        {
            return nullptr;
        }
        ElseVs.push_back(ElseV);
    }

    Builder->CreateBr(MergeBB);
    // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
    ElseBB = Builder->GetInsertBlock();

    // Emit merge block.
    TheFunction->insert(TheFunction->end(), MergeBB);
    Builder->SetInsertPoint(MergeBB);

    PHINode *PN = nullptr;
    PN = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, "ifftmp");

    PN->addIncoming(IfVs.back(), IfBB);
    PN->addIncoming(ElseVs.back(), ElseBB);
    return PN;
}

// Output for-loop as:
//   ...
//   start = startexpr
//   goto loop
// loop:
//   variable = phi [start, loopheader], [nextvariable, loopend]
//   ...
//   bodyexpr
//   ...
// loopend:
//   step = stepexpr
//   nextvariable = variable + step
//   endcond = endexpr
//   br endcond, loop, endloop
// outloop:
Value *ForExprAST::codegen()
{
    // Emit the start code first, without 'variable' in scope.
    // Type-checking and conversion if necessary.
    Type *VarType = Type::getDoubleTy(*TheContext);
    Value *StartVal = Start->codegen();
    Value *EndVal = End->codegen();

    if (!StartVal || !EndVal)
        return nullptr;

    // Check if the types are integers, and convert them to float if needed.
    if (StartVal->getType()->isIntegerTy())
    {
        StartVal = Builder->CreateSIToFP(StartVal, VarType, "caststart");
    }

    if (EndVal->getType()->isIntegerTy())
    {
        EndVal = Builder->CreateSIToFP(EndVal, VarType, "castend");
    }


    // Make the new basic block for the loop header, inserting after the current block.
    Function *TheFunction = Builder->GetInsertBlock()->getParent();
    BasicBlock *PreheaderBB = Builder->GetInsertBlock();
    BasicBlock *LoopBB = BasicBlock::Create(*TheContext, "loop", TheFunction);

    // Insert an explicit fall through from the current block to the LoopBB.
    Builder->CreateBr(LoopBB);

    // Start insertion in LoopBB.
    Builder->SetInsertPoint(LoopBB);

    // Start the PHI node with an entry for Start.
    PHINode *Variable =
        Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, VarName);
    Variable->addIncoming(StartVal, PreheaderBB);

    // Within the loop, the variable is defined equal to the PHI node.
    // If it shadows an existing variable, we have to restore it, so save it now.
    Value *OldVal = NamedValues[VarName];
    NamedValues[VarName] = Variable;

    // Emit the body of the loop.
    for (const auto &BodyExpr : Body)
    {
        if (!BodyExpr->codegen())
            return nullptr;
    }

    // Emit the step value.
    Value *StepVal = nullptr;
    if (Step)
    {
        StepVal = Step->codegen();
        if (!StepVal)
            return nullptr;
    }
    else
    {
        // If not specified, use 1.0.
        StepVal = ConstantFP::get(*TheContext, APFloat(1.0));
    }

    Value *NextVar = Builder->CreateFAdd(Variable, StepVal, "nextvar");

    // Convert condition to a bool by comparing non-equal to 0.0.
    Value *EndCond = Builder->CreateFCmpONE(
        Variable, EndVal, "loopcond");

    // Create the "after loop" block and insert it.
    BasicBlock *LoopEndBB = Builder->GetInsertBlock();
    BasicBlock *AfterBB =
        BasicBlock::Create(*TheContext, "afterloop", TheFunction);

    // Insert the conditional branch into the end of LoopEndBB.
    Builder->CreateCondBr(EndCond, LoopBB, AfterBB);

    // Any new code will be inserted in AfterBB.
    Builder->SetInsertPoint(AfterBB);

    // Add a new entry to the PHI node for the backedge.
    Variable->addIncoming(NextVar, LoopEndBB);

    // Restore the unshadowed variable.
    if (OldVal)
        NamedValues[VarName] = OldVal;
    else
        NamedValues.erase(VarName);

    // for expr always returns 0.0.
    return Constant::getNullValue(Type::getDoubleTy(*TheContext));
}

Function *PrototypeAST::codegen() {
  // Make the function type:  double(double,double) etc.
  std::vector<Type *> Doubles(Args.size(), Type::getDoubleTy(*TheContext));
  FunctionType *FT =
      FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);

  Function *F =
      Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

  // Set names for all arguments.
  unsigned Idx = 0;
  for (auto &Arg : F->args())
    Arg.setName(Args[Idx++]);

  return F;
}


Function *FunctionAST::codegen()
{
    // Transfer ownership of the prototype to the FunctionProtos map, but keep a
    // reference to it for use below.
    auto &P = *Proto;
    FunctionProtos[Proto->getName()] = std::move(Proto);
    Function *TheFunction = getFunction(P.getName());
    if (!TheFunction)
        return nullptr;
    // If this is an operator, install it.
    if (P.isBinaryOp())
        BinopPrecedence[P.getOperatorName()] = P.getBinaryPrecedence();

    // Create a new basic block to start insertion into.
    BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
    Builder->SetInsertPoint(BB);

    // Record the function arguments in the NamedValues map.
    NamedValues.clear();
    for (auto &Arg : TheFunction->args())
        NamedValues[std::string(Arg.getName())] = &Arg;

    if (Value *RetVal = Body->codegen())
    {
        // Finish off the function.
        Builder->CreateRet(RetVal);

        // Validate the generated code, checking for consistency.
        verifyFunction(*TheFunction);

        // Run the optimizer on the function.
        TheFPM->run(*TheFunction, *TheFAM);

        return TheFunction;
    }

    // Error reading body, remove function.
    TheFunction->eraseFromParent();

    if (P.isBinaryOp())
        BinopPrecedence.erase(P.getOperatorName());

    return nullptr;
}

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

static void InitializeModuleAndManagers()
{
    // Open a new context and module.
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("KaleidoscopeJIT", *TheContext);
    TheModule->setDataLayout(TheJIT->getDataLayout());

    // Create a new builder for the module.
    Builder = std::make_unique<IRBuilder<>>(*TheContext);

    // Create new pass and analysis managers.
    TheFPM = std::make_unique<FunctionPassManager>();
    TheLAM = std::make_unique<LoopAnalysisManager>();
    TheFAM = std::make_unique<FunctionAnalysisManager>();
    TheCGAM = std::make_unique<CGSCCAnalysisManager>();
    TheMAM = std::make_unique<ModuleAnalysisManager>();
    ThePIC = std::make_unique<PassInstrumentationCallbacks>();
    TheSI = std::make_unique<StandardInstrumentations>(*TheContext,
                                                       /*DebugLogging*/ true);
    TheSI->registerCallbacks(*ThePIC, TheMAM.get());

    // Add transform passes.
    // Do simple "peephole" optimizations and bit-twiddling optzns.
    TheFPM->addPass(InstCombinePass());
    // Reassociate expressions.
    TheFPM->addPass(ReassociatePass());
    // Eliminate Common SubExpressions.
    TheFPM->addPass(GVNPass());
    // Simplify the control flow graph (deleting unreachable blocks, etc).
    TheFPM->addPass(SimplifyCFGPass());

    // Register analysis passes used in these transform passes.
    PassBuilder PB;
    PB.registerModuleAnalyses(*TheMAM);
    PB.registerFunctionAnalyses(*TheFAM);
    PB.crossRegisterProxies(*TheLAM, *TheFAM, *TheCGAM, *TheMAM);
}

static void HandleDefinition()
{
    if (auto FnAST = ParseFunctionDefinition())
    {
        if (auto *FnIR = FnAST->codegen())
        {
            fprintf(stderr, "Read function definition:");
            FnIR->print(errs());
            fprintf(stderr, "\n");
            ExitOnErr(TheJIT->addModule(
                ThreadSafeModule(std::move(TheModule), std::move(TheContext))));
            InitializeModuleAndManagers();
        }
    }
    else
    {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleImport()
{
    if (auto ProtoAST = ParseImport())
    {
        if (auto *FnIR = ProtoAST->codegen())
        {
            fprintf(stderr, "Read import: ");
            FnIR->print(errs());
            fprintf(stderr, "\n");
            FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
        }
    }
    else
    {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleTopLevelExpression()
{
    // Evaluate a top-level expression into an anonymous function.
    if (auto FnAST = ParseTopLevelExpr())
    {
        if (FnAST->codegen())
        {
            // Create a ResourceTracker to track JIT'd memory allocated to our
            // anonymous expression -- that way we can free it after executing.
            auto RT = TheJIT->getMainJITDylib().createResourceTracker();

            auto TSM = ThreadSafeModule(std::move(TheModule), std::move(TheContext));
            ExitOnErr(TheJIT->addModule(std::move(TSM), RT));
            InitializeModuleAndManagers();

            // Search the JIT for the __anon_expr symbol.
            auto ExprSymbol = ExitOnErr(TheJIT->lookup("__anon_expr"));

            // Get the symbol's address and cast it to the right type (takes no
            // arguments, returns a double) so we can call it as a native function.
            double (*FP)() = ExprSymbol.getAddress().toPtr<double (*)()>();
            fprintf(stderr, "Evaluated to %f\n", FP());

            // Delete the anonymous expression module from the JIT.
            ExitOnErr(RT->remove());
        }
    }
    else
    {
        // Skip token for error recovery.
        getNextToken();
    }
}

/// top ::= definition | external | expression | ';'
static void MainLoop()
{
    while (true)
    {
        fprintf(stderr, "ready> ");
        switch (CurTok)
        {
        case TOK_EOF:
            return;
        case ';': // ignore top-level semicolons.
            getNextToken();
            break;
        case TOK_LET:
            HandleDefinition();
            break;
        case TOK_OPEN:
            HandleImport();
            break;
        default:
            HandleTopLevelExpression();
            break;
        }
    }
}

//===----------------------------------------------------------------------===//
// "Library" functions that can be "extern'd" from user code.
//===----------------------------------------------------------------------===//

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putchard - putchar that takes a double and returns 0.
extern "C" DLLEXPORT double putchard(double X)
{
    fputc((char)X, stderr);
    return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" DLLEXPORT double printd(double X)
{
    fprintf(stderr, "%f\n", X);
    return 0;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main()
{
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    // Install standard binary operators.
    // 1 is lowest precedence.
    BinopPrecedence['<'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40; // highest.

    // Prime the first token.
    fprintf(stderr, "ready> ");
    getNextToken();

    TheJIT = ExitOnErr(KaleidoscopeJIT::Create());

    InitializeModuleAndManagers();

    // Run the main "interpreter loop" now.
    MainLoop();

    return 0;
}
