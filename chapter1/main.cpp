#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>
#include <iostream>
using namespace std;


enum Token {
    TOK_EOF = -1,
  
    // commands
    TOK_LET=-2,
    TOK_OPEN=-3,
    
    // primary
    TOK_IDENT  = -4,
    TOK_INT = -5,
    TOK_FLOAT = -6,
    TOK_LIST = -7,
  };


  static std::string identifierStr; // Filled in if TOK_IDENT
  static int intVal;                // Filled in if TOK_INT
  static double floatVal;   // FIlled in if TOK_FLOAT
  
  static int gettok()
  {
      static int lastChar = ' ';
  
      // Skip any whitespace.
      while (isspace(lastChar))
          lastChar = getchar();
  
      // list: [1;2;3]
      if (lastChar == '[')
      {
          std::string listStr;
          lastChar = getchar();
          while(lastChar != ']' && lastChar != EOF)
          {
              listStr += lastChar;
  
              lastChar = getchar();
          }
        if(lastChar == ']')
        lastChar = getchar();
        return TOK_LIST;
      }

      // identifier: [a-zA-Z][a-zA-Z0-9]*
      if (isalpha(lastChar))
      {
          identifierStr = lastChar;
  
          while (isalnum((lastChar = getchar())))
              identifierStr += lastChar;
  
          if (identifierStr == "let")
              return TOK_LET;
  
          if (identifierStr == "open")
              return TOK_OPEN;

  
          return TOK_IDENT;
      }
       // Integer: [0-9]+ or Float: [0-9]+.[0-9]+
      if (isdigit(lastChar) || (lastChar == '.'))
      {
          std::string numStr;
          bool isFloat = false;
  
          do
          {
              numStr += lastChar;
  
              if (lastChar == '.')
              {
                  if (isFloat)
                  {
                      break;
                  }
  
                  isFloat = true;
              }
  
              lastChar = getchar();
          } while (isdigit(lastChar) || (!isFloat && lastChar == '.'));
  
          if (isFloat)
          {
              floatVal = strtod(numStr.c_str(), nullptr);
              return TOK_FLOAT;
          }
           else
          {
              intVal = strtol(numStr.c_str(), nullptr, 10);
              return TOK_INT;
          }
      }
    // Comments : //
      if (lastChar == '/' && ((lastChar = getchar()) == '/'))
      {
          // Comment until end of line.
          // single-line comment
          do
              lastChar = getchar();
          while (lastChar != EOF && lastChar != '\n' && lastChar != '\r');
  
          if (lastChar != EOF)
              return gettok();
      }
      //Comments: """ """
      else if (lastChar == '"' && ((lastChar = getchar()) == '"') && ((lastChar = getchar() == '"')))
      {
          // Comment until the closing '"""'.
          // multi-line comment
          do
              lastChar = getchar();
          while (lastChar != EOF && lastChar != '"' && lastChar != '"' && lastChar != '"');
  
          if (lastChar != EOF)
              return gettok();
      }
      
  
   
    // Check for end of file.  Don't eat the EOF.
   if (lastChar == EOF)
      return TOK_EOF;
  
    // Otherwise, just return the character as its ascii value.
    int ThisChar = lastChar;
    lastChar = getchar();
    return ThisChar;
  }
  
  

int main(){
    while(true){
        cout << "ready>";
        int tok = gettok();
        cout <<  "got token: " << tok << endl;
    }
}
