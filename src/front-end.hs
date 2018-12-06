{-# LANGUAGE FlexibleContexts #-}
import Text.ParserCombinators.Parsec
import Text.Parsec (ParsecT)
import Text.Parsec.Prim (Stream)

{-
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
-}

-- <relop> ::= > | < | =
relop = try (char '>')
        <|> try (char '<')
        <|> char '='

-- <mulop> ::= * | /
mulop = try (char '*')
        <|> char '/'

-- <addop> ::= + | -
addop = try (char '+')
        <|> char '-'

-- <digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
-- digit ja esta definido no parsec

-- <digiti> ::= <digit> | <digiti> <digiti>
digiti :: Stream s m Char => ParsecT s u m [Char]
digiti = many1 digit

-- <var> ::= <Identifier> que sera letter+ ++ alphanum*
var :: Stream s m Char => ParsecT s u m [Char]
var = do
  start <- many1 letter
  rest <- many alphaNum
  return (start ++ rest)

-- <factor> ::= <var> | <digiti> | (<expr>)
factor =
  try var
  <|> try digiti
  <|>
  do
    pi <- string "("
    e <- expr
    pf <- string ")"
    return $ pi ++ e ++ pf

-- <term> ::= <term> <mulop> <factor> | <factor>
-- eliminado recursao a' esquerda:
-- <term> ::= <factor> <term'>
-- <term'> ::= <mulop> <factor> <term'> | vazio
term =
  do
    f <- factor
    t' <-term'
    return $ f ++ t'
  where
    term' = try (
      do
        m <- mulop
        f1 <- factor
        t1' <- term'
        return $ m : f1 ++ t1'
      )
      <|> string ""

-- <expr> ::= <expr> <addop> <term> | <term>

-- <expr> ::=  <term> <expr'>
-- <expr'> ::= <addop> <term> <expr'> | vazio
expr =
  do
    t <- term
    e' <- expr'
    return $ t ++ e'
  where
    expr' = try (
      do
        aop <- addop
        t <- term
        ex' <- expr'
        return $ aop : t ++ ex'
      )
      <|> string ""
  
  
-- <rexp> ::= <rexp> <relop> <expr> | <expr>

-- <rexp> ::= <expr> <rexp'>
-- <rexp'> ::= <relop> <expr> <rexp'> | vazio
rexp =
  do
    e <- expr
    r' <- rexp'
    return $ e ++ r'
  where
    rexp' = try (
      do
        rl <- relop
        e1 <- expr
        r1' <- rexp'
        return $ rl : e1 ++ r1'
      )
      <|> string ""
  
{-
languageDef =
  emptyDef { Token.commentStart = "/*"
           , Token.commentEnd = "*/"
           , Token.commentLine = "//"
           , Token.identStart = letter
           , Token.identLetter = alphaNum
           , Token.reservedNames = ["if"
                                   ,"then"
                                   ,"else"
                                   ,"while"
                                   ,"do"
                                   ,"declare"
                                   ,"in"
                                   ,"print"
                                   ]
           , Token.reservedOpNames = ["<",">","="
                                     ,"-","+"
                                     ,"*","/"]
           }

lexer = Token.makeTokenParser languageDef

-- extraindo parsers lexicos
identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
pv         = Token.semi       lexer -- ;
whiteSpace = Token.whiteSpace lexer

whileParser :: Parser Com
whileParser = whiteSpace >> statement
-}