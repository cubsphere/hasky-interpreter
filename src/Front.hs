{-# LANGUAGE FlexibleContexts #-}
module Front
  (
    com
  ) where

import Text.ParserCombinators.Parsec
import Text.Parsec (ParsecT)
import Text.Parsec.Prim (Stream)
import PolyConc
import Data.Functor.Identity (Identity)
import Back

{-
-- EXPRESSOES
-- <relop> ::= > | < | =
relop = (char '>')
        <|> (char '<')
        <|> char '='
        <?> "relop"

-- <mulop> ::= * | /
mulop = (char '*')
        <|> char '/'
        <?> "mulop"

-- <addop> ::= + | -
addop = (char '+')
        <|> char '-'
        <?> "addop"
-}
-- <digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
-- digit ja esta definido no parsec

-- <digiti> ::= <digit> | <digiti> <digiti>
digiti = (Constant . read) <$> (many1 digit)

-- <var> ::= <Identifier> que sera letter ++ alphanum*
var = (Variable . show) <$> ((:) <$> letter <*> many alphaNum)

-- <factor> ::= <var> | <digiti> | (<expr>)
factor :: ParsecT [Char] m Identity Exp
factor =
  var
  <|> digiti
  <|> (string "(" *> spaces) *> expr <* (spaces *> string ")")
  <?> "factor"

-- <term> ::= <term> '*' <factor> | <tem> '/' <factor | <factor>
-- eliminado recursao a' esquerda:
-- <term> ::= <factor> <term'>
-- <term'> ::= '*' <factor> <term'> | '/' <factor> <term'> | vazio
term = try (Times <$> factor <*> termF)
       <|> try (Divide <$> factor <*> termD)
       <|> factor
       where
         termF = try (Times <$> (spaces *> char '*' *> spaces *> factor) <*> termF)
                 <|> try (Divide <$> (spaces *> char '*' *> spaces *> factor) <*> termD)
                 <|> (spaces *> char '*' *> spaces *> factor)
         termD = try (Times <$> (spaces *> char '/' *> spaces *> factor) <*> termF)
                 <|> try (Divide <$> (spaces *> char '/' *> spaces *> factor) <*> termD)
                 <|> (spaces *> char '/' *> spaces *> factor)

-- <expr> ::= <expr> <addop> <term> | <term>
-- <expr> ::=  <term> <expr'>
-- <expr'> ::= <addop> <term> <expr'> | vazio
expr = try (Plus <$> term <*> exprP)
       <|> try (Minus <$> term <*> exprM)
       <|> term
       where
         exprP = try (Plus <$> (spaces *> char '+' *> spaces *> term) <*> exprP)
                 <|> try (Minus <$> (spaces *> char '+' *> spaces *> term) <*> exprM)
                 <|> (spaces *> char '+' *> spaces *> term)
         exprM = try (Plus <$> (spaces *> char '-' *> spaces *> term) <*> exprP)
                 <|> try (Minus <$> (spaces *> char '-' *> spaces *> term) <*> exprM)
                 <|> (spaces *> char '-' *> spaces *> term)
  
-- <rexp> ::= <rexp> <relop> <expr> | <expr>

-- <rexp> ::= <expr> <rexp'>
-- <rexp'> ::= <relop> <expr> <rexp'> | vazio
rexp = try (Greater <$> expr <*> rexpG)
       <|> try (Less <$> expr <*> rexpL)
       <|> try (Equal <$> expr <*> rexpE)
       <|> expr
       where
         rexpG = try (Greater <$> (spaces *> char '>' *> spaces *> expr) <*> rexpG)
                 <|> try (Less <$> (spaces *> char '>' *> spaces *> expr) <*> rexpL)
                 <|> try (Equal <$> (spaces *> char '>' *> spaces *> expr) <*> rexpE)
                 <|> (spaces *> char '>' *> spaces *> expr)
         rexpL = try (Greater <$> (spaces *> char '<' *> spaces *> expr) <*> rexpG)
                 <|> try (Less <$> (spaces *> char '<' *> spaces *> expr) <*> rexpL)
                 <|> try (Equal <$> (spaces *> char '<' *> spaces *> expr) <*> rexpE)
                 <|> (spaces *> char '<' *> spaces *> expr)
         rexpE = try (Greater <$> (spaces *> char '=' *> spaces *> expr) <*> rexpG)
                 <|> try (Less <$> (spaces *> char '=' *> spaces *> expr) <*> rexpL)
                 <|> try (Equal <$> (spaces *> char '=' *> spaces *> expr) <*> rexpE)
                 <|> (spaces *> char '=' *> spaces *> expr)


-- COMANDOS
-- <printe> ::= print <rexp>
printe = (Print) <$> (string "print" *> space *> spaces *> rexp)

-- <declare> ::= declare <identif> = <rexp> in <com>
-- <identif> sera meu <var>
declare = (Declare) <$> ((string "declare") *> space *> spaces *> var) <*> (spaces *> string "=" *> spaces *> rexp) <*> (space *> spaces *> string "in" *> space *> spaces *> com)

-- <while> := while <rexp> do <com>
while = (While) <$> ((string "while") *> space *> spaces *> rexp) <*> (space *> spaces *> string "do" *> space *> spaces *> com)

-- <cond> := if <rexp> then <com> else <com>
cond = (Cond) <$> ((string "if") *> space *> spaces *> rexp) <*> (space <* spaces *> string "then" *> space *> spaces *> com) <*> (space *> spaces *> string "else" *> space *> spaces *> com)

-- <seqv> ::= { <com> ; <com> }
seqv = (Seq) <$> ((char '{') *> spaces *> com) <*> (spaces *> char ';' *> spaces *> com) <* (spaces *> char '}')

-- <assign> ::= <identif> := <rexp>
assign = (Assign) <$> var <*> (spaces *> string ":=" *> spaces *> rexp)

-- <com> ::= <assign> | <seqv> | <cond> | <while> | <declare> | <printe>
com = try assign <|> try seqv <|> try cond <|> try while <|> try declare <|> printe <?> "com"

{-
test :: IO ()
test = do
  putStrLn "TESTANDO..."

  -- rexp
  let testN = 1 :: Integer
  putStrLn "\ntestando rexp"
  putStr "teste..." >> print testN
  let ret = parse rexp "" ""
  case (ret) of
    Right e -> putStrLn "ERRO"
    Left e -> putStrLn "OK"
  testN <- return (succ testN)
  
  putStr "teste..." >> print testN
  let ret = parse rexp "" "a > b + 2 * a / 1 /a"
  case (ret) of
    Right e -> if (e == "a>b+2*a/1/a")
                  then putStrLn "OK" else putStr "ERRO" >> print e >> putStrLn ""
    Left e -> putStrLn "ERRO"
  testN <- return (succ testN)
  
  -- printe
  let testN = 1 :: Integer
  putStrLn "\ntestando printe"
  putStr "teste..." >> print testN
  let ret = parse printe "" ""
  case (ret) of
    Right e -> putStrLn "ERRO" >> print e
    Left e -> putStrLn "OK"
  testN <- return (succ testN)

  putStr "teste..." >> print testN
  let ret = parse printe "" "print     2"
  case (ret) of
    Right e -> if (e == "print 2")
               then putStrLn "OK" else putStr "ERRO" >> print e >> putStrLn ""
    Left e -> putStrLn "ERRO"
  testN <- return (succ testN)

  putStr "teste..." >> print testN
  let ret = parse printe "" "print2"
  case (ret) of
    Right e -> putStrLn "ERRO" >> print e
    Left e -> putStrLn "OK"
  testN <- return (succ testN)

  putStr "teste..." >> print testN
  let ret = parse printe "" "print(2)"
  case (ret) of
    Right e -> putStrLn "ERRO" >> print e
    Left e -> putStrLn "OK"
  testN <- return (succ testN)

  putStr "teste..." >> print testN
  let ret = parse printe "" "print     (2 * 2)"
  case (ret) of
    Right e -> if (e == "print (2*2)")
               then putStrLn "OK" else putStr "ERRO - " >> print e >> putStrLn ""
    Left e -> putStrLn "ERRO"
  testN <- return (succ testN)

  putStr "teste..." >> print testN
  let ret = parse printe "" "print     (   2 * 2 + 3) 1"
  case (ret) of
    Right e -> if (e == "print (2*2+3)")
               then putStrLn "OK" else putStr "ERRO - " >> print e >> putStrLn ""
    Left e -> putStrLn "ERRO"
  testN <- return (succ testN)

  -- testando declare
  let testN = 1 :: Integer
  putStrLn "\ntestando declare"
  putStr "teste..." >> print testN
  let ret = parse declare "" ""
  case (ret) of
    Right e -> putStrLn "ERRO"
    Left e -> putStrLn "OK"
  testN <- return (succ testN)

  putStr "teste..." >> print testN
  let ret = parse declare "" "declare a = 2 in print 2"
  case (ret) of
    Right e -> if (e == "declare a=2 in print 2")
               then putStrLn "OK" else putStr "ERRO - " >> print e >> putStrLn ""
    Left e -> putStrLn "ERRO"
  testN <- return (succ testN)

  putStr "teste..." >> print testN
  let ret = parse declare "" "declare    a=2    in print    2      "
  case (ret) of
    Right e -> if (e == "declare a=2 in print 2")
               then putStrLn "OK" else putStr "ERRO - " >> print e >> putStrLn ""
    Left e -> putStrLn "ERRO"
  testN <- return (succ testN)

  putStr "teste..." >> print testN
  let ret = parse declare "" "declare    a=2inprint    2      "
  case (ret) of
    Right e -> putStrLn "ERRO" >> print e
    Left e -> putStrLn "OK"
  testN <- return (succ testN)

  putStr "teste..." >> print testN
  let ret = parse declare "" "declare    a=2 inprint    2      "
  case (ret) of
    Right e -> putStrLn "ERRO" >> print e
    Left e -> putStrLn "OK"
  testN <- return (succ testN)

  putStr "teste..." >> print testN
  let ret = parse declare "" "declare    a=2in print    2      "
  case (ret) of
    Right e -> putStrLn "ERRO" >> print e >> putStrLn ""
    Left e -> putStrLn "OK"
  testN <- return (succ testN)

  -- testando assign
  let testN = 1 :: Integer
  putStrLn "\ntestando assign"
  putStr "teste..." >> print testN
  let ret = parse assign "" ""
  case (ret) of
    Right e -> putStrLn "ERRO" >> print e
    Left e -> putStrLn "OK"
  testN <- return (succ testN)

  putStr "teste..." >> print testN
  let ret = parse assign "" "a:=2"
  case (ret) of
    Right e -> if (e == "a:=2")
               then putStrLn "OK" else putStrLn "ERRO - " >> print e >> putStrLn ""
    Left e -> putStrLn "OK"
  testN <- return (succ testN)

  putStr "teste..." >> print testN
  let ret = parse assign "" "a :=2"
  case (ret) of
    Right e -> if (e == "a:=2")
               then putStrLn "OK" else putStrLn "ERRO - " >> print e >> putStrLn ""
    Left e -> putStrLn "OK"
  testN <- return (succ testN)

  putStr "teste..." >> print testN
  let ret = parse assign "" "a:= 2"
  case (ret) of
    Right e -> if (e == "a:=2")
               then putStrLn "OK" else putStrLn "ERRO - " >> print e >> putStrLn ""
    Left e -> putStrLn "OK"
  testN <- return (succ testN)

  putStr "teste..." >> print testN
  let ret = parse assign "" "a := 2"
  case (ret) of
    Right e -> if (e == "a:=2")
               then putStrLn "OK" else putStrLn "ERRO - " >> print e >> putStrLn ""
    Left e -> putStrLn "OK"
  testN <- return (succ testN)

  -- testando while
  let testN = 1 :: Integer
  putStrLn "\ntestando while"
  putStr "teste..." >> print testN
  let ret = parse assign "" ""
  case (ret) of
    Right e -> putStrLn "ERRO" >> print e
    Left e -> putStrLn "OK"
  testN <- return (succ testN)

  putStr "teste..." >> print testN
  let ret = parse assign "" "while (2) do a:=2"
  case (ret) of
    Right e -> if (e == "while (2) do a:=2")
               then putStrLn "OK" else putStrLn "ERRO - " >> print e >> putStrLn ""
    Left e -> putStrLn "OK"
  testN <- return (succ testN)

  -- testando cond
  let testN = 1 :: Integer
  putStrLn "\ntestando cond"
  putStr "teste..." >> print testN
  let ret = parse cond "" ""
  case (ret) of
    Right e -> putStrLn "ERRO"
    Left e -> putStrLn "OK"
  testN <- return (succ testN)

  putStr "teste..." >> print testN
  let ret = parse cond "" "if (2) then a:=2 else a:=3"
  case (ret) of
    Right e -> if (e == "if (2) then a:=2 else a:=3") 
               then putStrLn "OK" else putStrLn "ERRO - " >> print e >> putStrLn ""
    Left e -> putStrLn "ERRO"
  testN <- return (succ testN)
  
  putStr "teste..." >> print testN
  let ret = parse cond "" "if (2) then   a:=2   else   a:=3"
  case (ret) of
    Right e -> if (e == "if (2) then a:=2 else a:=3") 
               then putStrLn "OK" else putStrLn "ERRO - " >> print e >> putStrLn ""
    Left e -> putStrLn "ERRO"
  testN <- return (succ testN)

  putStr "teste..." >> print testN
  let ret = parse cond "" "if (2)then   a:=2   else   a:=3"
  case (ret) of
    Right e -> putStr "ERRO - " >> print e >> putStrLn ""
    Left e -> putStrLn "OK"
  testN <- return (succ testN)

  -- testando programa da especificacao

  putStrLn "\ntestando programa da especificacao"
  let ret = parse com "" "declare x=150 in declare y = 200 in { while x > 0 do { x:= x-1; y := y - 1};print y}"
  case (ret) of
    Right e -> if (e == "declare x=150 in declare y=200 in {while x>0 do {x:=x-1;y:=y-1};print y}")
      then putStrLn "OK" else putStr "ERRO - " >> print e >> putStrLn ""
    Left e -> putStrLn "ERRO"

  return ()
-}
