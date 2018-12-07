module Front
  (
    compute
  ) where

import Text.ParserCombinators.Parsec
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


compute = c' . parse com ""
  where c' (Right v) = print $ interp v
        c' (Left v) = print v

test :: IO ()
test = do
  putStrLn "DEVERIA DAR 50"
  compute "declare x=150 in declare y = 200 in { while x < 300 do { x:= x+1; y := y - 1};print y}"

  putStrLn "\n\nDEVERIA DAR 200"
  compute "declare x=150 in declare y = 200 in { if y = x then { x:= x+1; y := y - 1} else x:=2 ;print y}"

  putStrLn "\n\nDEVERIA DAR 149"
  compute "declare x=150 in declare y = 150 in { if y = x then { x:= x+1; y := y - 1} else x:=2 ;print y}"

  putStrLn "\n\nDEVERIA DAR 300"
  compute "declare x=150 in declare y = 150 in { if y = x then { x:= x+1; y := y - 1} else x:=2 ;print y+x}"

  putStrLn "\n\nDEVERIA DAR 202"
  compute "declare x=150 in declare y = 200 in { if y = x then { x:= x+1; y := y - 1} else x:=2 ;print y+x}"
