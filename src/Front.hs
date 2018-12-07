import Text.ParserCombinators.Parsec
import Text.Parsec (ParsecT)
import Text.Parsec.Prim (Stream)
import PolyConc
import Data.Functor.Identity (Identity)

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

-- <digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
-- digit ja esta definido no parsec

-- <digiti> ::= <digit> | <digiti> <digiti>
digiti = many1 digit

-- <var> ::= <Identifier> que sera letter ++ alphanum*
var = (:) <$> letter <*> many alphaNum

-- <factor> ::= <var> | <digiti> | (<expr>)
factor :: ParsecT [Char] u Identity [Char]
factor =
  var
  <|> digiti
  <|> (polyConc) <$> (string "(" <* spaces) <*> expr <*> (spaces *> string ")")
  <?> "factor"

-- <term> ::= <term> <mulop> <factor> | <factor>
-- eliminado recursao a' esquerda:
-- <term> ::= <factor> <term'>
-- <term'> ::= <mulop> <factor> <term'> | vazio
term = (++) <$> (factor) <*> term'
  where
    term' = try (
      (polyConc) <$> (spaces *> mulop <* spaces) <*> factor <*> term'
      )
      <|> string ""
      <?> "term"

-- <expr> ::= <expr> <addop> <term> | <term>

-- <expr> ::=  <term> <expr'>
-- <expr'> ::= <addop> <term> <expr'> | vazio
expr = (++) <$> (term) <*> expr'
  where
    expr' = try (
      (polyConc) <$> (spaces *> addop <* spaces) <*> term <*> expr'
      )
      <|> string ""
      <?> "expr"
  
  
-- <rexp> ::= <rexp> <relop> <expr> | <expr>

-- <rexp> ::= <expr> <rexp'>
-- <rexp'> ::= <relop> <expr> <rexp'> | vazio
rexp = (++) <$> (expr) <*> rexp'
  where
    rexp' = try (
      (polyConc) <$> (spaces *> relop <* spaces) <*> expr <*> rexp'
      )
      <|> string ""
      <?> "rexp"


-- COMANDOS
-- <printe> ::= print <rexp>
printe = (++) <$> string "print" <*> aux
  where aux = (polyConc) <$> space <* spaces <*> rexp

-- <declare> ::= declare <identif> = <rexp> in <com>
-- <identif> sera meu <var>
declare = (++) <$> string "declare" <*> aux
  where
    aux = (polyConc) <$> space <* spaces <*> var <*> (spaces *> string "=" <* spaces) <*> rexp <*> (space <* spaces) <*> string "in" <*> (space <* spaces) <*> com

-- <while> := while <rexp> do <com>
while = (++) <$> string "while" <*> aux
  where
    aux = (polyConc) <$> space <* spaces <*> rexp <*> (space <* spaces) <*> string "do" <*> (space <* spaces) <*> com

-- <cond> := if <rexp> then <com> else <com>
cond = (++) <$> string "if" <*> aux
  where
    aux = (polyConc) <$> space <* spaces <*> rexp <*> (space <* spaces) <*> string "then" <*> (space <* spaces) <*> com <*> (space <* spaces) <*> string "else" <*> (space <* spaces) <*> com

-- <seqv> ::= { <com> ; <com> }
seqv = (:) <$> char '{' <* spaces <*> aux
  where
    aux = (polyConc) <$> com <*> (spaces *> char ';' <* spaces) <*> com <*> (spaces *> char '}')

-- <assign> ::= <identif> := <rexp>
assign = (++) <$> var <* spaces <*> aux
  where aux = (polyConc) <$> (string ":=" <* spaces) <*> rexp

-- <com> ::= <assign> | <seqv> | <cond> | <while> | <declare> | <printe>
com = try assign <|> try seqv <|> try cond <|> try while <|> try declare <|> printe <?> "com"


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
