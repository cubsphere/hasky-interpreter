import Text.ParserCombinators.Parsec
import Text.Parsec (ParsecT)
import Text.Parsec.Prim (Stream)
import PolyConc

-- EXPRESSOES
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
digiti = many1 digit

-- <var> ::= <Identifier> que sera letter ++ alphanum*
var = (:) <$> letter <*> many alphaNum

-- <factor> ::= <var> | <digiti> | (<expr>)
factor =
  try var
  <|> try digiti
  <|> (polyConc) <$> (string "(" <* spaces) <*> expr <*> (spaces *> string ")")

-- <term> ::= <term> <mulop> <factor> | <factor>
-- eliminado recursao a' esquerda:
-- <term> ::= <factor> <term'>
-- <term'> ::= <mulop> <factor> <term'> | vazio
term = (++) <$> (factor <* spaces) <*> term'
  where
    term' = try (
      (polyConc) <$> (mulop <* spaces) <*> factor <*> (spaces *> term')
      )
      <|> string ""

-- <expr> ::= <expr> <addop> <term> | <term>

-- <expr> ::=  <term> <expr'>
-- <expr'> ::= <addop> <term> <expr'> | vazio
expr = (++) <$> (term <* spaces) <*> expr'
  where
    expr' = try (
      (polyConc) <$> (addop <* spaces) <*> term <*> (spaces *> expr')
      )
      <|> string ""
  
  
-- <rexp> ::= <rexp> <relop> <expr> | <expr>

-- <rexp> ::= <expr> <rexp'>
-- <rexp'> ::= <relop> <expr> <rexp'> | vazio
rexp = (++) <$> (expr <* spaces) <*> rexp'
  where
    rexp' = try (
      (polyConc) <$> (relop <* spaces) <*> expr <*> (spaces *> rexp')
      )
      <|> string ""
