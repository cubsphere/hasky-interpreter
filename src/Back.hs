module Back
  (
    Exp (Constant, Variable, Plus, Minus, Greater, Less, Equal, Times, Divide, Empty)
  , Com (Assign, Seq, Cond, While, Declare, Print)
  ) where

data Exp = Constant Int
    | Variable String
    | Plus Exp Exp
    | Minus Exp Exp
    | Greater Exp Exp
    | Less Exp Exp
    | Equal Exp Exp
    | Times Exp Exp
    | Divide Exp Exp
    | Empty
    deriving Show

data Com =  Assign Exp Exp
    |  Seq Com Com
    |  Cond Exp Com Com
    |  While Exp Com
    |  Declare Exp Exp Com
    |  Print Exp
    deriving Show

type SymbolTable = [(String, Int)]

tableset :: Exp -> Int -> SymbolTable -> SymbolTable
tableset s@(Variable sym) new [] = [(sym, new)]
tableset s@(Variable sym) new ((tablesym, old):vs) =
    if tablesym == sym
        then ((sym, new):vs)
        else ((tablesym, old):(tableset s new vs))

tableget :: Exp -> SymbolTable -> Int
tableget _ [] = 0
tableget s@(Variable sym) ((tablesym, val):vs) =
    if tablesym == sym
        then val
        else tableget s vs

--define this as a monad
newtype M a = StOut (SymbolTable -> (a ,  SymbolTable , String))

unStOut :: M a -> SymbolTable -> (a, SymbolTable, String)
unStOut (StOut f) = f

applyToFirst :: (a->a1) -> (a,b,c) -> (a1,b,c)
applyToFirst f (a,b,c) = (f a, b, c)

instance Functor M where
    fmap func (StOut innerfunc) = StOut (applyToFirst func . innerfunc)

compose func1 func2 table = (resfunc resval, restable, resstring)
    where (resval, restable, resstring) = func2 table
          (resfunc,_,_) = func1 table

instance Applicative M where
    pure a = StOut (\x -> (a, x, []))
    (<*>) (StOut func1) (StOut func2) = StOut(compose func1 func2)

first :: (a,b,c) -> a
first (a,_,_) = a

instance Monad M where
    return a = pure a
    (>>=) (StOut innerfunc) func = StOut (\x ->
        let (r1, s1, o1) = innerfunc x
            StOut(f) = func r1
            (r2, s2, o2) = f s1
        in (r2, s2, o1 ++ o2))


unwrap :: M a -> a
unwrap (StOut f) = first $ f []

eval :: Exp -> SymbolTable -> M Int
eval expr table = case expr of
    Constant n -> return n
    v@(Variable x) -> return (tableget v table)
    Plus e1 e2 -> return $ unwrap (eval e1 table) + unwrap (eval e2 table)
    Minus e1 e2 -> return $ unwrap (eval e1 table) - unwrap (eval e2 table)
    Greater e1 e2 -> if unwrap (eval e1 table) > unwrap (eval e2 table)
                        then return 1
                        else return 0
    Times e1 e2 -> return $ unwrap (eval e1 table) * unwrap (eval e2 table)

exec :: Com -> SymbolTable -> M SymbolTable
exec stm table = case stm of
    Assign name expr -> return $ tableset name (unwrap $ eval expr table) table
    Seq com1 com2 -> do
        table1 <- exec com1 table
        table2 <- exec com2 table1
        return table2
    Cond expr com1 com2 ->
        if 0 /= (unwrap $ eval expr table)
        then exec com1 table
        else exec com2 table
    While expr com -> do
        if 0 /= (unwrap $ eval expr table)
        then exec stm (unwrap $ exec com table)
        else return table
    Declare (Variable name) expr com -> do
        --special insertion at the front guarantees local variable properties
        table1 <- exec com ((name, unwrap $ eval expr table):table)
        return (removefirst table1)
            where removefirst ((nam, val):vs) = if nam == name
                                              then vs
                                              else (nam,val):(removefirst vs)
    Print expr -> StOut (\st -> ([], st, (show $ unwrap $ eval expr table) ++ "\n"))

test :: String
test = let
    StOut f = exec (Seq (Assign (Variable "x") (Plus (Constant 5)  (Constant 2))) (Print (Variable "x"))) []
    (_,_,str) = (f [])
    in str
