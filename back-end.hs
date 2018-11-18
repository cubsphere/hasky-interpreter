data Exp = Constant Int
    | Variable String
    | Plus Exp Exp
    | Minus Exp Exp
    | Greater Exp Exp
    | Times Exp Exp
    deriving Show

data Com =  Assign String Exp
    |  Seq Com Com
    |  Cond Exp Com Com
    |  While Exp Com
    |  Declare String Exp Com
    |  Print Exp
    deriving Show

type SymbolTable = [(String, Int)]

tableset :: String -> Int -> SymbolTable -> SymbolTable
tableset sym new [] = [(sym, new)]
tableset sym new ((tablesym, old):vs) =
    if tablesym == sym
        then ((sym, new):vs)
        else ((tablesym, old):(tableset sym new vs))

tableget :: String -> SymbolTable -> Int
tableget _ [] = 0
tableget sym ((tablesym, val):vs) =
    if tablesym == sym
        then val
        else tableget sym vs

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
    (>>=) (StOut innerfunc) func = (first . (applyToFirst func) . innerfunc) []

unwrap :: M a -> a
unwrap (StOut f) = first $ f []

eval :: Exp -> SymbolTable -> M Int
eval expr table = case expr of
    Constant n -> return n
    Variable x -> return (tableget x table)
    Plus e1 e2 -> return $ unwrap (eval e1 table) + unwrap (eval e2 table)
    Minus e1 e2 -> return $ unwrap (eval e1 table) - unwrap (eval e2 table)
    Greater e1 e2 -> if unwrap (eval e1 table) > unwrap (eval e2 table)
                        then return 1
                        else return 0
    Times e1 e2 -> return $ unwrap (eval e1 table) * unwrap (eval e2 table)

exec :: Com -> SymbolTable -> M SymbolTable
exec stm table = case stm of
    Assign name expr -> return $ tableset name (unwrap $ eval expr table) table --TODO
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
    Declare name expr com -> do
        --special insertion at the front guarantees local variable properties
        table1 <- exec com ((name, unwrap $ eval expr table):table)
        return (removefirst table1)
            where removefirst ((nam, val):vs) = if nam == name
                                              then vs
                                              else (nam,val):(removefirst vs)

    Print expr -> return[] --TODO

output :: Show a => a -> M ()
output v = StOut (\n -> ((), n, show v))