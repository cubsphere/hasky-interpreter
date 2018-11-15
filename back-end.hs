data Exp = Constant Int
    | Variable String
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

tableget :: String -> SymbolTable -> Int
tableget sym ((id, val):vs) =
    if id == sym
        then val
        else tableget sym vs

--define this as a monad
newtype M a = StOut (SymbolTable -> (a ,  SymbolTable , String))

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

instance Monad M where
    return a = pure a
    (>>=) (StOut innerfunc) func = (first . (applyToFirst func) . innerfunc) []
        where first (a,_,_) = a

eval :: Exp -> SymbolTable -> M Int
eval exp table = case exp of
    Constant n -> return n
    Variable x -> return (tableget x table)
    Minus e1 e2 -> return 0 --TODO
    Greater e1 e2 -> return 0 --TODO
    Times e1 e2 -> return 0 --TODO

exec :: Com -> SymbolTable -> M ()
exec stm table = case stm of
    Assign name exp -> return () --TODO
    Seq stm1 stm2 -> return () --TODO
    Cond exp com1 com2 -> return() --TODO
    While exp com -> return() --TODO
    Declare name exp stm -> return() --TODO
    Print exp -> return() --TODO

output :: Show a => a -> M ()
output v = StOut (\n -> ((), n, show v))