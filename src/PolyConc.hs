{-# LANGUAGE FlexibleInstances #-}
module PolyConc (polyConc) where

class PolyConcType a where
  polyConc' :: String -> a
instance PolyConcType String where
  polyConc' = id
instance PolyConcType x => PolyConcType (Char -> x) where
  polyConc' a x = polyConc' (a ++ [x])
instance PolyConcType x => PolyConcType (Bool -> x) where
  polyConc' a x = polyConc' (a ++ show x)
instance PolyConcType x => PolyConcType (String -> x) where
  polyConc' a x = polyConc' (a ++ x)

polyConc :: (PolyConcType x) => x
polyConc = polyConc' ""
