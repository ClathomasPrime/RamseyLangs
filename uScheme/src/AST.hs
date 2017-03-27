{-# LANGUAGE TypeFamilies, FlexibleContexts, DeriveFunctor, TemplateHaskell
           , DeriveFoldable, DeriveTraversable, Rank2Types, LambdaCase
  #-}
module AST where

import Errors

import qualified Data.Text as T
import qualified Control.Monad.Except as M
import qualified Control.Monad.IO.Class as M
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Functor.Foldable.TH as Rec -- (Recursion magic)


--
-- Environments
--
type Location = Int
type RefEnv = Map VarName Location
type ValEnv = (Location, Map Location Value)
type Env = (RefEnv, ValEnv)
emptyEnv :: Env
emptyEnv = (Map.empty, (0, Map.empty))

nextLocation :: ValEnv -> (Location, ValEnv)
nextLocation (loc, store)
  = (loc, (loc+1, store))

lookupRef :: VarName -> RefEnv -> Maybe Location
lookupRef = Map.lookup

insertRefTo :: VarName -> Location -> RefEnv -> RefEnv
insertRefTo = Map.insert

lookupVal :: Location -> ValEnv -> Maybe Value
lookupVal loc (nextLoc, store) = Map.lookup loc store

insertVal :: Value -> ValEnv -> (Location, ValEnv)
insertVal v (loc, store) = (loc, (loc+1, Map.insert loc v store))

mutateVal :: Location -> Value -> ValEnv -> ValEnv
mutateVal loc v (nextLoc, store) = (nextLoc, Map.insert loc v store)

insertValAndRef :: VarName -> Value -> RefEnv -> ValEnv -> (RefEnv, ValEnv)
insertValAndRef x v rho sigma =
  let (loc, sigma') = insertVal v sigma
      rho' = insertRefTo x loc rho
   in (rho', sigma')


--
-- Values
--
type VarName = T.Text

type ParamList = [VarName]
type Lambda = (ParamList, Expr)

data Value
  = Unspecified
  | NilV
  | BoolV Bool
  | NumV Integer
  | SymV T.Text
  | PairV Value Value
  | ClosureV Lambda RefEnv
  | PrimitiveV Primitive
  deriving(Show)

newtype Primitive
  = Primitive { getAction :: [Value] -> Maybe Value }

instance Show Primitive where
  show _ = "<primitive>"


--
-- Definitions
--
type Program = [Definition]

data Definition
  = Global VarName Expr
  deriving(Show)

--
-- Expressions
--
data Expr
  = Literal Value
  | Reference VarName
  | Set VarName Expr
  | If Expr Expr Expr
  | While Expr Expr
  | Block [Expr]
  | Print Expr
  | Apply Expr [Expr]
  | Let LetType [(VarName, Expr)] Expr
  | Lambda Lambda
  deriving(Show)

data LetType = LetLet | LetStar | LetRec | LetRecStar
  deriving(Show)

Rec.makeBaseFunctor ''Expr


arithOp :: (Integer -> Integer -> Integer) -> Primitive
arithOp f = Primitive phi
  where phi [NumV n, NumV m] = Just . NumV $ n `f` m
        phi _ = Nothing

cmpOp :: (Integer -> Integer -> Bool) -> Primitive
cmpOp f = Primitive phi
  where phi [NumV n, NumV m] = Just . BoolV $ n `f` m
        phi _ = Nothing


builtins :: Map VarName Primitive
builtins =
  Map.fromList . map (\(s,f) -> (T.pack s, f)) $
    [ ("+"    , arithOp (+) )
    , ("-"    , arithOp (-) )
    , ("*"    , arithOp (*) )
    , ("/"    , arithOp div )
    , ("<"    , cmpOp (<) )
    , ("<="   , cmpOp (<=) )
    , (">"    , cmpOp (>) )
    , (">="   , cmpOp (>=) )
    , ("="    , Primitive $ \case { [NumV n, NumV m] -> Just . BoolV $ n==m ; _ -> Nothing })
    , ("!="   , Primitive $ \case {[NumV n, NumV m] -> Just . BoolV $ n/=m ; _ -> Nothing })
    , ("negate",Primitive $  \case { [NumV n] -> Just . NumV $ -n ; _ -> Nothing })
    , ("abs"  , Primitive $ \case { [NumV n] -> Just . NumV $ abs n ; _ -> Nothing })
    ]

defaultEnv = builtins
