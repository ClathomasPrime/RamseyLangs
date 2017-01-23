{-# LANGUAGE TypeFamilies, FlexibleContexts, DeriveFunctor, TemplateHaskell
           , DeriveFoldable, DeriveTraversable, Rank2Types, LambdaCase
  #-}
module AST where

import Errors

import qualified Control.Monad.Except as M
import qualified Control.Monad.IO.Class as M
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Functor.Foldable.TH as Rec -- (Recursion magic)



-- These should probably be improved upon
type VarName = String
type Value = Integer
type ParamList = [VarName] -- this one might be okay


data Func
  = BuiltinFunc ([Value] -> Maybe Value)
  | UserFunc ParamList Expr

instance Show Func where
  show (BuiltinFunc f) = "<builtin>"
  show (UserFunc ps e) = "UserFunc " ++ show ps ++ show e


type Program = [Definition]

data Definition
  = Global VarName Expr
  | Function VarName ParamList Expr
  deriving(Show)

data Expr
  = If Expr Expr Expr
  | While Expr Expr
  | Block [Expr]
  | Print Expr
  | Local VarName Expr
  | Set VarName Expr
  | Apply VarName [Expr]
  | Reference VarName
  | Literal Value
  deriving(Show)

Rec.makeBaseFunctor ''Expr

data Env = Env
  { getGlobals :: Map VarName Value
  , getFunctions :: Map VarName Func
  , getParams :: Map VarName Value
  } deriving(Show)

emptyEnv = Env Map.empty Map.empty Map.empty

defaultEnv = Env Map.empty builtins Map.empty

builtins :: Map VarName Func
builtins =
  Map.fromList . map (\(s,f) -> (s, BuiltinFunc f)) $
    [ ("+", \case { [n, m] -> Just $ n+m ; _ -> Nothing })
    , ("-", \case { [n, m] -> Just $ n-m ; _ -> Nothing })
    , ("*", \case { [n, m] -> Just $ n*m ; _ -> Nothing })
    , ("/", \case { [n, m] -> Just $ n`div`m ; _ -> Nothing })
    , ("<", \case { [n, m] -> Just . conv $ n<m ; _ -> Nothing })
    , ("<=", \case { [n, m] -> Just . conv $ n<=m ; _ -> Nothing })
    , (">", \case { [n, m] -> Just . conv $ n>m ; _ -> Nothing })
    , (">=", \case { [n, m] -> Just . conv $ n>=m ; _ -> Nothing })
    , ("=", \case { [n, m] -> Just . conv $ n==m ; _ -> Nothing })
    , ("!=", \case { [n, m] -> Just . conv $ n/=m ; _ -> Nothing })
    , ("negate", \case { [n] -> Just $ -n ; _ -> Nothing })
    , ("abs", \case { [n] -> Just $ abs n ; _ -> Nothing })
    ]
  where conv = fromIntegral . fromEnum
