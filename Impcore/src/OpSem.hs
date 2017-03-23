{-# LANGUAGE FlexibleContexts
  #-}
module OpSem where

import AST
import Errors

import qualified Control.Monad.Except as M
import qualified Control.Monad.IO.Class as M
-- (Recursion magic happens here)
import qualified Data.Functor.Foldable as Rec
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.List as List


paramsUnique :: ParamList -> Bool
paramsUnique p = p == List.nub p -- uuuugly


executeProg :: (M.MonadIO m, M.MonadError Error m)
  => Program -> Env -> m Env
executeProg = Rec.fold executeDefList . reverse
  where executeDefList :: (M.MonadIO m, M.MonadError Error m)
          => Rec.ListF Definition (Env -> m Env) -> Env -> m Env
        executeDefList Rec.Nil env = return env
        executeDefList (Rec.Cons def prevDefs) env =
          execute def =<< prevDefs env

-- definitions are executed
execute :: (M.MonadIO m, M.MonadError Error m)
  => Definition -> Env -> m Env
execute (Global x e) env =
  do (v, Env glob func param) <- evaluate e env
     let glob' = Map.insert x v glob
     return $ Env glob' func param
execute (Function f paramList e) (Env glob func param) =
  do if paramsUnique paramList then return ()
        else M.throwError $ ErrorString "nonunique paramater names"
     let func' = Map.insert f (UserFunc paramList e) func
     return $ Env glob func' param


-- expressions are evaluated
evaluate :: (M.MonadIO m, M.MonadError Error m)
  => Expr -> Env -> m (Value, Env)
evaluate = Rec.fold evaluateF

evaluateF :: (M.MonadIO m, M.MonadError Error m)
  => ExprF (Env -> m (Value, Env)) -> Env -> m (Value, Env)

evaluateF (IfF p eT eF) env =
  do (b, env') <- p env
     if b /= 0
         then eT env'
         else eF env'

evaluateF (WhileF p e) env0 =
  let evalLoop env =
        do (b, env') <- p env
           if b == 0
              then return env'
              else do (_, env'') <- e env'
                      evalLoop env''
   in fmap (\env1 -> (0, env1)) (evalLoop env0)

evaluateF (BlockF []) env =
  return (0, env)
evaluateF (BlockF es) env0 =
  let evalBlock [e] env = e env
      evalBlock (e:es) env =
        do (_, env') <- e env
           evalBlock es env'
   in evalBlock es env0

evaluateF (PrintF e) env =
  do (v, env') <- e env
     M.liftIO $ print v
     return (v, env')

evaluateF (LocalF x e) env0 =
  do (v, Env glob func param) <- e env0
     let param' = Map.insert x v param
     return (v, Env glob func param')

evaluateF (SetF x e) env0 =
  do (v, Env glob func param) <- e env0
     let result
           | x `Map.member` param =
             let param' = Map.adjust (const v) x param
              in return (v, Env glob func param')
           | x `Map.member` glob =
             let glob' = Map.adjust (const v) x glob
              in return (v, Env glob' func param)
           | otherwise =
             M.throwError . ErrorString $ "undefined var " ++ x ++ " is Set"
     result

evaluateF (ApplyF fName argList) env0@(Env glob func param)
  | Just function <- Map.lookup fName func =
    do let evalArgs [] env = return ([], env)
           evalArgs (arg:args) env =
             do (v, env') <- arg env
                (vs, env'') <- evalArgs args env'
                return (v:vs, env'')
       (args, env'@(Env glob' _ param')) <- evalArgs argList env0
       case function of
            BuiltinFunc f ->
              case f args of
                   Nothing -> M.throwError . ErrorString
                     $ "wrong number of args to " ++ fName
                   Just n -> return (n, env')
            UserFunc argNames e ->
              do let argMap = Map.fromList $ zip argNames args
                 (v, Env globFinal _ _) <- evaluate e (Env glob' func argMap)
                 return (v, Env globFinal func param')
  | otherwise = M.throwError . ErrorString
    $ "undefined function " ++ fName ++ "is referenced"

evaluateF (ReferenceF x) env@(Env glob func param)
  | Just v <- Map.lookup x param = return (v, env)
  | Just v <- Map.lookup x glob = return (v, env)
  | otherwise = M.throwError $ ErrorString "undefined var is referenced"

-- well boy howdie this one is easy
evaluateF (LiteralF n) env = return (n, env)
