{-# LANGUAGE FlexibleContexts
           , OverloadedStrings
  #-}
module OpSem where

import AST
import Errors

import qualified Control.Monad.Except as M
import qualified Control.Monad.IO.Class()
-- (Recursion magic happens here)
import qualified Data.Functor.Foldable as Rec
-- import qualified Data.Map.Strict as Map
-- import Data.Map.Strict (Map)
import qualified Data.Text as T
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
execute (Global x e) env@(rho, _) =
  do (v, sigma') <- evaluate e env
     let (loc, sigma'') = insertVal v sigma'
         rho' = insertRefTo x loc rho
     return (rho', sigma'')


-- expressions are evaluated
evaluate :: (M.MonadIO m, M.MonadError Error m)
  => Expr -> Env -> m (Value, ValEnv)
evaluate = Rec.fold evaluateF


evaluateF :: (M.MonadIO m, M.MonadError Error m)
  => ExprF (Env -> m (Value, ValEnv))
  -> Env -> m (Value, ValEnv)

-- well boy howdie this one is easy
evaluateF (LiteralF val) (_, sigma) = return (val, sigma)

evaluateF (ReferenceF x) (rho, sigma) =
  case lookupRef x rho of
       Nothing -> M.throwError . ReferencingUndefinedVar $ x
       Just loc ->
         case lookupVal loc sigma of
              Nothing -> M.throwError . InternalInconsistency $
                "A var referenced a location with no value"
              Just Unspecified -> M.throwError ReferencedUnspecified
              Just  val -> return (val, sigma)

evaluateF (SetF x e) env0@(rho, _) =
  do (v, sigma') <- e env0
     case lookupRef x rho of
          Just loc ->
            let sigma'' = mutateVal loc v sigma'
             in return (v, sigma'')
          Nothing -> M.throwError . SettingUndefinedVar $ x

evaluateF (IfF p eT eF) env@(rho, _) =
  do (b, sigma') <- p env
     case b of
          BoolV True -> eT (rho, sigma')
          BoolV False -> eF (rho, sigma')
          _ -> M.throwError NonBoolInIf

evaluateF (WhileF p e) (rho, sigma0) =
  let evalLoop sigma =
        do (b, sigma') <- p (rho, sigma)
           case b of
                BoolV False -> return sigma'
                BoolV True ->
                  do (_, sigma'') <- e (rho, sigma')
                     evalLoop sigma''
                _ -> M.throwError NonBoolInWhile
   in do sigma1 <- evalLoop sigma0
         return (BoolV False, sigma1)

evaluateF (BlockF es) (rho, sigma0) =
  let accum mEnv e =
        do (_,sigma) <- mEnv
           e (rho, sigma)
   in foldl accum (return (BoolV False, sigma0)) es

evaluateF (PrintF e) env =
  do (v, sigma') <- e env
     M.liftIO $ print v
     return (v, sigma')

evaluateF (ApplyF eFunc eArgs) (rho, sigma) =
  let calcArgs sig [] = return (sig, [])
      calcArgs sig (e:es) =
        do (v, sig') <- e (rho, sig)
           (sig1, vs) <- calcArgs sig' es
           return (sig1, v:vs)
   in do (u, sigma0) <- eFunc (rho, sigma)
         case u of
              ClosureV (paramNames, body) lambdaEnv ->
                do (sigma1, vs) <- calcArgs sigma0 eArgs
                   let phi (x, v) (rhoI, sigmaI) =
                         let (loc, sigmaNext) = insertVal v sigmaI
                          in (insertRefTo x loc rhoI, sigmaNext)
                       argPairs = (zip paramNames vs)
                       (rhoF, sigmaF) = foldr phi (lambdaEnv, sigma1) argPairs
                   evaluate body (rhoF, sigmaF)
              PrimitiveV (Primitive f) ->
                do (sigma1, vs) <- calcArgs sigma0 eArgs
                   case f vs of
                        Nothing -> M.throwError ProblemInPrimitive
                        Just v -> return (v, sigma1)
              _ -> M.throwError AppliedNonFunc


evaluateF (LetF LetLet bindings body) (rho, sigma) =
  let calcBinds [] sig = return ([], sig)
      calcBinds ((x,e):binds) sig =
        do (v, sig') <- e (rho, sig)
           (vs, sig1) <- calcBinds binds sig'
           return ((x,v):vs, sig1)
   in do (valBinds, sigma') <- calcBinds bindings sigma
         let addValAt (rh, sig) (x, v) = insertValAndRef x v rh sig
             newEnvs = foldl addValAt (rho, sigma') valBinds
         body newEnvs

evaluateF (LetF LetStar bindings body) (rho0, sigma0) =
  let calcBinds [] rho sigma = return (rho, sigma)
      calcBinds ((x,e):binds) rho sigma =
        do (v, sigma') <- e (rho, sigma)
           let (rho', sigma'') = insertValAndRef x v rho sigma'
           calcBinds binds rho' sigma''
   in do (rho1, sigma1) <- calcBinds bindings rho0 sigma0
         body (rho1, sigma1)


evaluateF (LetF LetRec bindings body) (rho0, sigma0) =
  let -- rho' maps names to new locations.
      -- Sigma is not edited (for now) (except updating next loc)
      -- locToExpMap maps locations to expressions
      (rho', sigma1, locToExprMap)
        = foldl addUnspecifiedBind (rho0, sigma0, []) bindings
      addUnspecifiedBind (rho, sigma, locMap) (x,e) =
        let (loc, sigma') = nextLocation sigma
            rho' = insertRefTo x loc rho
         in (rho', sigma', (loc,e):locMap)

      -- fold down locToExprMap, changing exprs to vals
      calcBinds [] sigma = return ([], sigma)
      calcBinds ((loc,e):binds) sigma =
        do (v, sigma') <- e (rho', sigma)
           (vs, sigma'') <- calcBinds binds sigma'
           return ((loc,v):vs, sigma'')

   in do (locToValMap, sigma2) <- calcBinds locToExprMap sigma1
         -- update all the locations to the computed vals
         let updateVal sig (loc,v) = mutateVal loc v sig
             sigma3 = foldl updateVal sigma2 locToValMap
         body (rho', sigma3)

evaluateF (LetF LetRecStar bindings body) (rho0, sigma0) =
  let -- rho' maps names to new locations.
      -- Sigma is not edited (for now) (except updating next loc)
      -- locToExpMap maps locations to expressions
      (rho', sigma1, locToExprMap)
        = foldl addUnspecifiedBind (rho0, sigma0, []) bindings
      addUnspecifiedBind (rho, sigma, locMap) (x,e) =
        let (loc, sigma') = nextLocation sigma
            rho' = insertRefTo x loc rho
         in (rho', sigma', (loc,e):locMap)

      -- fold down locToExprMap, changing exprs to vals and updating locs
      calcBinds [] sigma = return sigma
      calcBinds ((loc,e):binds) sigma =
        do (v, sigma') <- e (rho', sigma)
           let sigma'' = mutateVal loc v sigma'
           calcBinds binds sigma''

   in do sigma2 <- calcBinds locToExprMap sigma1
         body (rho', sigma2)

evaluateF (LambdaF lambda@(paramNames, _)) (rho, sigma) =
  if paramsUnique paramNames
     then return (ClosureV lambda rho, sigma)
     else M.throwError . ParamsNotUnique . T.pack . show $ paramNames

