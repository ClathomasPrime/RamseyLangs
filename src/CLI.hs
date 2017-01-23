{-# LANGUAGE FlexibleContexts
  #-}
module CLI where

import AST
import Parse
import OpSem
import Errors

import qualified System.IO as IO
import qualified System.Environment as Arg
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Control.Monad.Except as M
import qualified Control.Monad.IO.Class as M
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)


usage :: String
usage =
     "usage:\n"
  ++ "  impcore --help : display this help message\n"
  ++ "  impcore --repl : enter the repl\n"
  ++ "  impcore FILENAME : load and evaluate FILENAME\n"
  ++ "  impcore --repl FILENAME : load and evaluate FILENAME, then enter the repl\n"

impcore :: IO ()
impcore =
  do args <- Arg.getArgs
     case args of
          "--repl":fileName:args' -> outputM $ impcoreReplWith fileName args'
          ["--repl"] -> outputM $ impcoreRepl 0 defaultEnv
          ["--help"] -> putStr usage
          fileName:args' -> outputM $ impcoreInterpret fileName args'
          _ -> putStr usage

outputM :: M.ExceptT Error IO a -> IO ()
outputM m =
  do res <- M.runExceptT m
     case res of
          Left e -> print e
          Right a -> return ()

impcoreInterpret :: (M.MonadIO m, M.MonadError Error m)
  => FilePath -> [String] -> m ()
impcoreInterpret file args =
  do source <- M.liftIO $ T.readFile file
     case parseProgram file source of
          Left err -> M.liftIO $ print err
          Right prog ->
            interpretProg prog (fmap read args) defaultEnv >> return ()

interpretProg :: (M.MonadIO m, M.MonadError Error m)
  => Program -> [Integer] -> Env -> m (Value, Env)
interpretProg prog args env =
  do Env glob func _ <- executeProg prog env
     case Map.lookup "main" func of
          Just (UserFunc params e) ->
            do let paramMap = Map.fromList $ zip params (args ++ repeat 0)
               (v, env') <- evaluate e $ Env glob func paramMap
               -- M.liftIO $ print v
               return (v, env')
          _ -> M.throwError $ ErrorString "no main found"

impcoreRepl :: (M.MonadIO m, M.MonadError Error m)
  => Int -> Env -> m ()
impcoreRepl n env =
  do let prompt = "repl:" ++ show n
     M.liftIO $ putStr (prompt ++ "> ") >> IO.hFlush IO.stdout
     lineText <- M.liftIO $ T.getLine
     let lineParse = parseReplLine prompt lineText
     case lineParse of
          Left err -> M.liftIO (print err) >> impcoreRepl (n+1) env
          Right (Left def) ->
            do env' <- execute def env
               -- M.liftIO $ print env'
               impcoreRepl (n+1) env'
          Right (Right exp) ->
            (do (v, Env glob func param) <- evaluate exp env
                M.liftIO $ print v -- >> print (Env glob func param)
                let glob' = Map.insert "it" v glob
                impcoreRepl (n+1) (Env glob' func param)
            ) `M.catchError`
              (\e ->
                do M.liftIO $ print e
                   impcoreRepl (n+1) env
              )

impcoreReplWith :: (M.MonadIO m, M.MonadError Error m)
  => FilePath -> [String] -> m ()
impcoreReplWith file args =
  do source <- M.liftIO $ T.readFile file
     case parseProgram file source of
          Left err -> M.liftIO $ print err
          Right prog ->
            do (v, env) <- interpretProg prog (fmap read args) defaultEnv
               impcoreRepl 0 env
