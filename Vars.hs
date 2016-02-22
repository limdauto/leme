module Vars where

import Control.Monad
import qualified Control.Monad.Except as E
import Data.IORef
import Data.Maybe as M

import LispData

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = liftM (M.isJust . lookup var) $ readIORef envRef

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- E.liftIO $ readIORef envRef
    maybe (E.throwError $ UnboundVar "Getting an unbound variable" var)
          (E.liftIO . readIORef)
          (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val = do
    env <- E.liftIO $ readIORef envRef
    maybe (E.throwError $ UnboundVar "Setting an unbound variable" var)
          (E.liftIO . (`writeIORef` val))
          (lookup var env)
    return val

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars env bindings = readIORef env >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- E.liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value >> return value
        else E.liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            return value

