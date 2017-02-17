{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | This module provides an example of using the *error-cause* package.
module Error.Cause.Tutorial where

import Error.Cause
import Control.Monad

newtype OpaqueError = OpaqueError { toString :: String }

instance Show OpaqueError where
    show (OpaqueError e) = "error: " ++ e

instance Exception OpaqueError

readDb :: FilePath -> IO String
readDb db = do
    d <- readFile db `causes` OpaqueError "Couldn't open the database"
    (12,value) <- return (read d) `causes` OpaqueError "The database is corrupt"
    return value

example :: IO ()
example = do
    putStrLn "Attempting /not/a/File!"
    putStrLn =<< catchAll (readDb "/not/a/File!") (return . displayException)

    putStrLn "\nAttempting /dev/null"
    putStrLn =<< catchAll (readDb "/dev/null") (return . displayException)

    putStrLn "\nAttempting /not/a/File! and catching IOErrors" 
    putStrLn =<< catchIOError (readDb "/not/a/File!") (return . displayException)

    putStrLn "\nAttempting /dev/null and catching IOErrors"
    putStrLn =<< catchIOError (readDb "/dev/null") (return . displayException)

data MyError
    = ErrorA
    | ErrorB
    | ErrorC
    deriving Show
instance Exception MyError

eitherExample :: Int -> Except OpaqueError Int
eitherExample i = do
    when (i < 0) (throwE $ OpaqueError "Negative input")
    return i

iThrowStuff :: Except (Error MyError) Int
iThrowStuff = eitherExample (-1) `causesE` ErrorA

-- `Err a` is a type alias for `Except (Error a)`
iMightThrowStuff :: Int -> Err MyError Int
iMightThrowStuff i = eitherExample i `causesE` ErrorB

eitherStuff :: Int -> Err MyError String
eitherStuff i = do
    s1 <- catchE (show <$> iThrowStuff) (return . displayException)
    s2 <- catchE (show <$> iMightThrowStuff i) (return . displayException)
    iThrowStuff `causesE` ErrorC
    return (s1 ++ "||" ++ s2)
