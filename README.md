# Error Causes

This library lets you add extra information to your exceptions and errors.

```haskell
-- Of course, you would have a better type.
newtype OpaqueError = OpaqueError { toString :: String }

instance Show OpaqueError where
    show (OpaqueError e) = "error: " ++ e

instance Exception OpaqueError

-- An IO action that can fail in a variety of ways.
readDb :: FilePath -> IO String
readDb db = do
    -- We take the IO error likely to occur in readFile and say that
    -- it causes another error.
    readFile db `causes` OpaqueError "Couldn't open the database"
    undefined `causes` OpaqueError "Database not implemented yet"

example :: IO ()
example = do
    putStrLn "Attempting /not/a/File!"
    putStrLn =<< catchAll (readDb "/not/a/File!") (return . displayException)

    putStrLn "\nAttempting /dev/null"
    putStrLn =<< catchAll (readDb "/dev/null") (return . displayException)

    -- We can catch errors up the chain if we need to.
    putStrLn "\nAttempting /not/a/File! and catching IOErrors" 
    putStrLn =<< catchIOError (readDb "/not/a/File!") (return . displayException)

    -- This will blow up, as it should, because we are only catching IOExceptions
    -- but this throws something else.
    putStrLn "\nAttempting /dev/null and catching IOErrors"
    putStrLn =<< catchIOError (readDb "/dev/null") (return . displayException)
```

If run in GHCi this program will generate the following output.

```
Attempting /not/a/File!
error: Couldn't open the database
caused by: /not/a/File!: openFile: does not exist (No such file or directory)

Attempting /dev/null
error: Database not implemented yet
caused by: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at src/Error/Cause/Tutorial.hs:19:5 in error-cause-0.1.0.0-C15OURfDzdKBy1ektzRstP:Error.Cause.Tutorial

Attempting /not/a/File! and catching IOErrors
/not/a/File!: openFile: does not exist (No such file or directory)

Attempting /dev/null and catching IOErrors
*** Exception: error: Database not implemented yet
caused by: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at src/Error/Cause/Tutorial.hs:19:5 in error-cause-0.1.0.0-C15OURfDzdKBy1ektzRstP:Error.Cause.Tutorial
```

It is also usable with `ExceptT` stacks.
