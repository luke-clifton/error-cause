{-# LANGUAGE ScopedTypeVariables #-}

module Error.Cause
    ( E.catchAll
    , Err
    , ErrT
    , Error
    , Except
    , ExceptT
    , Exception(..)
    , SomeException(..)
    , catch
    , catchE
    , catchIOError
    , causes
    , causesE
    , throwE
    , throwM
    , runExceptT
    )
    where

import Control.Monad.Catch (MonadCatch,MonadThrow,throwM, SomeException(..),Exception(..))
import qualified Control.Monad.Catch as E
import Control.Applicative
import Control.Monad.Trans.Except

-- | Type alias for when you are doing `ExceptT (Error a)` a lot.
type ErrT a = ExceptT (Error a)
type Err a = Except (Error a)

-- | The basic error type which collects a chain of errors. Customise the
-- kinds of errors you can create by providing a type for your errors.
-- e.g. `Error MyAppErrors`
data Error e = e `Because` SomeException

newtype WrapErr = WrapErr {unwrapErr :: Error SomeException}
instance Show WrapErr where
    show (WrapErr realErr) = show realErr
instance Exception WrapErr

infixr 0 `Because`

instance Exception e => Show (Error e) where
    show (e `Because` c)
        = displayException e ++ "\ncaused by: " ++ displayException c

instance Exception e => Exception (Error e) where
    toException (e `Because` c) = SomeException (WrapErr (toException e `Because` c))
    fromException se = do
        (e `Because` c) <- unwrapErr <$> fromException se
        e' <- fromException e
        return (e' `Because` c)

fromError :: forall c. (Exception c) => SomeException -> Maybe c
fromError e =
    fromException e
    <|> (fromError' =<< fromException e)
    where
        fromError' :: Error SomeException -> Maybe c
        fromError' (k `Because` e) = fromException k <|> fromError e

-- | Let's you push another error onto the stack.
causes :: (Exception e, MonadCatch m) => m a -> e -> m a
causes a e = do
    ecr <- E.try a
    case ecr of
        Left c -> throwM (e `Because` c)
        Right r -> return r
infixr 0 `causes`

causesE :: (Monad m, Exception e) => ExceptT e m a -> e' -> ExceptT (Error e') m a
causesE a e = withExceptT (\c -> e `Because` toException c) a
infixr 0 `causesE`

catch :: (MonadCatch m, Exception e) => m a -> (e -> m a) -> m a
catch = E.catchJust fromError

catchIOError :: (MonadCatch m) => m a -> (IOError -> m a) -> m a
catchIOError = catch

catchIf :: (MonadCatch m, Exception e) => (e -> Bool) -> m a -> (e -> m a) -> m a
catchIf f a b = a `catch` \e -> if f e then b e else throwM e
