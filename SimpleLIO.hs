{-# LANGUAGE Trustworthy #-}
module SimpleLIO (LIO, tryLIO, raiseLabel, getLabel,
                  LIORef, writeLIORef, readLIORef, newLIORef,
                  writeLIORefP, readLIORefP,
                  debug ) where

-- Imports
import Control.Monad
import Control.Applicative
import LIO.Label
import LIO.DCLabel
import Data.IORef
import DemoHelper ()
import DemoHelper2 hiding (tryDCS, DCS)
import Control.Exception

debug str = do
 l <- getLabel
 ioTCB $ putStrLn (show l ++">: "++str)

{- LIO monad is used to define secure safe Haskell sublanguage.
  
   All untrusted code executes in LIO monad.

   Monad keeps track of a current label:

   - Refects sensitivity of data read by the computation.
   - Restricts writes to objects that are at least as
     sensitive---i.e., current label must flow to label of object.
-}

newtype LIO a = LIOTCB (IORef CNF -> IO a) -- Must not be exported!

-- Get the current label
getLabel :: LIO CNF
getLabel = LIOTCB readIORef

-- Set the current label. Only safe to raise.
raiseLabel :: CNF -> LIO ()
raiseLabel l = LIOTCB $ \r -> 
  modifyIORef r (\lcur -> lcur `lub` l)

-- How do we execute untrusted code?
----------------------------------------------------------------
tryLIO :: LIO a -> IO (Either SomeException a, CNF)
tryLIO (LIOTCB act) = do
  s  <- newIORef public
  ea <- try $ act s 
  l  <- readIORef s
  return (ea, l)

-- Trusted code may want to lift IO actions into LIO
ioTCB :: IO a -> LIO a
ioTCB = LIOTCB . const

instance Monad LIO where
  return = LIOTCB . const . return

  (LIOTCB ma) >>= k = LIOTCB $ \s -> do
    a <- ma s
    case k a of LIOTCB mb -> mb s
  fail = LIOTCB . const . fail

instance Functor LIO where
  fmap f (LIOTCB a) = LIOTCB $ \s -> a s >>= return . f

instance Applicative LIO where
  pure = return
  (<*>) = ap

-- Labeled IORefs
----------------------------------------------------------------
data LIORef a = LIORefTCB CNF (IORef a)

writeLIORef :: LIORef a -> a -> LIO ()
writeLIORef (LIORefTCB l r) x = do
  lcur <- getLabel
  if lcur `canFlowTo` l
    then ioTCB $ writeIORef r x
    else fail "write may leak data"
  
readLIORef :: LIORef a -> LIO a
readLIORef (LIORefTCB l r) = do
  raiseLabel l
  ioTCB $ readIORef r

newLIORef :: CNF -> a -> LIO (LIORef a)
newLIORef l x = do
  lcur <- getLabel
  if lcur `canFlowTo` l
    then do r <- ioTCB $ newIORef x
            return $ LIORefTCB l r
    else fail "allocation may leak data"

-- Privileged LIORef interface 
----------------------------------------------------------------

writeLIORefP :: DCPriv -> LIORef a -> a -> LIO ()
writeLIORefP priv (LIORefTCB l r) x = do
  lcur <- getLabel
  if canFlowToP priv lcur l
    then ioTCB $ writeIORef r x
    else fail "write may leak data"
  
readLIORefP :: DCPriv -> LIORef a -> LIO a
readLIORefP priv (LIORefTCB l r) = do
  raiseLabel (downgradeP priv l)
  ioTCB $ readIORef r
