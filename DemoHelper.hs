{-# LANGUAGE MultiParamTypeClasses #-}
module DemoHelper where

import LIO.TCB (ioTCB)
import LIO
import LIO.DCLabel

debug str = do
 l <- getLabel
 ioTCB $ putStrLn (show l ++">: "++str)

debugVV str = do
 l <- getLabel
 c <- getClearance
 debug (show l ++","++ show c ++">: "++str)

instance Label CNF where
  lub a b = dcSecrecy $ a %% True `lub` b %% True
  glb a b = dcSecrecy $ a %% True `glb` b %% True
  canFlowTo a b = a %% True `canFlowTo` b %% True

instance PrivDesc CNF CNF where
   downgradeP p l = dcSecrecy $ downgradeP p (l %% True)
