{-# LANGUAGE ScopedTypeVariables #-}
import LIO
import LIO.DCLabel
import LIO.LIORef

import DemoHelper
import DemoHelper2

-- How do you run LIO code?
--------------------------------------------------------------------

run :: Example -> IO ()
run lioExample = do
  -- Mint alice's privilege:
  alicePriv <- mintAlicePriv
  -- Execute untrusted LIO example code giving it the privs:
  (res, st) <- tryDCS $ lioExample alicePriv
  -- Print the result and final label:
  case res of
    Left err -> putStrLn $ show (lioLabel st) ++ ">: FAILED: " ++ show err
    Right r  -> putStrLn $ show (lioLabel st) ++ ">: DONE!"


type Example = DCPriv -> DCS ()

-- Current label is the label on everything in scope
--------------------------------------------------------------------

example1 :: Example
example1 _ = do
  let (a, b) = (3, 4)
  lcur <- getLabel
  debug $ show (if True then a else b) ++ " has label " ++ show lcur

-- Current label is raised to allow for permissive reads (part 1)
--------------------------------------------------------------------

example2 :: Example
example2 _ = do
  aliceRef <- newLIORef alice "i like clarice"
  debug "START!"

  a <- readLIORef aliceRef                -- OK!
  debug $ "Read from alice: " ++ show a

-- Current label restricts allocations & writes (part 1)
-- Writing to bob's reference after reading may leak her data.
--------------------------------------------------------------------

example3 :: Example
example3 _ = do
  aliceRef <- newLIORef alice "abolish the state!!"
  bobRef   <- newLIORef bob   "cop cop cop cop cop"
  debug "START!"

  writeLIORef bobRef "bobby"              -- OK!
  debug "Wrote to bob!"                  

  a <- readLIORef aliceRef                -- OK!
  debug $ "Read from alice: " ++ show a

  writeLIORef aliceRef "wonderland"       -- OK!
  debug $ "Wrote to alice!"

  writeLIORef bobRef "digital"            -- FAIL!
  debug "Wrote to bob again!"                  

-- Current label restricts writes allocations & writes (part 2)
-- Privileges allow writing alice's data to bob's reference
--------------------------------------------------------------------

example4 :: Example
example4 alicePriv = do
  aliceRef <- newLIORef alice "abolish the state!!"
  bobRef   <- newLIORef bob   "cop cop cop cop cop"
  debug "START!"

  writeLIORef bobRef "bobby"              -- OK!
  debug "Wrote to bob!"                  

  a <- readLIORef aliceRef                -- OK!
  debug $ "Read from alice: " ++ show a

  writeLIORef aliceRef "wonderland"       -- OK!
  debug $ "Wrote to alice!"

  writeLIORefP alicePriv bobRef "digital" -- NOW OK!
  debug "Wrote to bob again!"                  

-- Current label is raised to allow for permissive reads (part 2):
-- Using privileges to avoid over-tainting the computation
--------------------------------------------------------------------

example5 :: Example -- revised example2
example5 alicePriv = do
  aliceRef <- newLIORef alice "i like clarice"
  debug "START!"

  a <- readLIORefP alicePriv aliceRef             -- OK! (NO TAINT)
  debug $ "Read from alice: " ++ show a

{- 
   Context also has current clearance.

   - Use as a form of discretionary access control: upper bound on
     current label
   - Cannot read data labeled above clearance
   - Cannot allocate objects labeled above the clearance

-}
