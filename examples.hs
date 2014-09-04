{-# LANGUAGE Safe #-}
import SimpleLIO
import LIO.DCLabel
import DemoHelper2 hiding (tryDCS, DCS)

-- How we're going to run the examples
run :: Example -> IO ()
run lioExample = do
  -- Mint alice's privilege:
  alicePriv <- mintAlicePriv
  -- Execute untrusted LIO example code giving it the privs:
  (res, st) <- tryLIO $ lioExample alicePriv
  -- Print the result and final label:
  case res of
    Left err -> putStrLn $ show st ++ ">: FAILED: " ++ show err
    Right r  -> putStrLn $ show st ++ ">: DONE!"

type Example = DCPriv -> LIO ()

-- Current label is raised to allow for permissive reads (part 1)
--------------------------------------------------------------------

example1 :: Example
example1 _ = do
  aliceRef <- newLIORef alice "my private photo"
  debug "START!"

  a <- readLIORef aliceRef                -- OK!
  debug $ "Read alice's data: " ++ show a -- WAIT, WHAT!?

-- Current label restricts writes (part 1)
-- Writing to 4chan reference after reading may leak her photo.
--------------------------------------------------------------------

example2 :: Example
example2 _ = do
  aliceRef <- newLIORef alice "my private photo"
  bobRef   <- newLIORef bob   "--"
  debug "START!"

  writeLIORef bobRef "Get PDF rederer spec" -- OK!
  debug "Wrote to bob's 4chan!"                  

  a <- readLIORef aliceRef                  -- OK!
  debug $ "Read alice's data: " ++ show a

  writeLIORef bobRef ("leak: " ++ show a)   -- FAIL!
  debug "Leaked photo to bob!"

-- Current label restricts writes (part 2)
-- Privileges allow writing alice's data to bob's reference
--------------------------------------------------------------------

example3 :: Example
example3 alicePriv = do
  aliceRef <- newLIORef alice "my private photo"
  icRef    <- newLIORef iCloud "--"
  debug "START!"

  a <- readLIORef aliceRef                               -- OK!
  debug $ "Read alice's data: " ++ show a

  writeLIORefP alicePriv icRef ("uploading: " ++ show a) -- OK!
  debug "Uploaded photo to iCloud!"
