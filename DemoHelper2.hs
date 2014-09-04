module DemoHelper2 where

import LIO
import LIO.DCLabel
import LIO.Run

mintAlicePriv :: IO DCPriv
mintAlicePriv = privInit alice

type DCS = LIO CNF

tryDCS :: DCS a -> IO (Either SomeException a, LIOState CNF)
tryDCS dc = tryLIO dc LIOState { lioLabel     = public
                               , lioClearance = toCNF False }

public = toCNF True                -- Public
alice  = toCNF "alice"             -- Alice's secret data
bob    = toCNF "bob.4chan.org"     -- Label of Bob's personal server
iCloud = toCNF "upload.icloud.com" -- Label of iCloud's upload server
aliceOrBob = alice \/ bob
aliceAndBob = alice /\ bob
