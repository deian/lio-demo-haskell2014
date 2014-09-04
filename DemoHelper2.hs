module DemoHelper2 where

import LIO
import LIO.DCLabel
import LIO.Run

public = toCNF True    -- Public
alice  = toCNF "alice" -- Alice can make data public
bob    = toCNF "bob"   -- Bob can make data public

aliceOrBob = "alice" \/ "bob"

aliceAndBob = "alice" /\ "bob"

mintAlicePriv :: IO DCPriv
mintAlicePriv = privInit alice

type DCS = LIO CNF

tryDCS :: DCS a -> IO (Either SomeException a, LIOState CNF)
tryDCS dc = tryLIO dc LIOState { lioLabel     = public
                               , lioClearance = toCNF False }
