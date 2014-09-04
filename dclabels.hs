import LIO
import LIO.DCLabel
import LIO.Run (privInit)
import DemoHelper

{- DCLabel consists of two CNF formula: secrecy %% integrity

   For this talk we are only going to use the secrecy components.
  
   Secrecy formula describes the authority required to make the data
   public. I.e., the principal to whom it is sensitive.
-}

-- Some labels we are going to use
--------------------------------------------------------------------

public = toCNF True                -- Public
alice  = toCNF "alice"             -- Alice's secret data
bob    = toCNF "bob.4chan.org"     -- Label of Bob's personal server
iCloud = toCNF "upload.icloud.com" -- Label of iCloud's upload server


-- Alice or iCloud can make data public:
aliceOrIC = alice \/ iCloud

-- Alice and Bob need to authorize the making of this data public:
aliceAndBob = alice /\ bob

-- How do we check if a flow of information is allowed?
--------------------------------------------------------------------

example1 = 
 [ canFlowTo public     alice  == undefined
  -----------------------------------------
 , canFlowTo alice      public == undefined
  -----------------------------------------
 , canFlowTo alice      bob    == undefined
  -----------------------------------------
 , canFlowTo aliceOrIC  iCloud == undefined ]

-- What happens when we combine data? Least Upper Bound!
--------------------------------------------------------------------

example2 = 
 [ canFlowTo alice (alice `lub` bob) == undefined
  -----------------------------------------------
 , canFlowTo (alice `lub` bob) alice == undefined
  -----------------------------------------------
 , (alice `lub` bob)                 == undefined ]

-- How do we encode authority? Privileges.
--------------------------------------------------------------------

{-
   A privilege can be used to remove secrecy restrictions (make data
   more public). This is commonly called declassification.
-}

-- downgradeP: make data as public as possible
--------------------------------------------------------------------

example3 = 
 [ downgradeP alice alice       == undefined
  ------------------------------------------
 , downgradeP alice aliceOrIC   == undefined
  ------------------------------------------
 , downgradeP alice aliceAndBob == undefined ]

-- canFlowToP: more permisive canFlowTo check that uses privileges
--------------------------------------------------------------------

example4 =
 [ canFlowToP alice aliceOrIC  public  == undefined
  -------------------------------------------------
 , canFlowToP alice alice iCloud       == undefined
  -------------------------------------------------
 , canFlowToP alice aliceAndBob alice  == undefined ]

-- I lied a bit
--------------------------------------------------------------------

{-
  
   Previous functions used "privilege descriptions", actual code
   must provide actual privileges.
  
   A privilege is a value of type Priv, which TCB code can "mint" with
   privInit (more on this later)
  
-}

mintAlicePriv :: IO DCPriv
mintAlicePriv = privInit alice
