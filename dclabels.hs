import LIO
import LIO.DCLabel
import LIO.Run (privInit)
import DemoHelper

{- 
   DCLabel consists of two formula: secrecy %% integrity
  
   secrecy: describes the authority required to make 
            the data public.
  
   integrity: describes the authority with which
              data was endorsed/is required to modify.

   For this talk we are only going to use the secrecy components.
-}

-- Some labels
--------------------------------------------------------------------

public = toCNF True    -- Public
alice  = toCNF "alice" -- Alice can make data public
bob    = toCNF "bob"   -- Bob can make data public


-- Alice or Bob can make data public:
aliceOrBob = "alice" \/ "bob"

-- Alice and Bob need to authorize the making of this data public:
aliceAndBob = "alice" /\ "bob"

-- How do we check if a flow of information is allowed?
--------------------------------------------------------------------

example1 = 
 [ canFlowTo public     alice  == undefined
  -----------------------------------------
 , canFlowTo alice      public == undefined
  -----------------------------------------
 , canFlowTo alice      bob    == undefined
  -----------------------------------------
 , canFlowTo aliceOrBob bob    == undefined ]

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
   A privilege can be used to:
    - remove secrecy restrictions (make data more public; declassify)
-}

-- downgradeP: make data as public as possible
--------------------------------------------------------------------

example3 = 
 [ downgradeP alice alice       == undefined
  ------------------------------------------
 , downgradeP alice aliceOrBob  == undefined
  ------------------------------------------
 , downgradeP alice aliceAndBob == undefined ]

-- canFlowToP: more permisive canFlowTo check that uses privileges
--------------------------------------------------------------------

example4 =
 [ canFlowToP alice aliceOrBob public  == undefined
  -------------------------------------------------
 , canFlowToP alice alice bob          == undefined
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
