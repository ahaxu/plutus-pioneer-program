{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Homework1 where

import           Plutus.V2.Ledger.Api (BuiltinData, MintingPolicy, POSIXTime,
                                       PubKeyHash, ScriptContext,
                                       TxInfo,
                                       scriptContextTxInfo,
                                       mkMintingPolicyScript, lowerBound)
import qualified PlutusTx
import           Plutus.V2.Ledger.Contexts (txSignedBy, txInfoValidRange)
import           PlutusTx.Prelude     (Bool, ($), (&&), traceIfFalse)
import           Utilities            (wrapPolicy, writeCodeToFile, writePolicyToFile)
import           Plutus.V1.Ledger.Interval (contains, to, from)
import           Prelude                    (IO, Show (show))
import           Text.Printf                (printf)

{-# INLINABLE mkDeadlinePolicy #-}
-- This policy should only allow minting (or burning) of tokens if the owner of the specified PubKeyHash
-- has signed the transaction and if the specified deadline has not passed.
mkDeadlinePolicy :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkDeadlinePolicy pkh deadline () ctx =
    traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
    traceIfFalse "deadline has passed" checkDeadline
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info pkh

    checkDeadline :: Bool
    checkDeadline =
        contains (to deadline) $ txInfoValidRange info

{-# INLINABLE mkWrappedDeadlinePolicy #-}
mkWrappedDeadlinePolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedDeadlinePolicy pkh deadline =
    wrapPolicy $
        mkDeadlinePolicy
            (PlutusTx.unsafeFromBuiltinData pkh)
            (PlutusTx.unsafeFromBuiltinData deadline)

deadlineCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
deadlineCode = $$(PlutusTx.compile [|| mkWrappedDeadlinePolicy ||])

deadlinePolicy :: PubKeyHash -> POSIXTime -> MintingPolicy
deadlinePolicy pkh deadline = mkMintingPolicyScript $
    deadlineCode
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData pkh)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData deadline)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveDeadlineCode :: IO ()
saveDeadlineCode = writeCodeToFile "assets/deadline-script.plutus" deadlineCode

saveDeadlinePolicy :: PubKeyHash -> POSIXTime -> IO ()
saveDeadlinePolicy pkh deadline = writePolicyToFile
    (printf "assets/deadline-script-%s-%s.plutus"
        (show pkh)
        (show deadline)
    ) $ deadlinePolicy pkh deadline

