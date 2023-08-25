{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module ParameterizedVesting where

import           Plutus.V1.Ledger.Interval (contains)
import           Plutus.V2.Ledger.Api      (BuiltinData, POSIXTime, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo (txInfoValidRange),
                                            Validator, from, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx                  (applyCode, compile, liftCode, CompiledCode, unstableMakeIsData,
                                            makeLift, UnsafeFromData (unsafeFromBuiltinData))
import           PlutusTx.Prelude          (Bool, traceIfFalse, ($), (&&), (.))
import           Prelude                   (IO)
import           Utilities                 (wrapValidator, writeValidatorToFile, writeCodeToFile)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingParams = VestingParams
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    }

unstableMakeIsData ''VestingParams
makeLift ''VestingParams

{-# INLINABLE mkParameterizedVestingValidator #-}
mkParameterizedVestingValidator
    :: VestingParams
    -> ()
    -> ()
    -> ScriptContext
    -> Bool
mkParameterizedVestingValidator params () () ctx =
    traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
    traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ beneficiary params

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline params) $ txInfoValidRange info

{-# INLINABLE  mkWrappedParameterizedVestingValidator #-}
mkWrappedParameterizedVestingValidator
    :: VestingParams -- param
    -> BuiltinData   -- datum
    -> BuiltinData   -- redeemer
    -> BuiltinData   -- context
    -> ()
mkWrappedParameterizedVestingValidator = wrapValidator . mkParameterizedVestingValidator

mkWrappedParameterizedVestingValidator'
    :: BuiltinData -- beneficiary
    -> BuiltinData -- deadline
    -> BuiltinData -- datum
    -> BuiltinData -- redeemer
    -> BuiltinData -- context
    -> ()
mkWrappedParameterizedVestingValidator' b d = wrapValidator f
    where
        vp = VestingParams
            { beneficiary = unsafeFromBuiltinData b
            , deadline = unsafeFromBuiltinData d
            }
        f = mkParameterizedVestingValidator vp


validatorCode
    :: PlutusTx.CompiledCode (
           BuiltinData
        -> BuiltinData
        -> BuiltinData
        -> BuiltinData
        -> BuiltinData
        -> ()
        )
validatorCode = $$(PlutusTx.compile [|| mkWrappedParameterizedVestingValidator' ||])


validator :: VestingParams -> Validator
validator params = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode params)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveValidatorCode :: IO ()
saveValidatorCode = writeCodeToFile "assets/offchain-parameterized-vesting.plutus" validatorCode

saveVal :: VestingParams -> IO ()
saveVal = writeValidatorToFile "./assets/parameterized-vesting.plutus" . validator
