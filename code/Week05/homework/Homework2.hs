{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Homework2 where

import           Plutus.V2.Ledger.Api       (BuiltinData, CurrencySymbol,
                                             MintingPolicy,
                                             ScriptContext (scriptContextTxInfo),
                                             TokenName (unTokenName),
                                             TxId (TxId, getTxId),
                                             TxInInfo (txInInfoOutRef),
                                             TxInfo (txInfoInputs, txInfoMint),
                                             TxOutRef (TxOutRef, txOutRefId, txOutRefIdx),
                                             mkMintingPolicyScript)
import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (False), ($), (.), traceIfFalse, (&&), any, (==))
import           Utilities            (wrapPolicy, writeCodeToFile, writePolicyToFile, currencySymbol)
import           Prelude                    (IO, Show (show), String)
import           Plutus.V1.Ledger.Value     (flattenValue)
import           Text.Printf                (printf)
import           PlutusTx.Builtins.Internal (emptyByteString)

{-# INLINABLE mkEmptyNFTPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkEmptyNFTPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkEmptyNFTPolicy oref () ctx =
    traceIfFalse "UTxO not consumed"   hasUTxO           &&
                                 traceIfFalse "wrong amount minted or token name is not empty" checkMintedAmount
      where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        hasUTxO :: Bool
        hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

        checkMintedAmount :: Bool
        checkMintedAmount = case flattenValue (txInfoMint info) of
            [(_, tn, amt)] ->
                amt == 1
                && unTokenName tn == emptyByteString
            _                -> False

{-# INLINABLE mkWrappedEmptyNFTPolicy #-}
mkWrappedEmptyNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedEmptyNFTPolicy tid ix =
  let
    oref :: TxOutRef
    oref = TxOutRef
        (TxId $ PlutusTx.unsafeFromBuiltinData tid)
        (PlutusTx.unsafeFromBuiltinData ix)

    p = mkEmptyNFTPolicy oref
  in wrapPolicy $ p

nftCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
nftCode = $$(PlutusTx.compile [|| mkWrappedEmptyNFTPolicy ||])

nftPolicy :: TxOutRef -> MintingPolicy
nftPolicy oref = mkMintingPolicyScript $
    nftCode
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ getTxId $ txOutRefId oref)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ txOutRefIdx oref)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveEmptyNFTCode :: IO ()
saveEmptyNFTCode = writeCodeToFile "assets/empty-nft.plutus" nftCode

saveEmptyNFTPolicy :: TxOutRef -> IO ()
saveEmptyNFTPolicy oref = writePolicyToFile
    (printf "assets/empty-nft-%s#%d.plutus"
        (show $ txOutRefId oref)
        (txOutRefIdx oref)
    )
    $ nftPolicy oref

nftCurrencySymbol :: TxOutRef -> CurrencySymbol
nftCurrencySymbol oref = currencySymbol $ nftPolicy oref

