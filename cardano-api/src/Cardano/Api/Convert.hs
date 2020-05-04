module Cardano.Api.Convert
  ( addressFromHex
  , addressToHex
  , parseTxIn
  , parseTxOut
  , renderTxIn
  , renderTxOut
  ) where

import           Cardano.Api.Types
import qualified Cardano.Binary as Binary
import           Cardano.Prelude

import           Data.Attoparsec (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Base16 as Base16
import           Data.Char (isAlphaNum)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.Crypto.Hash.Class
                   (Hash(..), getHashBytesAsHex, hashFromBytesAsHex)
import qualified Cardano.Crypto.Hash.Blake2b as Crypto

import qualified Shelley.Spec.Ledger.Address as Shelley


addressFromHex :: Text -> Maybe Address
addressFromHex txt =
  case Base16.decode (Text.encodeUtf8 txt) of
    (raw, _) ->
      case Shelley.deserialiseAddr raw of
        Just addr -> Just $ AddressShelley addr
        Nothing -> either (const Nothing) (Just . AddressByron) $ Binary.decodeFull' raw

addressToHex :: Address -> Text
addressToHex addr =
  -- Text.decodeUtf8 theoretically can throw an exception but should never
  -- do so on Base16 encoded data.
  Text.decodeUtf8 . Base16.encode $
    case addr of
      AddressByron ba -> Binary.serialize' ba
      AddressShelley sa -> Shelley.serialiseAddr sa

parseTxIn :: Text -> Maybe TxIn
parseTxIn t =
  case Atto.parseOnly pTxIn $ Text.encodeUtf8 t of
    Left _Str -> Nothing
    Right txIn -> txIn

parseTxOut :: Text -> Maybe TxOut
parseTxOut =
  either (const Nothing) Just . Atto.parseOnly pTxOut . Text.encodeUtf8

renderTxIn :: TxIn -> Text
renderTxIn (TxIn (TxId txid) txix) =
  mconcat
    [ Text.decodeUtf8 $ Base16.encode (getHashBytesAsHex txid)
    , "@"
    , Text.pack (show txix)
    ]

renderTxOut :: TxOut -> Text
renderTxOut (TxOut addr ll) =
  mconcat
    [ addressToHex addr
    , "&"
    , Text.pack (show ll)
    ]

pTxIn :: Parser (Maybe TxIn)
pTxIn = liftA2 mTxIn (pTxId <* Atto.char '@') Atto.decimal
 where
   mTxIn :: Maybe TxId -> TxIx -> Maybe TxIn
   mTxIn (Just txId) idx = Just $ TxIn txId idx
   mTxIn Nothing _ = Nothing

pTxId :: Parser (Maybe TxId)
pTxId = mTxId <$> pCBlakeHash
 where
   mTxId :: Maybe (Hash Crypto.Blake2b_256 ()) -> Maybe TxId
   mTxId (Just hash) = Just $ TxId hash
   mTxId Nothing = Nothing

pCBlakeHash :: Parser (Maybe (Hash Crypto.Blake2b_256 ()))
pCBlakeHash = hashFromBytesAsHex <$> pHexToByteString

pTxOut :: Parser TxOut
pTxOut =
  TxOut <$> (pAddress <* Atto.char '&') <*> pLovelace

pLovelace :: Parser Lovelace
pLovelace = Lovelace <$> Atto.decimal

pAddress :: Parser Address
pAddress =
  Atto.choice
    [ AddressByron <$> pByronAddress
    , AddressShelley <$> pShelleyAddress
    ]

pByronAddress :: Parser ByronAddress
pByronAddress = undefined

pShelleyAddress :: Parser ShelleyAddress
pShelleyAddress = undefined

pHexToByteString :: Parser ByteString
pHexToByteString =
  fst . Base16.decode <$> Atto.takeWhile1 isAlphaNum
