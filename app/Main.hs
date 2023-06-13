{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Autodocodec
import Autodocodec.Swagger.DerivingVia ()
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Void (Void)
import Data.Swagger (ToSchema, declareNamedSchema, Definitions, Schema)
import Data.Swagger.Declare (runDeclare)
import Data.Proxy (Proxy(Proxy))
import Data.List.NonEmpty (NonEmpty)

main :: IO ()
main =
  let
    schema :: Definitions Schema
    schema = fst $ runDeclare (declareNamedSchema (Proxy @Predicate)) mempty
  in
    print schema

data Predicate = And (NonEmpty Predicate) | Or (NonEmpty Predicate) | Eq Text Text
  deriving ToSchema via Autodocodec Predicate

instance HasCodec Predicate where
  codec = object "predicate" $ objectCodec

instance HasObjectCodec Predicate where
  objectCodec = discriminatedUnionCodec "predicate" enc dec where
    andCodec :: JSONObjectCodec (NonEmpty Predicate)
    andCodec = requiredField' "subExpressions"
    orCodec :: JSONObjectCodec (NonEmpty Predicate)
    orCodec = requiredField' "subExpressions"
    eqCodec :: JSONObjectCodec (Text, Text)
    eqCodec = (,)
      <$> requiredField' "key" .= fst
      <*> requiredField' "value" .= snd
    enc :: Predicate -> (Discriminator, ObjectCodec Predicate ())
    enc = \case
      And l -> ("and", mapToEncoder l andCodec)
      Or l -> ("or", mapToEncoder l orCodec)
      Eq k v -> ("eq", mapToEncoder (k, v) eqCodec)
    dec :: HashMap Discriminator (Text, ObjectCodec Void Predicate)
    dec = HashMap.fromList
      [ ("and", ("and", mapToDecoder And andCodec))
      , ("or", ("or", mapToDecoder Or orCodec))
      , ("eq", ("eq", mapToDecoder (uncurry Eq) eqCodec))
      ]



