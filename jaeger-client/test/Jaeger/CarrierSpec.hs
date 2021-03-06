{-# LANGUAGE OverloadedStrings #-}
module Jaeger.CarrierSpec where

import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text
import           Jaeger
import           Network.HTTP.Types.Header (Header)
import           Test.Hspec

spec :: Spec
spec = describe "Jaeger HTTP Header Handling" $ do

  let
    sampleTrace = "707f0a307d427024:48c06d0f2ca97ab4:167162f1dfc0fc1:1"
    detTr t = t { tracerIdGenerator = pure 1234567890 }


  it "extracts span and trace IDs from HTTP Header" $ do
    t <- openTracer (TracerConfiguration "localhost" "9999" "service")

    extract t [ sampleHeader ] `shouldBe` Just (SpanContext 8106209057666396196 5242309878200498868)

  it "injects span and trace IDs into HTTP Header" $ do
    t <- detTr <$> openTracer (TracerConfiguration "localhost" "9999" "service")

    Just sp <- pushSpan t "foo" Nothing >> readActiveSpan t

    inject t sp [(tracingHeader, "00:00:00:00")] `shouldBe` [(tracingHeader, "00000000499602d2:00000000499602d2" <> defaultEncodedSuffix) :: Header]

  it "injects span and trace IDs as Text" $ do
    t <- detTr <$> openTracer (TracerConfiguration "localhost" "9999" "service")

    Just sp <- pushSpan t "foo" Nothing >> readActiveSpan t

    inject t sp "" `shouldBe` ("00000000499602d2:00000000499602d2" <> defaultEncodedSuffix :: Text)


sampleHeader :: Header
sampleHeader = ( tracingHeader, "707f0a307d427024:48c06d0f2ca97ab4:167162f1dfc0fc1:1")
