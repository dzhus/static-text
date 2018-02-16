{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.ByteString.Char8 (ByteString, length, pack)
import Data.Typeable
import Test.Tasty
import Test.Tasty.HUnit

import Data.StaticText

mkPacket :: ByteString -> StaticText 32 ByteString
mkPacket inp =
  -- 5-character version signature
  $(sext "PKT10") `append`
  -- 25-character payload
  payload `append`
  -- 2-character payload checksum
  checksum
  where
    payload = createLeft 0x20 inp
    checksum :: StaticText 2 ByteString
    checksum = createLeft 0x20 $
               pack $ show $ Data.StaticText.length payload `mod` 100

message :: StaticText 64 ByteString
message = mkPacket "Hello" `append` mkPacket "world"

tests :: [TestTree]
tests =
  [ testCase ("The actual length of " ++ show (typeOf message)) $
    assertEqual "" 64 (Data.ByteString.Char8.length $ unwrap message)
  ]

main :: IO ()
main = defaultMain $ testGroup "Tests" tests
