{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.ByteString.Char8 (ByteString, length, pack)
import Data.Typeable
import Test.Tasty
import Test.Tasty.HUnit

import Data.StaticText

mkPacket :: ByteString -> Static ByteString 32
mkPacket inp =
  -- 5-character version signature
  $(st "PKT10") `append`
  -- 25-character payload
  payload `append`
  -- 2-character payload checksum
  checksum
  where
    payload = createLeft 0x20 inp
    checksum :: Static ByteString 2
    checksum = createLeft 0x20 $
               pack $ show $ Data.StaticText.length payload `mod` 100

message :: Static ByteString 64
message = mkPacket "Hello" `append` mkPacket "world"

tests :: [TestTree]
tests =
  [ testCase ("The actual length of " ++ show (typeOf message)) $
    Data.ByteString.Char8.length (unwrap message) @=? 64
  ]

main :: IO ()
main = defaultMain $ testGroup "Tests" tests
