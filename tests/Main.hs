{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Typeable
import Test.Tasty
import Test.Tasty.HUnit

import Data.Sext

mkPacket :: String -> Sext 32 String
mkPacket inp =
  -- 5-character version signature
  $(sext "PKT10") `append`
  -- 25-character payload
  payload `append`
  -- 2-character payload checksum
  checksum
  where
    payload = createLeft ' ' inp
    checksum :: Sext 2 String
    checksum = createLeft ' ' $
               show $ Data.Sext.length payload `mod` 100

message :: Sext 64 String
message = mkPacket "Hello" `append` mkPacket "world"

tests :: [TestTree]
tests =
  [ testCase ("The actual length of " ++ show (typeOf message)) $
    assertEqual "" 64 (Prelude.length $ unwrap message)
  ]

main :: IO ()
main = defaultMain $ testGroup "Tests" tests
