{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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

main = print message
