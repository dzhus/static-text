# sext: lists, Texts, ByteStrings and Vectors with type-encoded length

[![Travis CI build status](https://travis-ci.org/dzhus/sext.svg)](https://travis-ci.org/dzhus/sext)
[![Hackage](https://img.shields.io/hackage/v/sext.svg)](https://hackage.haskell.org/package/sext)
[![Hackage deps](https://img.shields.io/hackage-deps/v/sext.svg)](http://packdeps.haskellers.com/feed?needle=sext)

Sext (*s*tatic t*ext*) provides type-level safety for basic operations
on string-like types (finite lists of elements). Use it when you need
static guarantee on lengths of strings produced in your code.

An example application would be a network exchange protocol built of packets with fixed-width fields:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Data.Sext

mkPacket :: ByteString -> Sext 32 ByteString
mkPacket inp =
  -- 5-character version signature
  $(sext "PKT10") `append`
  -- 25-character payload
  payload `append`
  -- 2-character payload checksum
  checksum
  where
    payload = createLeft 0x20 inp
    checksum :: Sext 2 ByteString
    checksum = createLeft 0x20 $
               pack $ show $ Data.Sext.length payload `mod` 100

message :: Sext 64 ByteString
message = mkPacket "Hello" `append` mkPacket "world"
```

Please consult the [Hackage page for sext][hackage-doc] for
documentation and examples.

[hackage-doc]: http://hackage.haskell.org/package/sext/docs/Data-Sext.html
