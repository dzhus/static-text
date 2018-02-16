# static-text: lists, Texts, ByteStrings and Vectors of statically known length

[![Travis CI build status](https://travis-ci.org/dzhus/static-text.svg)](https://travis-ci.org/dzhus/static-text)
[![Hackage](https://img.shields.io/hackage/v/static-text.svg)](https://hackage.haskell.org/package/static-text)
[![Hackage deps](https://img.shields.io/hackage-deps/v/static-text.svg)](http://packdeps.haskellers.com/feed?needle=static-text)

static-text provides type-level safety for basic operations on
string-like types (finite lists of elements), such as `Data.Text`,
`String` (and all lists), `Data.ByteString` and `Data.Vector`. Use it
when you need static guarantee on lengths of strings produced in your
code.

An example application would be a network exchange protocol built of
packets with fixed-width fields:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
               pack $ show $ Data.Static.length payload `mod` 100

message :: Static ByteString 64
message = mkPacket "Hello" `append` mkPacket "world"
```

Please consult the [Hackage page for static-text][hackage-doc] for
documentation and examples.

[hackage-doc]: http://hackage.haskell.org/package/static-text/docs/Data-StaticText.html
