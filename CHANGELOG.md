# Changelog

## [Unreleased]

- Package renamed to `static-text` as per
  <https://github.com/dzhus/static-text/issues/2>. Old names were
  changed as follows:

    | before       | after             |
    |--------------|-------------------|
    | `Data.Sext`  | `Data.StaticText` |
    | `Sext n a`   | `Static a n`      |
    | `Sextable a` | `IsStaticText a`  |
    | `$(sext ..)` | `$(st ..)`        |

## [0.1.3.1] - 2017-10-29

### Added

- GHC 8.2.x support

## [0.1.3] - 2017-03-26

### Added

- `ShortByteString` support

### Fixed

- A bug in `createLeft` which failed to actually pad/truncate strings
  (reported by Altai-man <https://github.com/dzhus/static-text/issues/4>)
