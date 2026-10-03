# nft-tools

Tools for working with and issuing NFTs on Cardano.

On-chain code is Plinth (PlutusTx), compiled to Plutus V3 for the Conway
era. The vending machine and the off-chain client are not part of that
port yet.

## Status

| Component | File | State |
|---|---|---|
| One-shot NFT minting policy | `src/NFT.hs` | Plutus V3. In the `nft-plinth` library, evaluated by the test suite |
| `write-nft-policy` | `app/write-nft-policy.hs` | Writes a `cardano-cli` `PlutusScriptV3` text envelope |
| Vending machine | `src/MintingMachine.hs` | Not in the build. Still the 2021 plutus-apps draft |
| Off-chain client | `src/Client.hs` | Not in the build. Empty stub |
| `generate-vending-machine` | `app/generate-vending-machine.hs` | CLI skeleton |
| `generate-airdrop` | `app/generate-airdrop.hs` | Stub |
| `ticket-sale` | `app/ticket-sale.hs` | Stub. The minting policy now allows a later burn |
| Test suite | `test/NFTSpec.hs` | Evaluates the policy on hand-built V3 script contexts |

## Toolchain

[Plinth's environment setup](https://plutus.cardano.intersectmbo.org/docs/using-plinth/environment-setup)
points at [plinth-template](https://github.com/IntersectMBO/plinth-template).
That template supports Nix, Docker, Demeter, or plain GHC + Cabal. All four
share one Cabal project: `plutus-tx`, `plutus-tx-plugin`, `plutus-ledger-api`,
and `plutus-core` come from the [Cardano Haskell Package index](https://chap.intersectmbo.org/)
(CHaP), not Hackage, and `plutus-tx-plugin` 1.71 builds only with GHC 9.6.x
or GHC 9.12.x.

This repo follows that Cabal + CHaP setup, which is the path verified here:

| Piece | Choice |
|---|---|
| GHC | 9.6.7 (9.12.x is the other compiler the plugin allows; it has not been run here) |
| Cabal | 3.16.1.0 (anything from 3.8 up matches the template) |
| Plutus / Plinth | `plutus-tx`, `plutus-tx-plugin`, `plutus-ledger-api`, `plutus-core` `^>=1.71.0.0` |
| Package index | Hackage `index-state` `2026-09-29T19:48:01Z` (plinth-template `main`). CHaP `2026-09-30T00:00:00Z` |
| Plutus Core | 1.1.0 (`-fplugin-opt=Plinth.Plugin:target-version=1.1.0`) |
| Datatypes | Scott encoding (`datatypes=ScottEncoding`), not the 1.71 sums-of-products default |
| C libraries | `libsodium` (VRF-patched), `libsecp256k1`, `libblst`, via `get-crypto-libs.sh` |

`cabal.project` records the CHaP repository and those index-states, so a
later `cabal build` resolves the same package set. The CHaP date is one day
later than plinth-template `main`: that template still pins
`2026-09-24T00:08:56Z`, and `plutus-tx-plugin` 1.71.0.0 was uploaded to CHaP
on 2026-09-29, so the template pin cannot satisfy the `^>=1.71.0.0` bounds
the template itself declares. `2026-09-30T00:00:00Z` is the first midnight
after that upload.

The plugin's default target in 1.71 is Plutus Core 1.2.0. A Plutus V3 script
at that version fails to deserialise until the Dijkstra hard fork (major
protocol version 12). Core 1.1.0 is accepted from the Chang hard fork
(protocol version 9, the start of Conway) onward, which is why the option is
set. `PlutusTx.compile` still exists and registers `Plinth.Plugin`. The
template's cabal file still passes `PlutusTx.Plugin:target-version=1.1.0`;
that module was removed in plutus-tx-plugin 1.63, so the option has to name
`Plinth.Plugin` or the compiler never sees it.

The same 1.71 release compiles datatypes as sums-of-products by default.
That encoding lowers pattern matches to `case` on built-in types, and
casing on `Data` is rejected until Dijkstra (protocol version 12), which
is not on mainnet. `datatypes=ScottEncoding` keeps the older builtins
(`ifThenElse`, `chooseData`, and the rest), so the script deserialises and
evaluates at Chang (protocol version 9). The tests call
`evaluateScriptCounting` at `changPV` for that reason. Scott encoding of
`TxOutRef` itself miscompiles in this plugin (equality and a pattern match
both try to instantiate a `Data` constant), so the UTxO check walks the
script context as `Data` and compares the out-ref with `equalsData`. The
parameter is still the `toBuiltinData` encoding of the `TxOutRef`.

Nix (`nix develop` from plinth-template's flake, GHC 9.6 by default or
`nix develop .#ghc912`) is the other setup the same docs maintain. It is not
vendored in this repo: the flake is haskell.nix plus the IOG binary cache,
and this change was built with GHC and Cabal. Use the Nix shell if you
already work that way; `cabal.project` is what either environment builds.

`get-crypto-libs.sh` is the script shipped with plinth-template. It
downloads the prebuilt libraries from the pinned
[iohk-nix v3.1 release](https://github.com/input-output-hk/iohk-nix/releases/tag/v3.1),
checks the published sha256 sums, and links them into
`dist-newstyle/crypto-libs/` without installing anything system-wide. The
system `libsodium` package is not a substitute: `cardano-crypto-class` needs
the VRF-patched build.

## Building

GHC 9.6.7 and Cabal 3.8 or newer, plus `pkg-config`, `curl`, `tar`, and
the C development libraries GHC links against (`libgmp-dev`; `libncurses-dev`
for `libtinfo`). On Debian or Ubuntu:

```sh
sudo apt-get install pkg-config curl tar libgmp-dev libncurses-dev
```

With [ghcup](https://www.haskell.org/ghcup/):

```sh
ghcup install ghc 9.6.7 --set
ghcup install cabal recommended --set
./get-crypto-libs.sh
source dist-newstyle/crypto-libs/env.sh
cabal update
cabal build all
cabal test
```

Source `dist-newstyle/crypto-libs/env.sh` in every shell you build or run
from. On Linux it puts the C libraries on `PKG_CONFIG_PATH` and
`LD_LIBRARY_PATH`; without the latter, running a built executable fails to
load `libblst.so`.

## One-shot policy

`write-nft-policy` compiles the policy for one token name and one UTxO and
writes a text envelope cardano-cli accepts as a Plutus V3 minting script:

```sh
cabal run write-nft-policy -- "Ticket" <64-hex-tx-id> 0 ticket.plutus
```

The command prints the policy id. Minting succeeds only in a transaction
that spends that UTxO and mints exactly one token of that name under this
policy. Burning any positive number of that same token (a negative mint
quantity) succeeds without the UTxO, which is what a later redemption
transaction needs. Other minting policies in the same transaction are
ignored. A second token name under this policy is rejected.

The 2021 draft required the whole transaction to mint exactly one asset of
amount 1, so it could not share a transaction with another policy and it
could not burn. It also lifted unused `NftParams` fields (`nftMetadata`,
`nftAC`, `nftPubKey`) into the script, which changed the policy id without
being checked. Those fields are gone. The unfinished `mkNFTValidator` in
that file always failed and has been removed; a spending check belongs with
the vending-machine port.

## What is next

`src/MintingMachine.hs` is still written against `Ledger`,
`Ledger.Typed.Scripts`, `Ledger.Constraints`, and
`Plutus.Contract.StateMachine`. None of those modules exist on the Plinth
side of the split: plutus-apps is archived, and Plutus V3 scripts are plain
`BuiltinData -> BuiltinUnit` validators. The next port is an explicit V3
spending validator (datum for price and inventory, redeemer for set-price /
add / buy / withdraw), not a state-machine library. The one-shot policy
above can be the thread token that identifies the sale UTxO.

`src/Client.hs` was a placeholder for the plutus-apps `Contract` monad
(`getUnspentOutput` to choose the UTxO). That monad is gone with
plutus-apps. The off-chain side is cardano-cli or cardano-api: choose a
UTxO, run `write-nft-policy`, and submit the mint transaction. The same
client is where the vending-machine transactions will be built once the
validator exists.

`generate-vending-machine`, `generate-airdrop`, and `ticket-sale` are still
stubs on the `MyLib` placeholder. Ticketing can burn on redemption only
after the sale validator exists; the minting policy already authorizes the
burn.

## Notes

Working notes and the project log live in [docs/NOTES.md](docs/NOTES.md).
