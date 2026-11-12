# nft-tools

Tools for working with and issuing NFTs on Cardano.

On-chain code is Plinth (PlutusTx), compiled to Plutus V3 for the Conway
era. The off-chain client does not build transactions yet.

## Status

| Component | File | State |
|---|---|---|
| One-shot NFT minting policy | `src/NFT.hs` | Plutus V3. In the `nft-plinth` library, evaluated by the test suite |
| `write-nft-policy` | `app/write-nft-policy.hs` | Writes a `cardano-cli` `PlutusScriptV3` text envelope |
| Vending machine | `src/MintingMachine.hs` | Plutus V3 spending validator. One script for every machine in a drop |
| Thread-token family | `src/ThreadFamily.hs` | Mints one token name per machine under a shared policy id |
| `write-vending-machine` | `app/write-vending-machine.hs` | Writes a `cardano-cli` `PlutusScriptV3` text envelope |
| `write-thread-family` | `app/write-thread-family.hs` | Writes the thread-token minting policy envelope |
| Off-chain client | `src/Client.hs` | Not in the build. Does not construct transactions |
| `generate-vending-machine` | `app/generate-vending-machine.hs` | CLI skeleton. Does not build sale transactions |
| `generate-airdrop` | `app/generate-airdrop.hs` | Stub |
| `ticket-sale` | `app/ticket-sale.hs` | Stub. The minting policy now allows a later burn |
| Test suite | `test/Spec.hs` | Evaluates the policies and the vending machine on hand-built V3 script contexts |

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
that file always failed and has been removed; spending a sale UTxO is the
vending machine below. A one-machine drop can use this policy as its thread
policy. A drop with several machines uses `write-thread-family` instead, so
every machine token shares one policy id.

## Vending machine

`write-vending-machine` compiles one sale into a spending validator and
writes a text envelope:

```sh
cabal run write-thread-family -- <64-hex-tx-id> 0 family.plutus
cabal run write-vending-machine -- \
  <56-hex-seller-pkh> <56-hex-thread-policy> \
  <56-hex-nft-policy> NAME \
  1700000000000 1700086400000 "metadata" machine.plutus
```

`write-thread-family` prints the policy id. Spending its one-shot UTxO mints
the machine tokens: any number of token names, each of quantity 1. One name
is one machine. Burning those names later does not need the UTxO.

`write-vending-machine` prints the script hash. The seller, thread policy,
sale NFT, window, and metadata are the script parameters, so every machine
in the drop is the same script. The thread policy and the sale NFT have to
be different policies. Create each machine off chain by locking one thread
token on its own output at this script hash, with an inline datum and no
staking credential. The first spend checks that datum. There is no
state-machine library.

### Datum and redeemer

`SaleState` is constructor 0, two fields:

| Field | Type | Role |
|---|---|---|
| price | integer, zero or greater | lovelace per NFT |
| metadata | bytes | must equal the metadata parameter on every spend |

The stock is the sale-NFT quantity on the UTxO, not a list in the datum.
The metadata bytes cannot grow: a transition that writes any other blob
fails. Datum hashes are rejected; the output datum has to be inline.

`MachineRedeemer`:

| Constructor | Index | Arguments | Who signs | Effect |
|---|---|---|---|---|
| `SetPrice` | 0 | new price | seller | price becomes that integer; the value is unchanged. Allowed outside the sale window |
| `AddNFT` | 1 | count, greater than 0 | seller | sale-NFT quantity increases by that count; ada does not decrease |
| `BuyNFT` | 2 | count, greater than 0 | nobody | see below |
| `Withdraw` | 3 | lovelace, NFT count | seller | remove exactly those amounts, or close the machine |

`BuyNFT` requires the transaction validity range to be a finite closed
interval whose ends both sit inside the sale window (inclusive). An
`always` range, an open bound, or a range that sticks out of the window
fails. The machine's ada must rise by at least `count * price` (overpaying
is allowed, and a price of 0 is allowed). The machine's NFT quantity drops
by `count`. Outputs that do not carry this machine's thread token must
together hold exactly `count` of the sale NFT. The payment stays in that
machine until `Withdraw`. A `BuyNFT` transaction may contain only that one
machine's thread token, so one payment cannot be counted for two machines.
The 2021 transition left the payment in the machine; this port keeps that.

`Withdraw` has two shapes. If some output still holds the thread token,
both amounts are zero or greater, they are not both zero, and neither
exceeds the balance. The continuing output's ada and NFT quantity drop by
exactly those amounts. If no output holds the thread token, the redeemer
closes the machine: both amounts must equal the full ada and the full NFT
stock, and the transaction must burn exactly one thread token. Other
tokens (anything that is not ada, the sale NFT, or the thread token) stay
on the machine for a partial withdraw and leave with the close, because
the seller signed. The redeemer does not name them, and it does not require
the withdrawn value to go to the seller's address.

The seller's signature is `txSignedBy` on that payment key hash. `AddNFT`
did not require a signature in the 2021 transition; it does now, so a
third party cannot push inventory into the machine. `BuyNFT` has no
signature check. Paying the ada and delivering the NFT is what authorizes
it, and the buyer chooses which non-script outputs receive the NFT.

### Several machines, one drop

The spent input is found by comparing out-refs as `Data`. The thread token
on that input (exactly one name under the thread policy, quantity 1) is
this machine. Exactly one input may carry that name, and it has to be the
input being spent. Reference inputs do not count. Exactly one output may
carry it, and that output has to pay this same script hash with no staking
credential, except on close, where zero outputs carry it and the token is
burned. The continuing output cannot also carry a second machine's token.
Putting two machines on one output would let both script runs treat the
same lovelace as their own.

Another output at this script is allowed only when it carries a different
thread token of quantity 1. That is a sibling machine. An output at this
script with no thread token fails, because nothing could spend it. The
sale NFT must not be minted or burned in the same transaction. Inventory
moves between outputs.

`BuyNFT` is stricter. The transaction may not contain any other thread
token, as an input, an output, or a mint. Two purchases are two
transactions. They can still land in the same block, because they spend
different UTxOs. Each purchase checks the ada increase on its own
continuing output, so a payment that reaches only one machine does not
satisfy the other.

Seller actions may spend several machines in one transaction. `Withdraw`
on a machine that has too much stock and `AddNFT` on a machine that has
too little move inventory without a buyer. `SetPrice` and `Withdraw` are
per machine: the price in the datum is not forced to match the other
machines, and closing one machine does not close the others.

### Contention

N machine UTxOs can accept about N purchases in one block, one transaction
each. A buyer who wants several tokens from the machine they picked uses
`BuyNFT` with a count greater than 1. A buyer who finds a machine empty
picks another; the empty one fails with `not enough inventory` and does
not block the rest.

Stock will drift. A popular machine sells out while others still hold
tokens. The seller rebalances by withdrawing NFTs from a rich machine and
adding them to a poor one, which can be the same transaction. Prices can
drift the same way: `SetPrice` updates only the machine being spent, so
set it on each machine that should change. An idle machine can stay open
at price 0, or the seller can close it by burning its thread token.

## What is next

`src/Client.hs` was a placeholder for the plutus-apps `Contract` monad
(`getUnspentOutput` to choose the UTxO). That monad is gone with
plutus-apps. The off-chain side is cardano-cli or cardano-api: run
`write-thread-family`, mint one token name per machine, run
`write-vending-machine`, and submit `SetPrice`, `AddNFT`, `BuyNFT`, and
`Withdraw` against that envelope. This repo does not build those
transactions yet.

`generate-vending-machine`, `generate-airdrop`, and `ticket-sale` are still
stubs on the `MyLib` placeholder. The minting policy already authorizes a
later burn, which a redemption transaction would use.

## Notes

Working notes and the project log live in [docs/NOTES.md](docs/NOTES.md).
