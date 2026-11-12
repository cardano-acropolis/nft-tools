# nft-tools

Tools for working with and issuing NFTs on Cardano.

On-chain code is Plinth (PlutusTx), compiled to Plutus V3 for the Conway
era. `nft-client` builds unsigned Conway transactions for a multi-machine
drop. You sign them with `cardano-cli`.

## Status

| Component | File | State |
|---|---|---|
| One-shot NFT minting policy | `src/NFT.hs` | Plutus V3. In the `nft-plinth` library, evaluated by the test suite |
| `write-nft-policy` | `app/write-nft-policy.hs` | Writes a `cardano-cli` `PlutusScriptV3` text envelope |
| Vending machine | `src/MintingMachine.hs` | Plutus V3 spending validator. One script for every machine in a drop |
| Thread-token family | `src/ThreadFamily.hs` | Mints one token name per machine under a shared policy id |
| `write-vending-machine` | `app/write-vending-machine.hs` | Writes a `cardano-cli` `PlutusScriptV3` text envelope |
| `write-thread-family` | `app/write-thread-family.hs` | Writes the thread-token minting policy envelope |
| Off-chain client | `offchain/Client.hs`, `app/nft-client.hs` | Builds unsigned `TxBodyConway` bodies: mint, open, seed, set-price, buy, withdraw, close, rebalance |
| `generate-vending-machine` | `app/generate-vending-machine.hs` | Still a stub. Sale transactions are `nft-client` |
| `generate-airdrop` | `app/generate-airdrop.hs` | Stub |
| `ticket-sale` | `app/ticket-sale.hs` | Stub. The minting policy now allows a later burn |
| Test suite | `test/Spec.hs` | Evaluates the scripts, and checks the built transactions without a node |

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
from. On Linux it puts the C libraries on `PKG_CONFIG_PATH`,
`LIBRARY_PATH`, and `LD_LIBRARY_PATH`. `LIBRARY_PATH` is what the link
step uses to find `libsodium`. Without `LD_LIBRARY_PATH`, running a built
executable fails to load `libblst.so`. `cabal.project` turns off
`cardano-crypto-praos`'s `external-libsodium-vrf` flag so that package
compiles its own VRF code. The libsodium from `get-crypto-libs.sh` does
not export those symbols.

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

`BuyNFT` requires the transaction validity range to be finite, with both
ends inside the sale window. A fully closed interval is accepted. Conway's
ledger translation is closed at the start and open at the end
(`invalidHereafter` is `strictUpperBound`), and that shape is accepted
when every included millisecond is still inside the window. An `always`
range, an open lower bound, or a range that sticks out of the window
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

## Off-chain client

`nft-client` builds the transactions. The library is `offchain/Client.hs`,
not under `src/`, so the Plinth plugin is not applied to ledger code and the
validators are not compiled a second time. It does not query a node, sign, or
submit. The file it writes is a `TxBodyConway` text envelope (`type`,
`description`, `cborHex`) that `cardano-cli conway transaction sign`
accepts. Key witnesses are absent: there is no mnemonic file and no
hardware-wallet backend. `cardano-api` is not used. The newest release at
this CHaP pin (11.7) depends on `plutus-ledger-api ^>=1.70`, which excludes
the 1.71 line the scripts use. `cardano-ledger-conway` 1.23 accepts 1.71.
Release 1.24 is dated after the pin, so the solver never sees it.

`generate-airdrop` and `ticket-sale` are still stubs. The minting policy
already authorizes a later burn, which a redemption transaction would use.
`generate-vending-machine` does not build sale transactions either.

### Node, socket, and magic

Point `cardano-cli` at the node. The client never opens the socket.

```sh
export CARDANO_NODE_SOCKET_PATH=/path/to/node.socket
# Preview magic is 2, preprod is 1. Mainnet is --mainnet.
# The client does not hard-code a magic number.
cardano-cli conway query protocol-parameters \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --testnet-magic 2 \
  --out-file pparams.json
cardano-cli conway query utxo \
  --address "$(cat payment.addr)" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --testnet-magic 2
```

`--network testnet` (the default) or `--network mainnet` is the Shelley
address tag on the outputs, not the network magic. A preview or preprod
address uses `testnet`.

A buy's `--invalid-before` and `--invalid-hereafter` are slot numbers.
Conway turns that pair into a closed lower bound and an open upper bound.
Both ends have to land inside the sale's POSIX window. Convert slots with
the node's system start and slot length (`cardano-cli conway query
system-start`). The client does not do that conversion.

The script integrity hash covers the cost models in `--protocol-params`.
Use the file from the node you will submit to. `--mem` and `--cpu` are
copied onto every redeemer (defaults 14000000 and 10000000000). Those
defaults are a placeholder budget: they make the printed minimum fee large,
and a node will reject the script if the real cost is higher than the
budget you attached. Measure units against the node and pass them in, then
rebuild if the printed minimum exceeds `--fee`.

### Envelopes

Compile the three scripts first. Pass the files the writers emit. The
client reads `cborHex` and hashes those bytes; it does not recompile.

```sh
cabal run write-nft-policy -- "SALE" <64-hex-tx-id> 0 nft.plutus
cabal run write-thread-family -- <64-hex-tx-id> 0 family.plutus
cabal run write-vending-machine -- \
  <56-hex-seller-pkh> <56-hex-thread-policy> \
  <56-hex-nft-policy> SALE \
  1700000000000 1700086400000 "drop" machine.plutus
```

Hashes are hex with no `0x`. Payment key hashes and policy ids are 28
bytes. Transaction ids are 32 bytes. Addresses are `key:HEX` or
`script:HEX` and have no staking credential. An input is `TXID#INDEX`. A
token is `POLICY_HEX:NAME:QTY`.

### Seller flow

`nft-client help` prints every flag. The shape of a mint, then one machine,
is:

```sh
cabal run nft-client -- mint-nft \
  --protocol-params pparams.json --network testnet \
  --script nft.plutus --nft-name SALE \
  --one-shot TXID#0 --one-shot-lovelace 5000000 --one-shot-address key:SELLER \
  --dest key:SELLER --out-lovelace 2000000 \
  --cip25-name "Ticket" --cip25-image "ipfs://..." \
  --cip25-media-type "image/png" --cip25-description "a ticket" \
  --fee-in TXID#1 --fee-in-lovelace 10000000 --fee-in-address key:SELLER \
  --collateral TXID#2 --fee 200000 --change key:SELLER \
  --out-file mint-nft.txbody

cabal run nft-client -- mint-threads \
  --protocol-params pparams.json --script family.plutus \
  --thread-name THREAD --thread-name THREAD-B \
  --one-shot TXID#0 --one-shot-lovelace 5000000 --one-shot-address key:SELLER \
  --dest key:SELLER --out-lovelace 2000000 \
  --fee 200000 --change key:SELLER --out-file mint-threads.txbody

cabal run nft-client -- open \
  --protocol-params pparams.json --machine-script machine.plutus \
  --thread-policy THREAD_POLICY --thread-name THREAD \
  --nft-policy NFT_POLICY --nft-name SALE \
  --count 2 --price 100 --metadata-utf8 drop --lock-lovelace 2000000 \
  --wallet TXID#0 --wallet-lovelace 5000000 --wallet-address key:SELLER \
  --wallet-token THREAD_POLICY:THREAD:1 --wallet-token NFT_POLICY:SALE:2 \
  --fee 200000 --change key:SELLER --out-file open.txbody
```

`open` locks a fresh machine. The validator does not run. `seed` is
`AddNFT` on a machine that already exists (`--machine`, `--seller`,
`--wallet` holding the extra NFTs). `set-price`, `buy`, `withdraw`, and
`close` each name one machine UTxO (`--machine TXID#IX --machine-lovelace
--machine-nfts`). `buy` also needs `--pay`, `--buyer`, `--out-lovelace`,
and both slot bounds. `close` adds `--thread-script family.plutus` and
burns that machine's thread token. `rebalance` is `Withdraw` of NFTs from
`--from` and `AddNFT` of the same count on `--to`, in one seller
transaction. Ada on both machines stays put.

After each body:

```sh
cardano-cli conway transaction sign \
  --tx-body-file open.txbody \
  --signing-key-file payment.skey \
  --out-file open.tx
cardano-cli conway transaction submit \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --testnet-magic 2 \
  --tx-file open.tx
```

### CIP-25

`--cip25-name` and `--cip25-image` (optional `--cip25-media-type` and
`--cip25-description`) attach metadata label 721 on `mint-nft` and
`mint-threads`. The map is `policy id -> token name -> {name, image, ...}`
plus `version: 1.0`, which is CIP-25 version 1. The policy id is the hex
script hash. The token name is the UTF-8 text, not hex. Plutus V3
validators do not receive transaction metadata, so this map is off-chain
only. The bytes in `--metadata-utf8` are the `SaleState` datum and must
equal the metadata parameter of `write-vending-machine`.

### What still needs a live node

The unit tests build these transactions and inspect inputs, mint, datums,
redeemers, CIP-25, and the envelope. They do not need a node. Submitting
does. You still have to query UTxOs and protocol parameters, choose slots
inside the sale window, supply a measured Plutus budget, sign, and submit.
If `--fee` is below the printed ledger minimum, rebuild the body.

## What is left

`generate-airdrop` and `ticket-sale` remain stubs. A redemption flow would
burn the sale NFT with the policy that already allows that burn.

## Notes

Working notes and the project log live in [docs/NOTES.md](docs/NOTES.md).
