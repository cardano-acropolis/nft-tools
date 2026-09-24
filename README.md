# nft-tools

Tools for working with and issuing NFTs on Cardano.

## Planned modules

 1. **Vending machine** — a state machine that sells NFTs from an
    inventory at a seller-controlled price (`src/MintingMachine.hs`).
 2. **Airdrop** — a mechanism for verifiably random and fair airdrops
    (`app/generate-airdrop.hs`).
 3. **Staking** — a staking mechanism.
 4. **Royalties** — kickbacks to the creator on secondary sales.
 5. **Receipts** — receipts of purchase that convert physical items
    into NFTs, so that the holder of the wallet holding the receipt
    also has ownership and rightful possession of the thing itself.
     * What is the proper language to be included in such an NFT?
     * What is the proper legal instrument for such an entity?

## Status

| Component | File | State |
|---|---|---|
| One-shot NFT minting policy | `src/NFT.hs` | Drafted; needs Plutus toolchain to build |
| Vending machine | `src/MintingMachine.hs` | Drafted; validator checks and `Withdraw` are TODO |
| Off-chain client | `src/Client.hs` | Stub |
| `generate-vending-machine` | `app/generate-vending-machine.hs` | CLI skeleton |
| `generate-airdrop` | `app/generate-airdrop.hs` | Working: weighted, verifiable draw from a public seed |
| `ticket-sale` | `app/ticket-sale.hs` | Stub |
| Test suite | `test/MyLibTest.hs` | Placeholder |

The Plutus contract modules (`NFT`, `MintingMachine`, `Client`) are not
yet part of the cabal build: they need a pinned plutus-apps environment
(`plutus-ledger`, `plutus-tx`, `cardano-api`). See the note in
`nft-tools.cabal`.

## Building

```sh
cabal build all
cabal test
```

## Notes

Working notes and the project log live in [docs/NOTES.md](docs/NOTES.md).
