# Chain Finance - Supply Chain Finance for Retailers

Chain Finance is a Clarity smart contract that enables retailers to tokenize and sell expected inventory returns. This creates a decentralized supply chain finance platform where retailers can get early liquidity on their invoices, and investors can purchase these invoices at a discount for future returns.

## Features

- **Invoice Registration**: Retailers can register invoices with supplier information, amount, and due date
- **Invoice Tokenization**: Convert traditional invoices into tokenized digital assets
- **Marketplace**: Offer tokenized invoices for sale at a configurable discount
- **Investment**: Investors can purchase invoices at a discount and claim payment when due
- **Balances Management**: Track and withdraw funds for both retailers and investors

## Contract Functions

### Admin Functions

- `set-platform-fee`: Set the platform fee percentage (owner only)

### Retailer Functions

- `register-invoice`: Register a new invoice with supplier, amount, and due date
- `tokenize-invoice`: Convert a registered invoice into a tokenized asset
- `offer-invoice-for-sale`: Offer a tokenized invoice for sale with a discount
- `cancel-invoice-sale`: Cancel an invoice sale if not yet purchased
- `withdraw-retailer-funds`: Withdraw available retailer funds

### Investor Functions

- `buy-invoice`: Purchase an invoice that's for sale
- `claim-invoice-payment`: Claim payment for a purchased invoice after due date
- `withdraw-investor-funds`: Withdraw available investor funds

### Read-Only Functions

- `get-invoice`: Get details of a specific invoice
- `get-retailer-balance`: Check a retailer's current balance
- `get-investor-balance`: Check an investor's current balance
- `get-platform-fee`: Get the current platform fee percentage

## Usage Example

1. A retailer registers an invoice:
   ```
   (contract-call? .chain-finance register-invoice 'ST1SUPPLIER... u10000 u10000)
   ```

2. The retailer tokenizes the invoice:
   ```
   (contract-call? .chain-finance tokenize-invoice u1)
   ```

3. The retailer offers the invoice for sale at a 5% discount:
   ```
   (contract-call? .chain-finance offer-invoice-for-sale u1 u5)
   ```

4. An investor purchases the invoice:
   ```
   (contract-call? .chain-finance buy-invoice u1)
   ```

5. After the due date, the investor claims payment:
   ```
   (contract-call? .chain-finance claim-invoice-payment u1)
   ```

6. The retailer withdraws their funds:
   ```
   (contract-call? .chain-finance withdraw-retailer-funds)
   ```

## Error Codes

- `u100`: Owner only function
- `u101`: Item not found
- `u102`: Unauthorized operation
- `u103`: Item already exists
- `u104`: Invalid amount
- `u105`: Insufficient funds
- `u106`: Expired
- `u107`: Not for sale
- `u108`: Already claimed
