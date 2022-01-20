# hledger-toolbox
CLI tools and Haskell libraries for working with hledger

## Import data from YNAB API
Run the `ynab_import` via `cabal`:
```
$ cabal run ynab_import -- -h
Usage: ynab_import OUTPUT [-n|--no-pull-data] [-s|--settings SETTINGS] 
                   [-d|--db-dir DBS_DIR]
  YNAB data adapter for hledger

Available options:
  OUTPUT                   Output file path
  -n,--no-pull-data        Do not pull data from API
  -s,--settings SETTINGS   Path to the settings file
                           (default: "./.secrets/ynab_api_import_settings.json")
  -d,--db-dir DBS_DIR      Path to the base dir of local databases
                           (default: "./.secrets/dbs")
  -h,--help                Show this help text
```

As the usage info indicates, you need to provide a output file. Use `-` to print
the journal output to stdout. You also need to provide a JSON setting file. An
example is here:
```json
{
  "ynab_api_settings": {
    "api_base_url": "https://api.youneedabudget.com/v1",
    "api_token": "<your_API_token>"
  },
  "budget_name": "<budget_name>",
  "starting_balance_account": "equity:starting balance",
  "transfer_account": "assets:transfer",
  "account_map": {
    "Checking": "assets:cash:checking",
    "Savings": "assets:cash:savings",
    "Credit Card": "liabilities:short term:credit card"
  }
}
```
The `account_map` field is a map from YNAB account names to `hledger` account
names.

When the program pulls data from the YNAB API, it creates a local SQLite
database file named after the budget's ID. When it tries to create the journal,
it will use `expenses:<Category Group>:<Category>` as expense accounts and
`revenues:income:<Payee>` as revenue accounts.
