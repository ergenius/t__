# t__
Erlang gettext translation application

[![Erlang CI](https://github.com/ergenius/t__/actions/workflows/erlang.yml/badge.svg)](https://github.com/ergenius/t__/actions/workflows/erlang.yml)

## News

## Features
- T__("I have a joke about Erlang, but it requires a prologue.")
- Fast.
- Simple. Reading this file should be enough for understanding most use case scenarios.
- Documented source code with comments and specs.
- Higly configurable with multiple PO sources and languages per erlang application or process.
- Supports contexts.
- Supports interpolation using familiar Erlang format control sequences (from io:format).
- Supports translating singular term with or without interpolation and with or without context.
- Supports translating plural term with or without interpolation and with or without context.
- Supports ETS tables based caching.

## Alternatives

- Erlang Gettext tools for multi-lingual capabilities: https://github.com/etnt/gettext

### Quick comparison with Erlang Gettext

| #   | | t__                  | gettext        |
|-----| --- |------------------|----------------|
| 1   | Single macro that handles everything | YES: T__ | NO: TXT & TXT2 |
| 2   | gettext contexts | YES | NO |
| 3   | gettext plural terms | YES: supports gettext plural formulas for languages with more than 2 plural forms | NO |
| 4   | Separate configurations for each application started at the same node | YES | NO |
| 5   | Diffrent language sources | YES: using application 'repositories' | YES: using diffrent languages servers |
| 6   | Developer mode/monitoring PO file changes | YES - automatically detecting PO changes | NO - requires manual reloading of the PO files |
| 7   | Cache | YES - ETS reads/writes on the calling process (faster) | YES - ETS reads/writes inside the translation gen_server |
| 8   | Parser passed validating Gettext PO samples database | YES | NO |

## Limitations
- Gettext plural-forms formulas are hardcoded using a database. Work is underway to bypass this limitation and provide a full interpretor of any arbitrary gettext C formula.

## Examples:

```erlang
```

```erlang
```

## Project roadmap

1. Continuously fixing bugs and tuning performance.
2. Writing more testing units.
3. Add more features.

## Erlang versions supported

t__ officially supports OTP release 20 and later.

Development takes place using OTP 25 release and tests are done on:
- 25.0.3
- 24.3.4
- 23.3.4
- 22.3.4
- 21.3.8
- 20.3.8

Unofficially, you may be able to use t__ with older Erlang versions. No guarantee included.

## Dependencies

None.

## Authors

- Madalin Grigore-Enescu (ergenius) <github@ergenius.com>

## License

t__ is available under the MIT license (see `LICENSE`).
