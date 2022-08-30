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

| #   | | t__ | gettext |
|-----| --- |-----|----|
| 1   | Single macro that handles everything | YES: T__| NO: TXT & TXT2 |
| 2   | gettext contexts | YES | NO |
| 3   | gettext plural terms | YES: supports gettext plural formulas for languages with more than 2 plural forms | NO |
| 4   | Separate configurations for each application started at the same node | YES | NO |
| 5   | Diffrent language sources | YES: using application 'repositories' | YES: using diffrent languages servers |
| 6   | Developer mode/monitoring PO file changes | YES: automatically detecting PO changes | NO: requires manual reloading of the PO files |
| 7   | Cache | YES: ETS reads/writes on the calling process (faster) | YES: ETS reads/writes inside the translation gen_server |
| 8   | Parser passed validating Gettext PO samples database | YES | NO |

## Limitations
- Gettext plural-forms formulas are hardcoded using a database. Work is underway to bypass this limitation and provide a full interpretor of any arbitrary gettext C formula.

## Examples:

### T__ macro examples

#### Singular terms

```erlang
?T__("I have a joke about Erlang, but it requires a prologue.").
```

You can use binaries and atoms, however it is not recommended.

```erlang
?T__(<<"Erlang is user-friendly, it’s just picky about its friends!">>).
?T__('Why can\'t you trust atoms? Because they make up everything!').
```

#### Singular with context

Context is useful for the translators to distinguish in between identical strings.

```erlang
?T__({"menu", "Save"}).
?T__({"menu", "Quit"}).
?T__({"button", "Save"}).
?T__({"button", "Cancel"}).
```

Context can also be used to create proper translations based on grammatical gender.

```erlang
?T__({"female", "The cat belong to him/her"}).
?T__({"male", "The cat belong to him/her"}).
```

#### Singular with repository

Repositories are used to have different translations sources directories for the same application.
For example let's assume your application has many HTML templates, each with his own translation directory.

```erlang
?T__({"template1", {"Simple term from repository template1"}}).
?T__({"template2", {"Simple term from repository template2"}}).
```

Repository can be combined with context also.

```erlang
?T__({"template1", {"menu", "Save"}}).
```

#### Singular terms with interpolation

```erlang
?T__("~4.2f", [3.56]).
```

#### Context for gramatical gender with interpolation

Context can also be used to create the proper translation based on grammatical gender combined with interpolation:

```erlang
?T__({"female", "Her/his name is ~s"}, ["Marry"]).
?T__({"male", "Her/his name is ~s"}, ["John"]).
```

#### Plural terms with interpolation:

```erlang
?T__(["~B user", "~B users"], [3]).
```

#### Plural terms with context and interpolation

```erlang
?T__({"female", ["~B file belongs to her/him", "~B files belong to her/him"]}, [3]).
?T__({"male", ["~B file belongs to her/him", "~B files belong to her/him"]}, [3]).
```

#### Plural terms with context, interpolation and repositories

```erlang
?T__({"template1", {"female", ["~B file belongs to her/him", "~B files belong to her/him"]}}, [3]).
?T__({"template1", {"male", ["~B file belongs to her/him", "~B files belong to her/him"]}}, [3]).
```

#### Translate using #t__p{} record

You can also specify everything using the #t__p{} record as a single parameter to the T__ macro.
This is actually the performance wise way of doing it. You will save some extra functions calls necessary
to understand your tuples. All #t__p{} fields except msg are optional.

```erlang
?T__(#t__p{msg = "Hello world"}).
?T__(#t__p{msg = "Her name is ~s", data = ["Marry"]}).
?T__(#t__p{language = "ro", context = "female", msg = "Her/his name is ~s", data = ["Marry"]}).
?T__(#t__p{repository = "module1", language = "ro", context = "male", msg = "Her/his name is ~s", data = ["John"]}).
?T__(#t__p{application=myapp, repository = "module1", language = "ro", context = "female", msg = "Her/his name is ~s", data = ["Marry"]}).
```

#### Macro with all possible parameters separated

For your convenience a macro also exists with all possible parameters:

```erlang
?T__(Application, Repository, Language, Context, Msg, Data).
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
