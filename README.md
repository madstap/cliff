# cliff

Concise syntax for writing command line apps with babashka.
Can also be used from the JVM.

Built on top of [tools.cli](https://github.com/clojure/tools.cli).

Not yet stable, so doesn't have a release. Can be used as a git dep.

Quickstart:
```clojure
#!/usr/bin/env bb

(require '[babashka.deps :as deps])

(deps/add-deps
 '{:deps {dev.madland/cliff
          {:git/url "https://github.com/madstap/cliff.git"
           :sha     "<current git sha>"}}})

(ns example
 (:require [dev.madland.cliff :as cliff]))
;; TODO: Add small example script.
```
## Rationale

## Usage

A command (or subcommand) is a vector of `[command-name ?config-map & subcommands]`.

The config-map specifies the behavior of the command and/or subcommands.

(Sub)commands nest arbitrarily.

#### `:handler`

For a command to be callable it needs a `:handler`, which is a function that
takes a context map and executes the command. Since this is

#### `:fx`

Since many command line apps either print something or print many somethings,
each on a new line, there is the `:fx` key which does this with what the handler
returns. Some valid values for `:fx` are
`:println`, `:print-lines`, `:prn`, `:pr-lines` and `:pprint`.
This makes the handler easier to test, since it returns a value instead of
performing a side-effect.

#### `:opts`

:opts is used to specify option flags. Uses the same syntax as
clojure.tools.cli, with some small differences in semantics and some extra
functionality.

See [opts](#opts-1) for more.

#### `:args`

Used to specify arguments to a command.

See [args](#args-1) for more.

#### `:desc`

A short description. Is used as the first line of the commands help
text and also as the description in the list of subcommands in the
parent commands help text.

#### `:doc`

A more detailed description of the command. Is printed alongside `:desc`, so
there's no need to repeat `:desc` in `:doc`. Can be either a string or
a list of strings, which are joined with newlines.

#### `:version`

Will add a --version flag to the command that prints the version and exits with 0.

### Opts

In addition to the keys clojure.tools.cli uses, an option can specify:

#### `:type`
The type of the value (for flags with a required value).
See [Types](#Types) for more.

##### Differences with clojure.tools.cli

TODO


### Args

Like opts, arguments have an :id.
The list of args can end with either an optional arg or varargs.

#### `:optional`
Whether the argument is optional, default false for normal args and
true for varargs.

#### `:varargs`
Whether the argument is cardinality 0.n (or 1.n if :optional is false).

#### `:type`
The type of the argument.
See [Types](###Types) for more.


### Types
There are many convenient built-in types. There is not yet a way to add your own
types, but this is planned.

TODO: List of types.

### Autocomplete


## Prior art
