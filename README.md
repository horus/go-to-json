# Go-to-JSON

Online json.Marshal as opposed to json-to-go

## Usage

Please click the [link](https://horus.github.io/go-to-json/) to the right and paste your `struct` definitions.

## Is there anything...

Yes.

People asked the same question [here](https://github.com/mholt/json-to-go/issues/121) and [here](https://github.com/mholt/json-to-go/issues/62).

This repo was born out of a request by one of my colleagues: how to marshal Go structs to corresponding JSON data, as with:

```go
import "encoding/json"
output, err := json.Marshal(v)
```

...but online. You don't have to involve the Go compiler to get the result.

So the first version was implemented in Haskell as a command-line tool. Later, I added an HTTP API, read some Elm tutorials, and then wrote a web UI for it. It met my expectations.

However, I didn't stop there. I rewrote everything in pure Elm to take advantage of Github Pages. I strive to keep it as close to the one-true implementation as possible, but it will be opinionated in some corner cases.

Feel free to try it out.

Overall, this tool is a hobbyist's creation. I have been working on it since 2020, and I hope it reaches a wider audience and helps those in need.

## Implementations

| Branch                          | Language                                                               |
| ------------------------------- | ---------------------------------------------------------------------- |
| [archive](../../tree/archive)   | Haskell (back) + Elm (front)                                           |
| [main](../../tree/main)         | pure Elm                                                               |
| [ghcjs](../../tree/ghcjs)       | 98% Haskell (w./ GHC 9.8.1 JavaScript Backend) + 1% HTML  + 1% JS shim |

## Got Questions?

Issues or PRs welcome.
