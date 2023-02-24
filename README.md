maelstrom-hs
---

Attempting to work through [maelstrom][maelstrom] in Haskell.

Running the demo
---

```
maelstrom test -w echo --bin demo/echo.rb --time-limit 5
```

Running the echo server
---

```
maelstrom test -w echo --bin (cabal exec which maelstromHs) --nodes n1 --time-limit 10 --log-stderr
```

There is also `make maelstrom` but we'll need to separate the various
executables because `maelstromHs` is just the echo server at the moment.

TODO
---

- [ ] Data modeling for [maelstrom-protocol][this protocol]
  - [ ] Need to start `src/Maelstrom/Types/Message.hs`
  - [ ] Need to start `src/Maelstrom/Types/Body.hs`

[maelstrom]: https://github.com/jepsen-io/maelstrom
[maelstrom-protocol]: https://github.com/jepsen-io/maelstrom/blob/main/doc/protocol.md
