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

[maelstrom]: https://github.com/jepsen-io/maelstrom
