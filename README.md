# lisp-devcontainer

```bash
$ rlwrap sbcl
```

```lisp
(load "maze.lisp")
```

## Maze console game

```bash
sbcl --script maze.lisp
```

- `W` / `A` / `S` / `D`: move without Enter
- `Q`: quit
- `@`: player
- `G`: goal
- `#`: wall

The maze uses ANSI colors in compatible terminals.
