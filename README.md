# lisp-devcontainer

```bash
$ rlwrap sbcl
```

```lisp
(load "maze.lisp")
```

## Dungeon console game

```bash
sbcl --script maze.lisp
```

- `h` / `j` / `k` / `l`: move without Enter
- `y` / `u` / `b` / `n`: move diagonally
- `Q`: quit
- `@`: player
- `|` / `-`: wall
- `+`: door
- `#`: corridor
- `.`: room floor
- `%`: stairs to the next floor
- `,`: Amulet of Yendor

The dungeon uses Rogue-style ASCII symbols.
