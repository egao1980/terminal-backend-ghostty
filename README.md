# terminal-backend-ghostty

[`terminal-protocol`](https://github.com/egao1980/terminal-protocol) backend on [libghostty-vt 1.0](https://mitchellh.com/writing/libghostty-1-0), through [`ghostty-vt`](https://github.com/egao1980/ghostty-vt). Loading it sets `*terminal-backend*` unless a backend is already selected.

```lisp
(asdf:load-system "terminal-backend-ghostty")

(stack-terminal:with-terminal (term :cols 80 :rows 24)
  (stack-terminal:terminal-feed term (babel:string-to-octets "hi"))
  (stack-terminal:terminal-snapshot term))
```

| Protocol | libghostty-vt |
|----------|---------------|
| `terminal-feed` | `ghostty_terminal_vt_write`. Octets are pinned, not copied, when they're a simple `(unsigned-byte 8)` vector. |
| host effects | `OPT_WRITE_PTY` / `BELL` / `TITLE_CHANGED` / `PWD_CHANGED` / `CLIPBOARD_WRITE` / `CLIPBOARD_READ` / `DESKTOP_NOTIFICATION` / `COLOR_SCHEME` / `SEMANTIC_PROMPT` callbacks. Userdata is an integer id, and every effect goes through `call-host`. |
| frames | `GhosttyRenderState` + row iterator + row cells, copied into fresh Lisp rows on `frame-update` |
| `encode-key` / `encode-mouse` / `encode-focus` / `encode-paste` | key / mouse encoders reconfigured from the terminal before each event, plus `focus_encode` and `paste_encode` |
| `terminal-text :format :vt` / `:html` | `GhosttyFormatter` over the whole screen (`:region :screen`) |

Features: `:scrollback :alternate-screen :kitty-keyboard :osc-52 :osc-133 :format-vt :format-html`.

Not yet: OSC 8 hyperlink URIs in cells (`style-hyperlink` is NIL), selection, search, Kitty graphics.

Native handles are freed by `terminal-close` / `frame-close`. A finalizer frees anything you forget.

## License

MIT
