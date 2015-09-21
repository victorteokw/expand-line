Expand line [![MELPA](http://melpa.org/packages/expand-line-badge.svg)](http://melpa.org/#/expand-line)
===========

Expand line works like `expand-region`, instead of `region`, this package expand `line`.

## Usage

Set a key binding for `turn-on-expand-line-mode`.

``` elisp
(global-set-key (kbd "s-l") 'turn-on-expand-line-mode)
(global-set-key (kbd "C-c l") 'turn-on-expand-line-mode)
```

And then you can expand with `C-p`, `C-n`, contract with `M-p`, `M-n`.

By `C-g`, you will return back to the point expand started.

If you want to deactivate the region and don't want to go back to the point
where it started, you can press `M-g`.

By given prefix arguments, you can expand or contract by more than 1 line.

You can even call `turn-on-expand-line-mode` when region is active, in this
case, region will not expand for you, but you get expand line behavior and
keybindings for `C-n`, `C-p`, `M-n`, `M-p`, `C-g` and `M-g`.

## Contribution

Any contribution is welcome. If you find any bugs, please report it via issue
or submit a pull request.
