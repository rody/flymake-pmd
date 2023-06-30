# flymake-pmd - A Flymake backend using [PMD](https://pmd.github.io)

`flymake-pmd` integrates [PMD](https://pmd.github.io) with [flymake](https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/flymake.el). It can be configured to use PMD 6.x and 7.x.
`flymake-pmd` can be used to analyse any language supported by PMD.

## Features

 - Supports PMD 6.x and 7.x
 - Auto detection of custom rules files 
 - Can be used with Eglot
 
## Installation

### Manually

Download `flymake-pmd.el` or clone this repository and add it to your load-path.

### from Melpa

Coming soon...

## Activation

To activate `flymake-pmd` on a buffer: `M-x flymake-pmd-enable`.

You can also add a hook to a major mode to enable `flymake-pmd` automatically:
``` emacs-lisp
(add-hook 'java-mode-hook 'flymake-pmd-enable)
```

## Configuration

Here is an example with `use-package`:

``` emacs-lisp
(use-package flymake-pmd
  :hook (apex-ts-mode . flymake-pmd-enable)
  :init
  ;; name of the PMD executable (defaults to 'pmd')
  ;; you can also provide a full path if pmd is not in the exec-path
  (setq flymake-pmd-executable-name "~/pmd-bin-6.55.0/bin/run.sh")
  
  ;; When configured to use flymake, Eglot replaces
  ;; all the diagnostics functions by its own.
  ;; Setting `flymake-pmd-use-eglot` to t ensure that
  ;; flymake-pmd will be added back to the diagnostic
  ;; functions after eglot has been initialised
  (setq flymake-pmd-use-eglot t)
  
  ;; to use PMD 6.x instead of 7.x, use the following
  (setq flymake-pmd-use-pmd-6 t)
  ;; PMD 6.x cli differs between windows and unix-like OSes.
  ;; On windows, the pmd command can be called directly and
  ;; `flymake-pmd-pmd-6-app-name should be "" (empty string).
  ;; On macos and unix, the run.sh script provided by PMD
  ;; needs the name of the app as a parameter.
  (setq flymake-pmd-pmd-6-app-name "pmd"))
```

At runtime `flymake-pmd` tries to discover the ruleset file to use. It is looking for a file in the same directory or any parent directory as the current on which matches the following names: "pmd.xml", "pmd-ruleset.xml" or "ruleset.xml". If no such file is found, an error will be raised.

This list of name can be configured by changing the value of `flymake-pmd-ruleset-filename-list`.

## Limitations

 - PMD uses file extensions to detect which language is used during the analysis. This means that `flymake-pmd` only works on buffer that are visiting an existing file. Behind the scene `flymake-pmd` creates a temporary file with the same extension as the original file and send that to PMD for analysis. This allows the language detection to work correctly and allows to check unsaved changes in the buffer.
 
## TODO

 - [ ] allow the usage of external rules/jar
 
## Contributions

Contributions are welcome! Feel free to open an issue or raise a PR.

## Thanks

 - The author of [flymake-eslint](https://github.com/orzechowskid/flymake-stylelint) from whom I took a lot of inspiration.
 - The authors and maintainers of Flymake and Emacs
