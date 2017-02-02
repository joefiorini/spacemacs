(defvar flow-type-no-auto-start nil
  "Set flow server auto-start behaviour.

Possible values:

nil
  do nothing

not nil
  add `--no-auto-start' to all relevant flow commands

'process
  add `--no-auto-start' to all relevant flow commands, and on entry to a
  `js2-mode' buffer call `flow-type/ensure-server-buffer' which runs `flow
  server' in a project specific comint buffer")
