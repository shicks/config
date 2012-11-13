function strip_semis {
  # usage: strip_semis <var_name>
  # eliminates any extra semicolons, as well as any spaces
  # before semicolons and any extra spaces after semis
  local prev
  eval "prev=\"\$$1\""
  eval "$1=\"\${$1#;}\""
  eval "$1=\"\${$1# }\""
  eval "$1=\"\${$1% }\""
  eval "$1=\"\${$1%;}\""
  eval "$1=\"\${$1// ;/;}\""
  eval "$1=\"\${$1//;  /; }\""
  eval "$1=\"\${$1//;;/;}\""
  local curr
  if [ "$prev" = "$(eval echo "\"\$$1\"")" ]; then
    return
  fi
  strip_semis "$1"
}

function add_command {
  # usage: add_command <var_name> <command>
  # example: add_command PROMPT_COMMAND "echo \"hello $USER\""
  local cmd="$2"
  strip_semis cmd  # clean up $cmd before searching for it
  strip_semis "$1" # clean up $$1
  eval "$1=\"; \$$1;\""  # add ; to both ends so we can match on it
  eval "$1=\"\${$1//; \$cmd;/;}\""
  # Add it back to the end
  eval "$1=\"\$$1; \"'$cmd'"
  # Now clean up any extra semis
  strip_semis "$1"
}

function ensure_function {
  if ! type "$1" &> /dev/null; then
    eval "function $1 { :; }"
  fi
}
