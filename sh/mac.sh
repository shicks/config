# Mac-specific customizations
if [ -d /Users/sdh ]; then
  # TODO(sdh): are there bash versions of this?
  case "$SHELL" in
    (*zsh*)
      # The next line updates PATH for the Google Cloud SDK.
      source '/Users/sdh/local/opt/google-cloud-sdk/path.zsh.inc'

      # The next line enables shell command completion for gcloud.
      source '/Users/sdh/local/opt/google-cloud-sdk/completion.zsh.inc'
      ;;
  esac

  PATH=$HOME/local/bin:$PATH
  export DYLD_LIBRARY_PATH=/Users/sdh/local/opt/rust/lib/rustlib/x86_64-apple-darwin/lib
fi
