function precmd {
  LASTSTATUS=$?
  # TODO(sdh): LASTSTATUS?
  # TODO(sdh): shift in
  PS1=$(repoline prompt --exit=$LASTSTATUS) # NOTE: also draws line
  # TODO(sdh): timer
  # TODO(sdh): history
}
