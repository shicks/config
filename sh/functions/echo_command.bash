echo -ne '\e[1;30m'
echo -n "$@" # TODO(sdh): do a better job escaping?
echo -e '\e[m'
