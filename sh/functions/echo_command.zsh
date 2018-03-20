local cmd=("$@")
print -P "%B%F{black}${(q-)cmd[@]}%b%f"
