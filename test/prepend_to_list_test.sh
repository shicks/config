set -e

source $(dirname $0)/harness.sh
source $(dirname $0)/../sh/00_functions.$(basename $SHELL)

# Basic functionality
prepend_to_list x foo bar
assert_eq "bar:foo" "$x"

prepend_to_list x foo
assert_eq "foo:bar:foo" "$x"

prepend_to_list -u x foo
assert_eq "foo:bar" "$x"

prepend_to_list -u x bar baz
assert_eq "baz:bar:foo" "$x"

prepend_to_list x foo
assert_eq "foo:baz:bar:foo" "$x"

prepend_to_list -u x qux
assert_eq "qux:foo:baz:bar:foo" "$x"

prepend_to_list -u x foo
assert_eq "foo:qux:baz:bar" "$x"

# Substrings not removed
x='abcdef'
prepend_to_list -u x abc
assert_eq "abc:abcdef" "$x"

prepend_to_list -u x def
assert_eq "def:abc:abcdef" "$x"

prepend_to_list -u x bcd
assert_eq "bcd:def:abc:abcdef" "$x"

# Existence checks, multiple arguments
f=$(tempfile)
rm "$f"
mkdir "$f"

prepend_to_list -e y "$f/a"
assert_eq "" "$y"

prepend_to_list -e y "$f"
assert_eq "$f" "$y"

prepend_to_list -e y "$f"
assert_eq "$f:$f" "$y"

prepend_to_list -eu y "$f"
assert_eq "$f" "$y"

prepend_to_list -ue y "$f"
assert_eq "$f" "$y"

prepend_to_list -u -e y "$f"
assert_eq "$f" "$y"

prepend_to_list -e -u y "$f"
assert_eq "$f" "$y"

touch "$f/a"
prepend_to_list -e y "$f/a"
assert_eq "$f/a:$f" "$y"

rm -rf "$f"

teardown 18
