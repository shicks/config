failed=0
tests=0

function assert_eq {
  tests=$((tests+1))
  if [ "$1" = "$2" ]; then
    echo -ne "\e[1;32mOK\e[m $tests\t" >&2
    echo "$3" >&2
  else
    echo -ne "\e[1;31mFAIL\e[m $tests\t" >&2
    echo "$3" >&2
    echo "Expected:" >&2
    echo "$1" >&2
    echo "Actual:" >&2
    echo "$2" >&2
    echo >&2
    failed=$((failed+1))
  fi
}

function teardown {
  local expected=$1
  if [ "$tests" -ne "$expected" ]; then
    echo -ne "\e[1;31mFAIL\e[m - Missing $((expected-tests)) tests" >&2
    failed=true
    exit 2
  elif [ "$failed" -gt 0 ]; then
    echo -ne "\e[1;31mFAIL\e[m - $failed tests failed" >&2
    exit 1
  else
    echo "All tests passed" >&2
  fi
}
