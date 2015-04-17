#!/bin/sh

if [ "${include_run_cmd:-}" = "1" ]; then
    return 0
fi
readonly include_run_cmd=1

# Run commands defined in variables passed as arguments.
# Args:
# 1.. - names (not values!) of variables, containing command definition.
run_cmd()
{
    local cmd=''
    for cmd in "$@"; do
	# Result will be empty, if either cmd or $cmd is unset or empty.
	eval "set -- ${cmd:+\${$cmd:+\$$cmd\}}"; 
	if which "${1:-}" >/dev/null ; then
	    echo "Run $cmd"
	    # I need `eval` for compatibility with ~/.fehbg format.
	    eval "$@" &
	else
	    echo "Can't run $cmd."
	fi
    done
}

