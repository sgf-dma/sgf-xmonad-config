#!/bin/sh

if [ "${include_xkb_ctrl_esc_to_super:-}" = "1" ]; then
    return 0
fi
readonly include_xkb_ctrl_esc_to_super=1

xkb_ctrl_esc_to_super()
{
    local xkb_user_dir="$HOME/.xkb"
    local xkb_rules_user_dir="$xkb_user_dir/rules"
    local xkb_rules_sys_dir='/usr/share/X11/xkb/rules'

    if ! setxkbmap -query >/dev/null 2>&1; then
	echo "XKB extension not supported." 1>&2
	return 0
    fi

    if [ ! -f "$xkb_user_dir/symbols/ctrl_esc_to_super" ]; then
	echo "Symbols definition for 'ctrl_esc_to_super' not found." 1>&2
	return 0
    fi

    rules="$(setxkbmap -query | sed -r -ne 's/^rules:[[:space:]]+//p')"
    user_rules="$xkb_rules_user_dir/${rules}.ctrl_esc_to_super"
    echo "Composing user XKB rules with ctrl_esc_to_super to '$user_rules'."
    mkdir -pv "$xkb_rules_user_dir"
    cp -v "$xkb_rules_sys_dir/$rules" "$user_rules"
    cp -v "$xkb_rules_sys_dir/$rules.lst" "$user_rules.lst"
    sed -i -r -e '/^! option[[:space:]]+=[[:space:]]+symbols$/{
	    s/.*/&\n  ctrl_esc_to_super     =       +ctrl_esc_to_super/
	}' "$user_rules"

    echo "Reloading XKB config."
    setxkbmap -I"$xkb_user_dir" \
	    -rules "$(basename "$user_rules")" -option ctrl_esc_to_super -print \
	| xkbcomp -I"$xkb_user_dir" - "$DISPLAY"
}

