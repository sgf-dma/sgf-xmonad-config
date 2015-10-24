! Solarized color scheme for the X Window System
!
! http://ethanschoonover.com/solarized

m4_changequote(`[', `]')m4_dnl
m4_dnl Defaults to Dark.
m4_ifdef([Light], [m4_undefine([Dark])], [m4_define([Dark])])m4_dnl
m4_dnl Defaults to no prefix for X resources.
m4_ifdef([pref], [], [m4_define([pref],[])])m4_dnl

m4_dnl Common
m4_define([S_yellow], [#b58900])m4_dnl
m4_define([S_orange], [#cb4b16])m4_dnl
m4_define([S_red], [#dc322f])m4_dnl
m4_define([S_magenta], [#d33682])m4_dnl
m4_define([S_violet], [#6c71c4])m4_dnl
m4_define([S_blue], [#268bd2])m4_dnl
m4_define([S_cyan], [#2aa198])m4_dnl
m4_define([S_green], [#859900])m4_dnl

m4_dnl Dark
m4_ifdef([Dark], [[! Dark]
m4_define([S_base03], [#002b36])m4_dnl
m4_define([S_base02], [#073642])m4_dnl
m4_define([S_base01], [#586e75])m4_dnl
m4_define([S_base00], [#657b83])m4_dnl
m4_define([S_base0], [#839496])m4_dnl
m4_define([S_base1], [#93a1a1])m4_dnl
m4_define([S_base2], [#eee8d5])m4_dnl
m4_define([S_base3], [#fdf6e3])m4_dnl
])m4_dnl
m4_dnl Light
m4_ifdef([Light], [[! Light]
m4_define([S_base03], [#fdf6e3])m4_dnl
m4_define([S_base02], [#eee8d5])m4_dnl
m4_define([S_base01], [#93a1a1])m4_dnl
m4_define([S_base00], [#839496])m4_dnl
m4_define([S_base0], [#657b83])m4_dnl
m4_define([S_base1], [#586e75])m4_dnl
m4_define([S_base2], [#073642])m4_dnl
m4_define([S_base3], [#002b36])m4_dnl
])m4_dnl

! To only apply colors to your terminal, for example, prefix
! the color assignment statement with its name. Example:
!
! URxvt*background:            S_base03

pref*background:              S_base03
pref*foreground:              S_base0
pref*fading:                  40
pref*fadeColor:               S_base03
pref*cursorColor:             S_base1
pref*pointerColorBackground:  S_base01
pref*pointerColorForeground:  S_base1

pref*color0:                  S_base02
pref*color1:                  S_red
pref*color2:                  S_green
pref*color3:                  S_yellow
pref*color4:                  S_blue
pref*color5:                  S_magenta
pref*color6:                  S_cyan
pref*color7:                  S_base2
pref*color9:                  S_orange
pref*color8:                  S_base03
pref*color10:                 S_base01
pref*color11:                 S_base00
pref*color12:                 S_base0
pref*color13:                 S_violet
pref*color14:                 S_base1
pref*color15:                 S_base3

