
include gmsl
include Makefile-common

# To enable features (for make and other, .e.g m4), set corresponding
# environment variable to non-empty value (e.g. 1).
mkdir		?= mkdir -pv
rmdir		?= rm -v -rf
rm		?= rm -v -f
# I should not use '-a' with `cp`, becuase in that case, when source and built
# file are the same (fallback build), timestamp on built file will not change
# after clean and rebuild.
cp		?= cp -v

# Haskell's `arch` from System.Info defines 32-bit architecture as i386, so
# should i.
uname_M 	:= $(subst i686,i386,$(shell uname -m))
uname_S 	:= $(call lc,$(shell uname -s))

.SUFFIXES:

src_dir		:= src
build_dir	:= build

xsession_dir		:= $(HOME)/.Xsession.d
xsession		:= Xsession
xsession_scripts	:= run_cmd.sh xkb_ctrl_esc_to_super.sh
installed_xsession	:= $(HOME)/.Xsession $(addprefix $(xsession_dir)/, $(xsession_scripts))

bin_path		:= $(HOME)/bin
home_local_path		:= $(HOME)/.local
stack_bin_path		:= $(HOME)/.local/bin
installed_xmonad	:= $(stack_bin_path)/xmonad $(HOME)/.xmonad xmonad-$(uname_M)-$(uname_S)

xmobar_configs		:= xmobarrc xmobarrcAlt
installed_xmobar 	:= $(stack_bin_path)/xmobar $(HOME)/.xmobarrc $(HOME)/.xmobarrcAlt

installed_files		:= $(installed_xsession) $(installed_xmonad) $(installed_xmobar)


## Build.
# Warning: following implicit rules create intermediate files! See 10.4
# "Chains of Implicit Rules". That means:
# - If they (built files) don't exist, `make install` won't create them and
#   compare target (installed) file timestamp with source file timestamp
#   (avoiding built files).
# - After using them (e.g. during `make install`) `make` will delete these
#   (built) files.
#
# But because these files are not compiled and just copied "as is", there is
# no sense in taking them into account during install. Though, if i want to
# treat them as *not* intermediata, i may rewrite following implicit rules to
# static pattern rules and delete some (of them) from .INTERMEDIATE below.
$(build_dir)/%.sh :: $(src_dir)/%.sh
	$(mkdir) $(dir $@)
	$(cp) $< $@

# Fallback build (terminal rule).
$(build_dir)/% :: $(src_dir)/%
	$(mkdir) $(dir $@)
	$(cp) $< $@

.PHONY: build_xmonad
build_xmonad : 
	stack --install-ghc build


## Install.

# List file backups, created by `g_install` .
define g_list_old_files
	$(foreach f, $(1), @find $(dir $(f)) -maxdepth 1 -type f -name '$(notdir $(f)).~*~' | sort;
)
endef

$(xsession_dir) :
	$(install_dir) $@

$(xsession_dir)/%.sh : $(build_dir)/%.sh $(xsession_dir)
	$(call install_file)
# See above note about intermediate files.
.INTERMEDIATE: $(build_dir)/$(xsession)
$(HOME)/.Xsession : $(build_dir)/$(xsession)
	$(call install_file)

.PHONY: install_Xsession
install_Xsession : $(installed_xsession)
	@echo "@@@ Backups of Xsession files:"
	$(call g_list_old_files, $^)
	@echo "@@@"

$(bin_path) $(home_local_path) :
	$(install_dir) $@

$(stack_bin_path) : $(bin_path) $(home_local_path)
	$(call install_link)

$(HOME)/.xmonad : FORCE
	$(call install_link,$(CURDIR))

xmonad-$(uname_M)-$(uname_S): $(stack_bin_path)/xmonad FORCE
	$(call install_link,$(stack_bin_path)/xmonad)

$(stack_bin_path)/xmonad $(stack_bin_path)/xmobar : build_xmonad $(stack_bin_path)
	stack install

.PHONY: install_xmonad
install_xmonad : $(installed_xmonad)
	@echo "@@@ Backups of xmonad files:"
	$(call g_list_old_files, $^)
	@echo "@@@"

# I need non-empty stem for all matching config names.
$(HOME)/.xmobar% : $(build_dir)/xmobar%
	$(call install_file)

.PHONY: install_xmobar
install_xmobar : $(installed_xmobar)
	@echo "@@@ Backups of xmobar files:"
	$(call g_list_old_files, $^)
	@echo "@@@"

.PHONY: install
install : install_Xsession install_xmonad install_xmobar

.PHONY: list_old_files
list_old_files :
	@echo "@@@ Backups of installed files:"
	$(call g_list_old_files, $(installed_files))
	@echo "@@@"


# See 4.7 "Rules without Recipes or Prerequisites" in make manual.
FORCE:

clean :
	$(rmdir) $(build_dir)
	stack clean

remove :
	$(rm) $(installed_files)

