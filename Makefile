
# To enable features (for make and other, .e.g m4), set corresponding
# environment variable to non-empty value (e.g. 1).
mkdir		?= mkdir -pv
rmdir		?= rm -v -rf
rm		?= rm -v -f
# I should not use '-a' with `cp`, becuase in that case, when source and built
# file are the same (fallback build), timestamp on built file will not change
# after clean and rebuild.
cp		?= cp -v
install		?= install -v  -m 640
install_dir	?= install -v -m 750 -d

.SUFFIXES:

# Whether to backup installed files, if they differ from new files. Note, that
# `g_install` compares built version of file, which it wants to install, with
# already installed version. Particularly, it does not try to determine
# whether current installed version comes from previous git revision. For
# determining is installed file comes from some revision in git repository i
# may use something like:
#
# 	git rev-list --all | xargs -IREPL git ls-tree REPL -- src/Xsession | uniq \
# 		| grep $(git hash-object ~/.Xsession)
#
# but anyway, this will only work for files installed "as is" (e.g. shell
# scripts), but not for built files (m4).
backup_install	?=

# Arguments for building solarized.m4 .
solarized_args	:= Dark Light pref

src_dir		:= src
build_dir	:= build

appdefaults_dir		:= $(HOME)/.app-defaults
xresources		:= XTerm Test
installed_xresources	:= $(addprefix $(appdefaults_dir)/, $(xresources)) 

xsession_dir		:= $(HOME)/.Xsession.d
xsession		:= Xsession
xsession_scripts	:= run_cmd.sh
installed_xsession	:= $(HOME)/.Xsession $(addprefix $(xsession_dir)/, $(xsession_scripts))

installed_files		:= $(installed_xresources) $(installed_xsession)


## Build.
# Check is any of listed make variables are set, and, if so, return m4
# command-line arguments setting corresponding m4 macroses to make variables
# value,
argv_m4 = $(foreach v,$(1),$(if $(value $(v)),-D $(v)=$(value $(v))))

# I built solarized.m4 differently (with different prefix 'pref') for
# different X resources.
$(build_dir)/%-solarized : $(src_dir)/Xresources/solarized.m4
	$(mkdir) $(dir $@)
	m4 -P $(call argv_m4, $(solarized_args)) $< > $@

$(build_dir)/% : $(src_dir)/%.m4
	$(mkdir) $(dir $@)
	m4 -P $(call argv_m4, $($(notdir $*)_args)) $< > $@

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

$(build_dir)/Xresources/XTerm : pref ?= '*VT100'
$(build_dir)/Xresources/XTerm : $(build_dir)/Xresources/XTerm-solarized
	cat $(src_dir)/Xresources/XTerm $< > $@

# Test build with different 'pref'.
$(build_dir)/Xresources/Test : pref ?= '*VT'
$(build_dir)/Xresources/Test : $(build_dir)/Xresources/Test-solarized
	cat $< > $@

.PHONY: Xresources
Xresources : $(addprefix $(build_dir)/Xresources/, $(xresources))

## Install.
# Generic install recipe, which checks whether file already exists and differs
# from new one, and, if so, instructs `install` to create a numbered backup.
# Args:
# 1 - source file. If not present, will use the first prerequisite.
# I need, that 'if' and following file `install` command execute in *one*
# shell, otherwise 'inst_opts' shell variable set in 'if' would not be
# available at 'install' invocation. Thus, i need to group them into one shell
# command.
define g_install
	$(install_dir) -d $(dir $@)
	if ! diff -q $@ $(if $(1), $(1), $<); then \
	  inst_opts="--backup=numbered"; \
	fi; \
	$(install) $(if $(backup_install),$$inst_opts) -T $(if $(1), $(1), $<) $@
endef

# List file backups, created by `g_install` .
define g_list_old_files
	$(foreach f, $(1), @find $(dir $(f)) -maxdepth 1 -type f -name '$(notdir $(f)).~*~' | sort;
)
endef

$(installed_xresources) : $(appdefaults_dir)/% : $(build_dir)/Xresources/%
	$(call g_install, $(build_dir)/Xresources/$*)

$(xsession_dir)/%.sh : $(build_dir)/%.sh
	$(call g_install)
# See above note about intermediate files.
.INTERMEDIATE: $(build_dir)/$(xsession)
$(HOME)/.Xsession : $(build_dir)/$(xsession)
	$(call g_install)

.PHONY: install_Xresources
install_Xresources : $(installed_xresources)
	@echo "@@@ Backups of X resource files:"
	$(call g_list_old_files, $^)
	@echo "@@@"

.PHONY: install_Xsession
install_Xsession : $(installed_xsession)
	@echo "@@@ Backups of Xsession files:"
	$(call g_list_old_files, $^)
	@echo "@@@"

.PHONY: install
install : install_Xresources install_Xsession

.PHONY: list_old_files
list_old_files :
	@echo "@@@ Backups of installed files:"
	$(call g_list_old_files, $(installed_files))
	@echo "@@@"


# See 4.7 "Rules without Recipes or Prerequisites" in make manual.
FORCE:

clean :
	$(rmdir) $(build_dir)

remove :
	$(rm) $(installed_files)

