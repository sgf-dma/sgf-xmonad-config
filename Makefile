
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

src_dir		:= src
build_dir	:= build

xsession_dir		:= $(HOME)/.Xsession.d
xsession		:= Xsession
xsession_scripts	:= run_cmd.sh
installed_xsession	:= $(HOME)/.Xsession $(addprefix $(xsession_dir)/, $(xsession_scripts))

installed_files		:= $(installed_xsession)


## Build.
$(build_dir)/%.sh : $(src_dir)/%.sh
	$(mkdir) $(dir $@)
	$(cp) $< $@

# Fallback build.
$(build_dir)/% : $(src_dir)/%
	$(mkdir) $(dir $@)
	$(cp) $< $@

.PHONY: Xsession
Xsession : $(addprefix $(build_dir)/, $(xsession) $(xsession_scripts))


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

$(xsession_dir)/%.sh : $(build_dir)/%.sh
	$(call g_install)
$(HOME)/.Xsession : $(build_dir)/$(xsession)
	$(call g_install)

.PHONY: install_Xsession
install_Xsession : $(installed_xsession)
	@echo "@@@ Backups of Xsession files:"
	$(call g_list_old_files, $^)
	@echo "@@@"

.PHONY: install
install : install_Xsession

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

