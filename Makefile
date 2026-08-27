ICONS=.icons

.PHONY: all
all: nix.Profile xmonad.Config vim.PluginInstall tmux.TpmInstall .themes/Nordic $(ICONS)/Papirus git.Config npm.Config

.themes/Nordic:
	curl -L -s https://github.com/EliverLara/Nordic/releases/latest/download/Nordic.tar.xz | tar -xJC .themes
	gsettings set org.gnome.desktop.interface gtk-theme "Nordic"
	gsettings set org.gnome.desktop.wm.preferences theme "Nordic"
	xfconf-query -c xsettings -p /Net/ThemeName -s "Nordic"

.PHONY: .themes/Nordic/clean
.themes/Nordic/clean:
	gsettings set org.gnome.desktop.interface gtk-theme "Adwaita"
	gsettings set org.gnome.desktop.wm.preferences theme "Adwaita"
	xfconf-query -c xsettings -p /Net/ThemeName -s "Default"
	rm -Rf .themes/Nordic

$(ICONS)/Papirus:
	curl -L -s https://git.io/papirus-icon-theme-install | DESTDIR=$(ICONS) sh
	-mkdir .papirus-nord
	curl -L -s https://github.com/Adapta-Projects/Papirus-Nord/releases/latest/download/Papirus-Nord.tar.xz | tar -xJC .papirus-nord
	cd .papirus-nord && (yes "N" | ./install) && (./papirus-folders -C polarnight1 --theme Papirus-Dark)
	-rm -r .papirus-nord
	gsettings set org.gnome.desktop.interface icon-theme "Papirus-Dark"
	xfconf-query -c xsettings -p /Net/IconThemeName -s Papirus-Dark --create

.PHONY: $(ICONS)/Papirus/clean
$(ICONS)/Papirus/clean:
	curl -L -s https://git.io/papirus-icon-theme-install | DESTDIR=$(ICONS) uninstall=true sh
	xfconf-query -c xsettings -p /Net/IconThemeName -s Adwaita

# 	gtk-update-icon-cache

.local/bin/dir_colors:
	curl -L -s https://github.com/arcticicestudio/nord-dircolors/releases/latest/download/dir_colors --output $@
	chmod +x $@

.vim/autoload/plug.vim:
	curl -fLo .vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

.tmux/plugins/tpm:
	git clone https://github.com/tmux-plugins/tpm .tmux/plugins/tpm

.PHONY: tmux.TpmInstall
tmux.TpmInstall: .tmux/plugins/tpm
	.tmux/plugins/tpm/scripts/install_plugins.sh

.PHONY: vim.PluginInstall
vim.PluginInstall: .vim/autoload/plug.vim
	vim +PluginInstall +qall

.PHONY: git.Config
git.Config:
	git config --global mergetool.fugitive.cmd 'vim -f -c "Gdiffsplit!" "$$$\MERGED"'
	git config --global merge.tool fugitive
	git config --global mergetool.keepBackup false
	git config --global init.defaultBranch main

.PHONY: npm.Config
npm.Config:
	npm config set prefix "${HOME}/.npm-packages"

.PHONY: xmonad.Config
xmonad.Config: .local/bin/xmonad-session

.PHONY: xmonad.LemursInstall
xmonad.LemursInstall: .local/bin/xmonad-session
	install -Dm755 $< /etc/lemurs/wms/xmonad

.PHONY: nix.Profile
nix.Profile:
	nix --extra-experimental-features "nix-command flakes" profile install .

# `make test` is the standard verification entrypoint for this repo.
# It runs `nix flake check` (static config + no-network self-tests)
# followed by `.local/bin/*-smoke-test` scripts (out-of-band live
# checks that require network and user secrets). Smoke tests are
# enabled by default and can be disabled via SMOKE_TESTS_ENABLED=false
# (or the legacy SKIP_SMOKE=1 alias for backwards compatibility).
#
# Usage:
#   make test                          Run flake check + smoke tests (default).
#   make test SMOKE_TESTS_ENABLED=false Run flake check only (CI default).
#   SMOKE_TESTS_ENABLED=false make test Equivalent.
#   make test SKIP_SMOKE=1             Backwards-compatible alias for disabling smoke.
#
# E2E/integration smoke tests that need live services/secrets/network
# MUST live as `.local/bin/*-smoke-test` scripts. They are out-of-band
# and must never be added to `nix flake check`.
.PHONY: test
test: flake.check smoke

.PHONY: flake.check
flake.check:
	nix --extra-experimental-features "nix-command flakes" flake check

.PHONY: fmt
fmt:
	nix --extra-experimental-features "nix-command flakes" fmt

.PHONY: smoke
smoke:
	@set -e; \
	if [ -n "$$SKIP_SMOKE" ]; then \
		smoke_enabled=0; \
	else \
		case "$${SMOKE_TESTS_ENABLED:-true}" in \
			1|true|TRUE|yes|YES|on|ON) smoke_enabled=1 ;; \
			0|false|FALSE|no|NO|off|OFF|"") smoke_enabled=0 ;; \
			*) echo "smoke: invalid SMOKE_TESTS_ENABLED='$$SMOKE_TESTS_ENABLED'" >&2; exit 2 ;; \
		esac; \
	fi; \
	if [ "$$smoke_enabled" != "1" ]; then \
		echo "smoke: skipped (SMOKE_TESTS_ENABLED=$${SMOKE_TESTS_ENABLED:-false} SKIP_SMOKE=$${SKIP_SMOKE:-})"; \
		exit 0; \
	fi; \
	for t in .local/bin/*-smoke-test; do \
		[ -x "$$t" ] || continue; \
		echo "smoke: running $$t api-token"; \
		if ! "$$t" api-token; then \
			echo "smoke: $$t api-token FAILED" >&2; \
			exit 1; \
		fi; \
		echo "smoke: running $$t oauth"; \
		if ! "$$t" oauth; then \
			echo "smoke: $$t oauth FAILED" >&2; \
			exit 1; \
		fi; \
	done

.PHONY: clean
clean: .themes/Nordic/clean $(ICONS)/Papirus/clean
	rm .local/bin/dir_colors
	rm -Rf .vim/autoload/plug.vim .tmux/plugins/tpm
	rm -f .local/share/applications/xmonad.desktop
