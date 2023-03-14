ICONS=.icons

.PHONY: all
all: vim.PluginInstall tmux.TpmInstall .themes/Nordic $(ICONS)/Papirus git.Config npm.Config nix.Profile

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
	xfconf-query -c xsettings -p /Net/IconThemeName -s Papirus-Dark

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

.PHONY: nix.Profile
nix.Profile:
	nix profile install .

.PHONY: clean
clean: .themes/Nordic/clean $(ICONS)/Papirus/clean
	rm .local/bin/dir_colors
	rm -Rf .vim/autoload/plug.vim .tmux/plugins/tpm 
