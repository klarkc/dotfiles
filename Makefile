.PHONY: all
all: vim.PluginInstall tmux.TpmInstall nix.Install .themes/Nordic git.Config

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

.PHONY: nix.Install
nix.Install: nix.LorriInstall nix.DirenvInstall

.PHONY: nix.LorriInstall
nix.LorriInstall:
	nix-env -i lorri 

.PHONY: nix.DirenvInstall
nix.DirenvInstall:
	nix-env -i direnv

.PHONY: git.Config
git.Config:
	git config --global mergetool.fugitive.cmd 'vim -f -c "Gdiffsplit!" "$$$\MERGED"'
	git config --global merge.tool fugitive
	git config --global mergetool.keepBackup false

.PHONY: clean
clean: .themes/Nordic/clean
	rm .local/bin/dir_colors
	rm -Rf .vim/autoload/plug.vim .tmux/plugins/tpm 
