.PHONY: all
all: vim.PluginInstall tmux.TpmInstall

.vim/autoload/plug.vim:
	curl -fLo .vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

.tmux/plugins/tpm: 
	git clone https://github.com/tmux-plugins/tpm .tmux/plugins/tpm

.tmux-powerline:
	git clone https://github.com/erikw/tmux-powerline.git .tmux-powerline

.PHONY: tmux.TpmInstall
tmux.TpmInstall: .tmux/plugins/tpm .tmux-powerline
	.tmux/plugins/tpm/scripts/install_plugins.sh

.PHONY: vim.PluginInstall
vim.PluginInstall: .vim/autoload/plug.vim
	vim +PluginInstall +qall

.PHONY: clean
clean:
	rm -Rf .vim/autoload/plug.vim .tmux/plugins/tpm .tmux-powerline
