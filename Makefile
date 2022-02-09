.PHONY: all
all: vim.PluginInstall .tmux/plugins/tpm

.vim/autoload/plug.vim:
	curl -fLo .vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

.tmux/plugins/tpm:
	git clone https://github.com/tmux-plugins/tpm .tmux/plugins/tpm

.PHONE: vim.PluginInstall
vim.PluginInstall: .vim/autoload/plug.vim
	vim +PluginInstall +qall

.PHONY: clean
clean:
	rm -Rf .vim/autoload/plug.vim .tmux/plugins/tpm
