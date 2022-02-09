all: .vim/autoload/plug.vim .tmux/plugins/tmp

.vim/autoload/plug.vim:
	curl -fLo .vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

.tmux/plugins/tmp:
	git clone https://github.com/tmux-plugins/tpm .tmux/plugins/tpm
