syntax on
set number
set nocompatible
set encoding=utf-8
filetype plugin indent on
call plug#begin('~/.vim/plugged')
" ---> this is where you add your plugins <---

"{{ Configuring NerdTree
	Plug 'scrooloose/nerdtree'
	let NERDTreeIgnore = [ 'node_modules/' ]
	let NERDTreeShowHidden=1
	map <C-n> :NERDTreeToggle<CR>
"}}

"{{ Configuring EditorConfig
	Plug 'editorconfig/editorconfig-vim'
"}}

"{{ Configuring CtrlP
Plug 'ctrlpvim/ctrlp.vim'
"}}

"{{ Git Integration
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'Xuyuanp/nerdtree-git-plugin'
"}}

call plug#end()