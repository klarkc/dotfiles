syntax on
set number
set nocompatible
set encoding=utf-8
set clipboard=unnamed
filetype plugin indent on

call plug#begin('~/.vim/plugged')
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

"{{ TMux - Vim integration
Plug 'christoomey/vim-tmux-navigator'
""}}

"{{ More languages
Plug 'sheerun/vim-polyglot'
"}}
call plug#end()
