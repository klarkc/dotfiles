syntax on
set number
set nocompatible
set encoding=utf-8
set clipboard=unnamed
filetype plugin indent on

"{{ Fix Colors
	autocmd VimEnter * highlight CursorColumn term=reverse ctermbg=Black 
"}}

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
	Plug 'Xuyuanp/nerdtree-git-plugin'
	Plug 'mhinz/vim-signify'
	autocmd VimEnter * highlight SignColumn ctermbg=NONE cterm=NONE guibg=NONE gui=NONE
	autocmd VimEnter * highlight DiffAdd ctermbg=Black ctermfg=LightGreen 
	autocmd VimEnter * highlight DiffChange ctermbg=Black ctermfg=Yellow
	autocmd VimEnter * highlight DiffDelete ctermbg=Black ctermfg=Red
"}}

"{{ TMux - Vim integration
	Plug 'christoomey/vim-tmux-navigator'
"}}

"{{ More languages
	Plug 'sheerun/vim-polyglot'
"}}

"{{ LSP
	Plug 'prabirshrestha/vim-lsp'
	Plug 'mattn/vim-lsp-settings'
"}}
call plug#end()
