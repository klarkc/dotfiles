syntax on
set number
set nocompatible
set encoding=utf-8
set clipboard=unnamed
set cursorline
filetype plugin indent on

call plug#begin('~/.vim/plugged')
"{{ Configuring Nord
	Plug 'arcticicestudio/nord-vim'
"}}

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
	function! s:on_lsp_buffer_enabled() abort
    setlocal omnifunc=lsp#complete
    setlocal signcolumn=yes
    if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
    nmap <buffer> gd <plug>(lsp-definition)
    nmap <buffer> gs <plug>(lsp-document-symbol-search)
    nmap <buffer> gS <plug>(lsp-workspace-symbol-search)
    nmap <buffer> gr <plug>(lsp-references)
    nmap <buffer> gi <plug>(lsp-implementation)
    nmap <buffer> gt <plug>(lsp-type-definition)
    nmap <buffer> <leader>rn <plug>(lsp-rename)
    nmap <buffer> [g <plug>(lsp-previous-diagnostic)
    nmap <buffer> ]g <plug>(lsp-next-diagnostic)
    nmap <buffer> K <plug>(lsp-hover)
    nnoremap <buffer> <expr><c-f> lsp#scroll(+4)
    nnoremap <buffer> <expr><c-d> lsp#scroll(-4)

    let g:lsp_format_sync_timeout = 1000
    autocmd! BufWritePre *.rs,*.go call execute('LspDocumentFormatSync')

    " refer to doc to add more commands
	endfunction

	augroup lsp_install
			au!
			" call s:on_lsp_buffer_enabled only for languages that has the server registered.
			autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
	augroup END
"}}
call plug#end()

"{{ Colors
	if !has('gui_running') && &term =~ '^\%(screen\|tmux\)'
		let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
		let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
	endif
	set termguicolors
	colorscheme nord
"}}
