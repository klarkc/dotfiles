syntax on
set number
set nocompatible
set encoding=utf-8
set clipboard=unnamedplus
set cursorline
set noshowmode
set timeoutlen=500
filetype plugin indent on

"{{ Leader key
let g:mapleader = "m"
"}}

"{{ Set Fold
function! s:SetFoldmethod()
  setlocal foldmethod=indent
  for item in synstack(line('.'), col('.'))
    if item =~# 'fold'
      setlocal foldmethod=syntax
      break
    endif
  endfor
endfunction
autocmd FileType * call s:SetFoldmethod()
"}}

"{{ Alt key fix
let c='a'
while c <= 'z'
	exec "set <A-".c.">=\e".c
	exec "imap \e".c." <A-".c.">"
	let c = nr2char(1+char2nr(c))
endw
set timeout ttimeoutlen=50
"}}

"{{ Use spaces instead of tabs on PureScript
" see purescript-contrib/purescript-vim#76
function! PurescriptIndent()
  setlocal expandtab
  setlocal shiftwidth=2
  setlocal tabstop=2
endfunction

"{{ LSP Folding
function! SetLspFolding()
  let allowed_servers = lsp#get_allowed_servers()
  let folding_supported = 0
  for server_name in allowed_servers
    " FIXME PureScript LSP folding broken
    if server_name ==# 'purescript-language-server'
      continue
    endif

    " FIXME Haskell LSP folding broken
    if server_name ==# 'haskell-language-server'
      continue
    endif

    if lsp#capabilities#has_folding_range_provider(server_name)
      let folding_supported = 1
    endif
  endfor

  if folding_supported
    set foldmethod=expr
    set foldexpr=lsp#ui#vim#folding#foldexpr()
    set foldtext=lsp#ui#vim#folding#foldtext()
  endif
endfunction
"}}

au BufRead,BufNewFile *.purs call PurescriptIndent()
"}}

"{{ Open GitHub links with <Leader>o
function! OpenGithubIssue()
    let l = getline('.')
		let match = matchlist(l, '\v(\S+)/(\S+)#(\d+)')
		let url = 'https://github.com/'.match[1].'/'.match[2].'/issues/'.match[3]
    silent exec '!xdg-open '.url ' > /dev/null 2>&1 &'
    execute 'redraw!'
endfunction
nmap <leader>o :call OpenGithubIssue()<CR>

call plug#begin('~/.vim/plugged')
"{{ Which Key
Plug 'liuchengxu/vim-which-key'
nnoremap <silent> <leader> :<c-u>WhichKey '<Leader>'<CR>
let g:which_key_map = {
      \ 's': { 'name': "+At cursor" },
      \ }
augroup WhickKeyMappings
    autocmd!
    autocmd VimEnter * call which_key#register('m', "g:which_key_map")
augroup END
"}}

"{{ Configuring NerdTree
	Plug 'scrooloose/nerdtree'
	let NERDTreeIgnore = [ 'node_modules/' ]
	let NERDTreeShowHidden=1
	map <Leader>n :NERDTreeToggle<CR>
  let g:which_key_map.n = 'NERDTree'
"}}

"{{ Configuring Airline
	Plug 'vim-airline/vim-airline'
	let g:airline#extensions#tabline#enabled = 1
	let g:airline#extensions#tabline#left_sep = ' '
	let g:airline#extensions#tabline#left_alt_sep = ' '
  let g:airline#extensions#zoomwintab#enabled = 1
	let g:airline_powerline_fonts = 1
"}}

"{{ Configuring Nord
	Plug 'arcticicestudio/nord-vim'
"}}

"{{ Configuring fzf
  Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
  Plug 'junegunn/fzf.vim'
  let g:which_key_map.k = { 'name': '+FZF' }
	map <leader>kk :GFiles<CR>
  let g:which_key_map.k.k = 'git files'
	map <leader>ks :GFiles?<CR>
  let g:which_key_map.k.s = 'git status'
	map <leader>ko :Buffers<CR>
  let g:which_key_map.k.o = 'buffers'
	map <leader>kg :RG<CR>
  let g:which_key_map.k.g = 'ripgrep'
	map <leader>km :Files<CR>
  let g:which_key_map.k.m = 'all files'
"}}

"{{ Git Integration
	Plug 'tpope/vim-fugitive'
	Plug 'junegunn/gv.vim'
	Plug 'Xuyuanp/nerdtree-git-plugin'
	Plug 'mhinz/vim-signify'

  nmap <leader>ss :SignifyHunkDiff<CR>
  let g:which_key_map.s.s = 'diff'
  nmap <leader>sm :SignifyHunkUndo<CR>
  let g:which_key_map.s.m = 'diff undo'
  nmap <leader>sa :diffget //2<CR>
  let g:which_key_map.s.a = 'diffget left'
  nmap <leader>sd :diffget //3<CR>
  let g:which_key_map.s.d = 'diffget right'
"}}

  let g:which_key_map.l = { 'name': '+Git history' }
  nmap <leader>ll :GV!<CR>
  let g:which_key_map.l.l = 'from branch'
  nmap <leader>lm :GV<CR>
  let g:which_key_map.l.m = 'since begin'
"}}

"{{ TMux - Vim integration
	Plug 'christoomey/vim-tmux-navigator'
"}}

"{{ More languages
	Plug 'sheerun/vim-polyglot'
"}}

"{{ LSP
	Plug 'klarkc/vim-lsp'
	Plug 'prabirshrestha/asyncomplete.vim'
	inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
	inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
	inoremap <expr> <cr>    pumvisible() ? asyncomplete#close_popup() : "\<cr>"
	Plug 'prabirshrestha/asyncomplete-lsp.vim'
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
    nmap <buffer> <leader>r <plug>(lsp-rename)
		nmap <buffer> <leader>c <plug>(lsp-code-action)
    nmap <buffer> [g <plug>(lsp-previous-diagnostic)
    nmap <buffer> ]g <plug>(lsp-next-diagnostic)
    nmap <buffer> K <plug>(lsp-hover)
    nmap <buffer> W <plug>(lsp-document-diagnostics)
    map <buffer> f <plug>(lsp-document-range-format)
    nmap <buffer> f <plug>(lsp-document-range-format)
		nmap <S-f> <plug>(lsp-document-format)
    nnoremap <buffer> <expr><M-u> lsp#scroll(+4)
    nnoremap <buffer> <expr><M-d> lsp#scroll(-4)
    call SetLspFolding()
	endfunction

	augroup lsp_install
		au!
		let g:lsp_signs_enabled = 1
		let g:lsp_diagnostics_echo_cursor = 1
		let g:lsp_highlight_references_enabled = 1
		let g:lsp_document_highlight_enabled = 1
    "let g:polyglot_disabled = ['folds']
		autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
	augroup END
"}}

"{{ Confortable Motion
Plug 'yuttie/comfortable-motion.vim'
"}}
"
"{{ Undotree
Plug 'mbbill/undotree'
nnoremap <Leader>u :UndotreeToggle<CR>
"}}

"{{ Zoom
Plug 'troydm/zoomwintab.vim'
"}}

"{{ Vimsence
Plug 'anurag3301/vimsence'
"}}

"{{ Bible
Plug 'sirjofri/vim-biblereader'
let g:which_key_map.b = { 'name': '+Bible' }
map <leader>bb :call FindInBible()<CR>
let g:which_key_map.b.b = 'find in bible'
map <leader>bm :call VFindInBible()<CR>
let g:which_key_map.b.m = 'find in bible (vertically)'
map <leader>bp :call PasteVerse()<CR>
let g:which_key_map.b.m = 'paste verse'
"}}

"{{ VimWiki
Plug 'vimwiki/vimwiki'
Plug 'michal-h21/vimwiki-sync'
let g:which_key_map.w = { 'name': '+VimWiki' }
let g:which_key_map.w.i = 'diary index'
let g:which_key_map.w.w = 'open'
let g:which_key_map.w.t = 'split open'
let g:which_key_map.w.s = 'select and open'
let g:which_key_map.w.b = 'select and open BibleWiki'
map <leader>wb :VimwikiIndex 2<CR>
let g:which_key_map.w.d = 'delete cur wiki file'
let g:which_key_map.w.r = 'rename cur wiki file'
map <leader>wq :help vimwiki <CR>
let g:which_key_map.w.q = 'help'

let personal_wiki = {}
let bible_wiki = {}
let bible_wiki.path = '~/Sources/BibleWiki'
let bible_wiki.ext = '.md'
let bible_wiki.index = 'README'
let bible_wiki.syntax = 'markdown'

let g:vimwiki_list = [personal_wiki , bible_wiki]
"}}

"{{ vim-unicoder
Plug 'arthurxavierx/vim-unicoder'
"}}

"{{ vim-chatgpt
Plug 'CoderCookE/vim-chatgpt'
let g:chat_gpt_max_tokens=200
let g:chat_gpt_model='gpt-4'
let g:chat_gpt_session_mode=0
let g:chat_gpt_temperature = 0.7
let g:chat_gpt_lang = 'English'
let g:chat_gpt_split_direction = 'vertical'
let g:split_ratio=4
nmap <leader>sg :Ask<CR>
let g:which_key_map.s.g = 'gpt complete'
"}}

"{{ vim-llama
Plug 'skywind3000/asyncrun.vim'
Plug 'Dr4x14913/vim-llama'
let g:light_models = {
      \ 'p': 'phi3:latest',
      \ 'd': 'deepseek-coder:latest',
      \ 'c': 'codegemma:code',
      \ 'o': 'orca-mini:latest', 
      \ }
let g:heavy_models = {
      \ 'l': 'llama3:latest',
      \ 'c': 'codellama:latest',
      \ 'm': 'mixtral:latest',
      \ }
let g:vim_llama_model = light_models.p
function! SetLlamaModel(model)
  let g:vim_llama_model = a:model
  echo "Selected to model: " . g:vim_llama_model
endfunction
function! SetModels(prop, models)
  let prop = a:prop
  let models = a:models
  for key in keys(models)
    let model = models[key]
    let g:which_key_map.a[prop][key] = model
    execute 'map <leader>a' . prop . key . ' :call SetLlamaModel("' . model . '")<CR>'
  endfor
endfunction

vmap <S-j> :VLMAStart What is ,?<CR>
let g:which_key_map.a = { 'name': '+Llama' }
let g:which_key_map.a.a = 'start'
map <leader>aa :VLMAStart<CR>
let g:which_key_map.a.s = 'stop'
map <leader>as :VLMAStop<CR>
let g:which_key_map.a.p = 'prompt'
map <leader>ap :VLMAPrompt<CR>
let g:which_key_map.a.l = { 'name': '+select light model' }
call SetModels('l', light_models)
let g:which_key_map.a.h = { 'name': '+select heavy model' }
call SetModels('h', light_models)
"}}

call plug#end()

"{{ Colors
	colorscheme nord
"}}

"{{ Buffers
	map g] :bn<cr>
	map g[ :bp<cr>
"}}
