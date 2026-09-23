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
"{{ OSC 52
Plug 'ojroques/vim-oscyank'
"}}
"
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
	Plug 'rickhowe/diffunitsyntax'

  let g:DiffUnit = 'Word1'
  let g:DiffUnitSyntax = 2
  let g:signify_smart_diff_max_line_distance = 0.6
  let g:signify_smart_diff_block_min_lines = 8
  let g:signify_smart_diff_debounce_ms = 20

  function! s:SignifyLineDistance(old, new) abort
    let l:old = split(a:old, '\zs')
    let l:new = split(a:new, '\zs')
    let l:oldlen = len(l:old)
    let l:newlen = len(l:new)
    let l:maxlen = max([l:oldlen, l:newlen])
    if l:maxlen == 0
      return 0.0
    endif
    if empty(l:old) || empty(l:new)
      return 1.0
    endif

    if l:oldlen == l:newlen
      let l:changes = 0
      for l:i in range(0, l:oldlen - 1)
        if l:old[l:i] !=# l:new[l:i]
          let l:changes += 1
        endif
      endfor
      return l:changes * 1.0 / l:maxlen
    endif

    let l:minlen = min([l:oldlen, l:newlen])
    let l:prefix = 0
    while l:prefix < l:minlen
          \ && l:old[l:prefix] ==# l:new[l:prefix]
      let l:prefix += 1
    endwhile

    let l:suffix = 0
    while l:prefix + l:suffix < l:minlen
          \ && l:old[l:oldlen - l:suffix - 1]
          \    ==# l:new[l:newlen - l:suffix - 1]
      let l:suffix += 1
    endwhile

    let l:changed = max([
          \ l:oldlen - l:prefix - l:suffix,
          \ l:newlen - l:prefix - l:suffix,
          \ ])
    return l:changed * 1.0 / l:maxlen
  endfunction

  function! s:SignifyBlockUsesChar(removed, added) abort
    let l:pairs = min([len(a:removed), len(a:added)])
    if l:pairs == 0
      return -1
    endif

    for l:i in range(0, l:pairs - 1)
      if s:SignifyLineDistance(a:removed[l:i], a:added[l:i])
            \ > g:signify_smart_diff_max_line_distance
        return 0
      endif
    endfor
    return 1
  endfunction

  function! s:SignifyBlockDistance(removed, added) abort
    let l:span = max([len(a:removed), len(a:added)])
    if l:span == 0
      return 0.0
    endif

    let l:pairs = min([len(a:removed), len(a:added)])
    let l:distance = (l:span - l:pairs) * 1.0
    if l:pairs > 0
      for l:i in range(0, l:pairs - 1)
        let l:distance += s:SignifyLineDistance(
              \ a:removed[l:i],
              \ a:added[l:i])
      endfor
    endif

    return l:distance / l:span
  endfunction

  function! s:SignifyBlockUsesBlock(removed, added) abort
    let l:span = max([len(a:removed), len(a:added)])
    if l:span < g:signify_smart_diff_block_min_lines
      return 0
    endif

    if empty(a:removed) || empty(a:added)
      return 1
    endif

    return s:SignifyBlockDistance(a:removed, a:added)
          \ > g:signify_smart_diff_max_line_distance
  endfunction

  function! s:SignifySmartDiffMode(lines) abort
    let l:removed = []
    let l:added = []
    let l:has_pairs = 0
    let l:has_word = 0

    for l:line in a:lines + [' ']
      if l:line =~# '^-'
        call add(l:removed, l:line[1:])
      elseif l:line =~# '^+'
        call add(l:added, l:line[1:])
      else
        if s:SignifyBlockUsesBlock(l:removed, l:added)
          return 'Block'
        endif

        let l:decision = s:SignifyBlockUsesChar(l:removed, l:added)
        if l:decision == 0
          let l:has_word = 1
        elseif l:decision == 1
          let l:has_pairs = 1
        endif
        let l:removed = []
        let l:added = []
      endif
    endfor

    if l:has_word
      return 'Word1'
    endif
    return l:has_pairs ? 'Char' : 'Word1'
  endfunction

  let s:signify_smart_popup = 0
  let s:signify_smart_source_win = 0
  let s:signify_smart_source_buf = 0
  let s:signify_smart_anchor_line = 0
  let s:signify_smart_anchor_col = 1
  let s:signify_smart_generation = 0
  let s:signify_smart_render_timer = -1
  let s:signify_smart_pending_lines = []
  let s:signify_smart_motion_timer = -1
  let s:signify_smart_motion_watch_timer = -1
  let s:signify_smart_motion_popup = 0

  augroup SignifySmartPopup
    autocmd!
    autocmd WinScrolled * call <SID>SignifySmartPopupReposition(0)
  augroup END

  function! s:SignifySmartMotionRunning() abort
    return s:signify_smart_motion_timer > 0
          \ && !empty(timer_info(s:signify_smart_motion_timer))
  endfunction

  function! s:SignifySmartFindMotionTimer() abort
    let l:interval = float2nr(round(get(
          \ g:,
          \ 'comfortable_motion_interval',
          \ 1000.0 / 60)))

    for l:timer in timer_info()
      if l:timer.repeat == -1
            \ && abs(l:timer.time - l:interval) <= 1
            \ && string(l:timer.callback) =~# '_tick'
        return l:timer.id
      endif
    endfor
    return -1
  endfunction

  function! s:SignifySmartMotionWatch(timer) abort
    if s:SignifySmartMotionRunning()
      return
    endif

    call timer_stop(a:timer)
    let s:signify_smart_motion_watch_timer = -1
    let s:signify_smart_motion_timer = -1

    let s:signify_smart_motion_popup = 0
    call s:SignifySmartPopupReposition(1)
  endfunction

  function! s:SignifySmartFlick(impulse) abort
    if s:signify_smart_popup != 0
          \ && !empty(popup_getpos(s:signify_smart_popup))
      let s:signify_smart_motion_popup = s:signify_smart_popup
      if get(popup_getpos(s:signify_smart_popup), 'visible', 0)
        call popup_hide(s:signify_smart_popup)
      endif
    endif

    call comfortable_motion#flick(a:impulse)

    if !s:SignifySmartMotionRunning()
      let s:signify_smart_motion_timer = s:SignifySmartFindMotionTimer()
    endif

    if s:signify_smart_motion_popup != 0
          \ && s:signify_smart_motion_watch_timer == -1
      let s:signify_smart_motion_watch_timer = timer_start(
            \ 30,
            \ function('<SID>SignifySmartMotionWatch'),
            \ {'repeat': -1})
    endif
  endfunction

  function! s:SignifySmartPopupReposition(force) abort
    if s:signify_smart_popup == 0
          \ || s:signify_smart_motion_popup != 0
          \ || empty(popup_getpos(s:signify_smart_popup))
          \ || (!a:force
          \     && !has_key(v:event, string(s:signify_smart_source_win)))
      return
    endif

    let l:screen = screenpos(
          \ s:signify_smart_source_win,
          \ s:signify_smart_anchor_line,
          \ s:signify_smart_anchor_col)
    if empty(l:screen) || l:screen.row == 0
      if get(popup_getpos(s:signify_smart_popup), 'visible', 0)
        call popup_hide(s:signify_smart_popup)
      endif
      return
    endif

    let l:visible = get(popup_getpos(s:signify_smart_popup), 'visible', 0)
    call popup_move(s:signify_smart_popup, {'line': l:screen.row + 1})
    if !l:visible
      call popup_show(s:signify_smart_popup)
    endif
  endfunction

  function! s:SignifySmartCreatePopup(lines) abort
    let l:source_win = win_id2win(s:signify_smart_source_win)
    let l:screen = screenpos(
          \ s:signify_smart_source_win,
          \ s:signify_smart_anchor_line,
          \ s:signify_smart_anchor_col)
    let l:winpos = win_screenpos(s:signify_smart_source_win)
    let l:wininfo = getwininfo(s:signify_smart_source_win)
    let l:textoff = empty(l:wininfo) ? 0 : l:wininfo[0].textoff
    let l:padding = repeat(' ', max([0, l:textoff - 1]))
    let l:popup_lines = map(copy(a:lines),
          \ 'empty(v:val) ? v:val : v:val[0] . l:padding . v:val[1:]')
    let l:maxheight = max([1, winheight(l:source_win)])

    let l:popup = popup_create(l:popup_lines, {
          \ 'line': l:screen.row + 1,
          \ 'col': l:winpos[1] - 1,
          \ 'minwidth': winwidth(l:source_win),
          \ 'maxheight': l:maxheight,
          \ 'wrap': 1,
          \ 'scrollbar': 1,
          \ 'zindex': 1000,
          \ 'posinvert': 1,
          \ 'hidden': 1,
          \ })

    call setwinvar(l:popup, '&linebreak', getwinvar(l:source_win, '&linebreak'))
    call setwinvar(l:popup, '&breakindent', getwinvar(l:source_win, '&breakindent'))
    call setwinvar(l:popup, '&breakindentopt', getwinvar(l:source_win, '&breakindentopt'))
    call setwinvar(l:popup, '&showbreak', getwinvar(l:source_win, '&showbreak'))
    return l:popup
  endfunction

  function! s:SignifySmartHunkContains(header, lnum) abort
    let [_old_line, l:old_count, l:new_line, l:new_count] =
          \ sy#sign#parse_hunk(a:header)

    if a:lnum == 1 && l:new_line == 0
      return 1
    endif
    if a:lnum == l:new_line && l:new_count < l:old_count
      return 1
    endif
    return a:lnum >= l:new_line
          \ && a:lnum < l:new_line + l:new_count
  endfunction

  function! s:SignifySmartExtractHunk(diff, lnum) abort
    let l:inside = 0
    let l:hunk = []

    for l:line in a:diff
      if l:inside
        if empty(l:line) || l:line[:2] ==# '@@ '
          break
        endif
        call add(l:hunk, l:line)
      elseif l:line[:2] ==# '@@ '
            \ && s:SignifySmartHunkContains(l:line, a:lnum)
        let l:inside = 1
      endif
    endfor

    return l:hunk
  endfunction

  function! s:SignifySmartRender(timer) abort
    let s:signify_smart_render_timer = -1
    let l:lines = s:signify_smart_pending_lines
    let s:signify_smart_pending_lines = []

    if empty(l:lines)
          \ || win_id2win(s:signify_smart_source_win) == 0
          \ || winbufnr(s:signify_smart_source_win)
          \    != s:signify_smart_source_buf
      return
    endif

    let l:mode = s:SignifySmartDiffMode(l:lines)
    let l:diff_unit_syntax = get(g:, 'DiffUnitSyntax', 1)
    let g:DiffUnitSyntax = min([1, l:diff_unit_syntax])
    let l:popup = 0

    try
      let l:popup = s:SignifySmartCreatePopup(l:lines)
      let s:signify_smart_popup = l:popup
      let l:buffer = winbufnr(l:popup)
      call setbufvar(l:buffer, 'SignifySmartDiffMode', l:mode)
      call setbufvar(l:buffer, 'DiffColors', 0)
      if l:mode !=# 'Block'
        call setbufvar(l:buffer, 'DiffUnit', l:mode)
      endif
      call setbufvar(l:buffer, '&syntax', 'diff')

      if l:mode !=# 'Block'
        call win_execute(l:popup, 'call diffunitsyntax#DiffUnitSyntax()')
      endif

      let l:initial_line = get(popup_getpos(l:popup), 'line', 0)
      if l:initial_line > 0
        call popup_move(l:popup, {'line': l:initial_line})
      endif
      call popup_show(l:popup)
      redraw
    catch
      if l:popup != 0 && !empty(popup_getpos(l:popup))
        call popup_close(l:popup)
      endif
      let s:signify_smart_popup = 0
      throw v:exception
    finally
      let g:DiffUnitSyntax = l:diff_unit_syntax
    endtry
  endfunction

  function! s:SignifySmartDiffReady(generation, source_buf, source_win,
        \ anchor_line, _sy, _vcs, diff) abort
    if a:generation != s:signify_smart_generation
          \ || a:source_buf != s:signify_smart_source_buf
          \ || a:source_win != s:signify_smart_source_win
      return
    endif

    let l:hunk = s:SignifySmartExtractHunk(a:diff, a:anchor_line)
    if empty(l:hunk)
      return
    endif

    let s:signify_smart_pending_lines = l:hunk
    if s:signify_smart_render_timer != -1
      call timer_stop(s:signify_smart_render_timer)
    endif
    let s:signify_smart_render_timer = timer_start(
          \ g:signify_smart_diff_debounce_ms,
          \ function('<SID>SignifySmartRender'))
  endfunction

  function! s:SignifySmartHunkDiff() abort
    let s:signify_smart_generation += 1

    if s:signify_smart_render_timer != -1
      call timer_stop(s:signify_smart_render_timer)
      let s:signify_smart_render_timer = -1
    endif
    let s:signify_smart_pending_lines = []

    if s:signify_smart_popup != 0
          \ && !empty(popup_getpos(s:signify_smart_popup))
      call popup_close(s:signify_smart_popup)
    endif

    let s:signify_smart_popup = 0
    let s:signify_smart_source_win = win_getid()
    let s:signify_smart_source_buf = bufnr('%')
    let s:signify_smart_anchor_line = line('.')
    let s:signify_smart_anchor_col = max([1, col('.')])

    let l:sy = getbufvar(s:signify_smart_source_buf, 'sy')
    if empty(l:sy) || empty(l:sy.updated_by)
      return
    endif

    call sy#repo#get_diff(
          \ s:signify_smart_source_buf,
          \ l:sy.updated_by,
          \ function('<SID>SignifySmartDiffReady', [
          \   s:signify_smart_generation,
          \   s:signify_smart_source_buf,
          \   s:signify_smart_source_win,
          \   s:signify_smart_anchor_line,
          \ ]))
  endfunction

  nmap <leader>ss :call <SID>SignifySmartHunkDiff()<CR>
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

"{{ More mappings
  Plug 'tpope/vim-unimpaired'

"{{ LSP
	Plug 'prabirshrestha/vim-lsp'
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
let g:comfortable_motion_no_default_key_mappings = 1
Plug 'yuttie/comfortable-motion.vim'
nnoremap <silent> <C-d> :call <SID>SignifySmartFlick(100)<CR>
nnoremap <silent> <C-u> :call <SID>SignifySmartFlick(-100)<CR>
nnoremap <silent> <C-f> :call <SID>SignifySmartFlick(200)<CR>
nnoremap <silent> <C-b> :call <SID>SignifySmartFlick(-200)<CR>
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
Plug 'michal-h21/vim-zettel'

" Global variable to store the last "front_matter" value
let g:last_front_matter = []

function! ParseFM()
  let l:fm_value = []
  " Check if the current buffer has front matter
  let l:lines = getline(1, 10)
  if l:lines[0] != '---'
    return l:fm_value
  endif

  " Find the front matter section
  let l:front_matter_end = -1
  for l:i in range(1, len(l:lines))
    if l:lines[l:i] == '---'
      let l:front_matter_end = l:i
      break
    endif
  endfor

  if l:front_matter_end == -1
    return l:fm_value
  endif

  " Parse the front matter
  echom "parsing"
  for l:i in range(1, l:front_matter_end)
    let l:line = l:lines[l:i]
    "echom "checking " . l:line
    " Check if the line is a valid key-value pair
    if l:line =~ '^\s*\S\+:\s*\S\+'
      " Extract the key and value
      let l:key = matchstr(l:line, '^\(.\{-}\):\@<=')[:-2]
      "echom "key " . l:key
      let l:value = matchstr(l:line, ':\(.*\)')[1:]
      "echom "value " . l:value
      " Remove leading and trailing whitespace
      let l:key = substitute(l:key, '\s\+$', '', '')
      "echom "key' " . l:key
      let l:value = substitute(l:value, '^\s*', '', '')
      "echom "value' " . l:value
      " Append the key-value pair to fm_value
      call add(l:fm_value, [l:key, l:value])
    endif
  endfor

  return l:fm_value
endfunction

function! StoreFM()
  let g:last_front_matter = ParseFM()
  call zettel#vimwiki#zettel_new_selected()
endfunction
xnoremap <silent> StoreFM :call StoreFM()<CR>

function! s:insert_last_fm(field, default)
  echom "insert_last_fm called for field " . a:field
  let front_matter = zettel#vimwiki#get_option("front_matter")
  if g:last_front_matter != []
    echom "reusing last_front_matter"
    let front_matter = g:last_front_matter
  endif
  for item in front_matter
    echom "checking " . item[0] . " against " . a:field
    if item[0] == a:field 
      if type(item[1]) == type(function("s:insert_last_fm", [a:field, a:default]))
        echom "it's a function, returning default"
        return a:default
      else
        echom "returning field value"
        return item[1]
      endif
    endif
  endfor
endfunction

augroup CustomVimWikiMappings
  autocmd!
  autocmd FileType vimwiki nnoremap <C-w> viw:call StoreFM()<CR>
  autocmd FileType vimwiki xmap <buffer> <C-w> StoreFM()<CR>
augroup END

function! VimwikiOpen()
  let wiki = g:bible_wiki
  call chdir(wiki.path)
  execute ":VimwikiIndex"
endfunction

let g:which_key_map.w = { 'name': '+VimWiki' }
let g:which_key_map.w.w = 'open'
map <leader>ww :call VimwikiOpen()<CR>
let g:which_key_map.w.t = 'split open'
let g:which_key_map.w.s = 'select and open'
let g:which_key_map.w.d = 'delete cur wiki file'
let g:which_key_map.w.r = 'rename cur wiki file'
let g:which_key_map.w.x = 'capture'
map <leader>wx :ZettelCapture<CR>
let g:which_key_map.w.s = 'check sanity'
map <leader>ws :VimwikiCheckLinks<CR>
let g:which_key_map.w.g = { 'name': '+generate' }
let g:which_key_map.w.g.i = 'inbox'
map <leader>wgi :ZettelInbox<CR>
let g:which_key_map.w.g.l = 'links'
map <leader>wgl :ZettelGenerateLinks<CR>
let g:which_key_map.w.g.b = 'backlinks'
map <leader>wgb :ZettelBacklinks<CR>
let g:which_key_map.w.g.t = 'tags'
map <leader>wgt :ZettelGenerateTags<CR>
let g:which_key_map.w.k = { 'name': '+fzf' }
let g:which_key_map.w.k.w = 'open note'
map <leader>wkw :ZettelOpen<CR>
let g:which_key_map.w.k.i = 'insert note'
map <leader>wki :ZettelInsertNote<CR>
map <leader>wq :help vimwiki <CR>
let g:which_key_map.w.q = 'help'

let bible_wiki = {}
let bible_wiki.path = '~/Sources/BibleWiki'
let bible_wiki.ext = '.md'
let bible_wiki.index = 'README'
let bible_wiki.syntax = 'markdown'
let bible_wiki.links_space_char = '-'
let bible_zettel = {}
let bible_zettel.rel_path = ''
let bible_zettel.template = 'note.tpl'
let bible_zettel.front_matter = [['bible', function("s:insert_last_fm", ['bible', ''])]]

let g:vimwiki_markdown_link_ext = 1
let g:vimwiki_list = [bible_wiki]
let g:zettel_options = [bible_zettel]
let g:zettel_format = '%title--%file_no'

"}}

"{{ vim-unicoder
Plug 'arthurxavierx/vim-unicoder'
"}}

"{{ vim-easy-align
Plug 'junegunn/vim-easy-align'
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)
"}}

"{{ vim-highlighter
function! GetFilePath(file)
  let l:keywords = ".keywords"
  return "./" . l:keywords . "/" . a:file 
endfunction

function! GetFileHash(file)
  " Generate a hash of the file name using md5sum and cut
  let l:command = 'echo -n ' . shellescape(a:file) . ' | md5sum | cut -d" " -f1'
  let l:hash = system(l:command)
  " Remove any trailing newline
  let l:hash = substitute(l:hash, '\n$', '', '')
  return l:hash
endfunction

function! LoadHi()
  let l:file = expand('%')
  let l:hash = GetFilePath(GetFileHash(l:file))
  "echom "loading hl file " . l:hash
  execute ':Hi load ' . l:hash
endfunction

function! SaveHi()
  let l:file = expand('%')
  let l:hash = GetFilePath(GetFileHash(l:file))
  "echom "saving hl file " . l:hash
  execute ':!mkdir -p $(dirname ' . l:hash . ')'
  execute ':Hi save ' . l:hash
endfunction

function! RunIfFileIsVimWiki(func)
  for wiki in g:vimwiki_list
    if fnamemodify(wiki.path, ':p') ==# fnamemodify(expand('%:p:h'), ':p')
      "echom "current file is from vimwiki, executing function ". a:func
      execute ":call ". a:func. "()"
      return
    endif
  endfor
  "echom "current file is not from vimwiki, skipping function ". a:func
endfunction

" Wrapper function to handle BufUnload
function! HandleBufUnload()
  call RunIfFileIsVimWiki('SaveHi')
endfunction

" Wrapper function to handle BufWritePost
function! HandleBufWritePost()
  call RunIfFileIsVimWiki('SaveHi')
endfunction

" Wrapper function to handle BufReadPost
function! HandleBufReadPost()
  call RunIfFileIsVimWiki('LoadHi')
endfunction

autocmd BufUnload	* call HandleBufUnload()
autocmd BufWritePost	* call HandleBufWritePost()
autocmd BufReadPost * call HandleBufReadPost()
Plug 'azabiong/vim-highlighter'
let g:which_key_map.h = { 'name': '+Highlighter' }
let g:which_key_map.h.s = 'save'
map <leader>hs :call SaveHi()<CR>
let g:which_key_map.h.l = 'load'
map <leader>hl :call LoadHi()<CR>
"}}
call plug#end()

"{{ Colors
  let g:nord_uniform_diff_background = 1
	colorscheme nord
"}}

"{{ Buffers
	map g] :bn<cr>
	map g[ :bp<cr>
"}}
