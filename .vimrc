"================================================"
"                  Vim Config                    "
"
"              Author: Bugi Abdulkarim           "
"           Last Modified: 03/14/2018            "
"                 Dependencies                   "
"      silverserver-ag: for the ack plugin       "
"      exuberant-ctags: for easytags plugin      "
"================================================"

"---------------------------------"
"            Plugins              "
"---------------------------------"
call plug#begin('~/.vim/plugged')

Plug 'mhinz/vim-startify' " fancy start page
Plug 'scrooloose/nerdtree' " filetree
Plug 'tomtom/tcomment_vim' " comment toggler
Plug 'easymotion/vim-easymotion' " jump to any word with ease
Plug 'ctrlpvim/ctrlp.vim' " fuzzyfinder
Plug 'mileszs/ack.vim' " search files for a pattern recursively
Plug 'vim-airline/vim-airline' "status line and tab bar
Plug 'vim-airline/vim-airline-themes'
Plug 'flazz/vim-colorschemes' " a bunch of colorschemes

" build and install autocompleter
Plug 'ervandew/supertab' " tab for omnicompletion
" Plug 'w0rp/ale' " linting
Plug 'xolox/vim-misc' " dependency for vim-easytags
Plug 'xolox/vim-easytags' " easy tag generation for jumping to definitions
Plug 'tpope/vim-surround' " easily change surrounding brackets, quotes, etc.
Plug 'terryma/vim-expand-region' " highlight increasingly larger regions of text
Plug 'octol/vim-cpp-enhanced-highlight' "additional cpp syntax support
Plug 'pangloss/vim-javascript' " javascript syntax support
Plug 'mxw/vim-jsx' " jsx syntax support for react
Plug 'rstacruz/sparkup' "html expander
Plug 'airblade/vim-gitgutter' "show added/deleted lines in gutter
Plug 'bronson/vim-trailing-whitespace' "show trailing whitespace as red bg
Plug 'jparise/vim-graphql' " graphql for vim
Plug 'Yggdroot/indentLine' " displays thin vertical lines at each indentation
Plug 'tpope/vim-fugitive' " Git wrapper
Plug 'vim-scripts/indentpython.vim' " alternative indentation script for python
Plug 'vim-latex/vim-latex'
Plug 'xuhdev/vim-latex-live-preview' " lively previewing LaTeX PDF output
Plug 'davidhalter/jedi-vim' " code completion python

call plug#end()
"--------------------------------"
"         General Config         "
"--------------------------------"
set nocompatible
filetype plugin indent on
syntax on
set encoding=utf8		  "utf8 standard encoding
set clipboard=unnamed     " access clipboard
set paste 				  " paste from vim
set go+=a 				  " visual selection automatically copied to clipboard
set hidden                " buffers can be in the bg without having to be saved
set autoread              " autoreload changed files
set nobackup
set noswapfile            " I hate swp files sometimes, use at your own risk

set number                " line numbers
set relativenumber        " hybrid numbering

set cursorline            " underline current line
set regexpengine=1        " use old regex engine to fix scroll lag
set nowrap                " don't wrap lines

set hlsearch              " highlight search hits
set incsearch             " show search hits as you type
set ignorecase            " ignore case when searching
set smartcase             " override ignore case if uppercase letters in pattern


set tabstop=4 " show existing tab with 4 spaces width
set shiftwidth=4 " when indenting with '>', use 4 spaces width
set expandtab " On pressing tab, insert 4 spaces

set foldmethod=indent
set foldminlines=5        " min num of lines before a block is foldable
set foldlevelstart=12     " don't autofold unless there are 12 indents

set undodir=~/.vim/undodir
set undofile              " persistent undo

set pastetoggle=<F2>      " switch to paste mode to paste easily
"set clipboard=unnamedplus " use system clipboard

set shellpipe=>           " hide ack searches from stdout

set visualbell            " visual bell
colorscheme 0x7A69_dark 		" sourcerer, oceandeep
" let g:livepreview_previewer = 'okular'
let g:livepreview_previewer = 'mupdf-gl'
"let g:livepreview_previewer = 'open -a Preview'

"--------------------------------"
"            Style Guide         "
"--------------------------------"
" Python
au BufNewFile, BufRead *.py:
\ set textwidth=79           " line length doesn't exceed 80 characters
\ set fileformat=unix        " store file in unix format
" Full stack development
au BufNewFile, BufRead *.js, *.html, *.css:
\ set tabstop=2
\ set softtabstop=2
\ set shiftwidth=2
autocmd Filetype javascript setlocal ts=2 sw=2 sts=0 noexpandtab
" CPP
autocmd FileType cpp setlocal ts=2 sts=2 sw=2

"--------------------------------"
"            Mappings            "
"--------------------------------"
let g:TerminusFocusReporting=0
let mapleader   = "," " remap leader to ,
let g:mapleader = ","
" easy normal mode
imap jk <Esc>
imap kj <Esc>
" easy command input
nnoremap ; :

" Cursor will always be in middle of window
" set scrolloff=9999

" Disable navigation with up/down
nmap <Up> <nop>
nmap <Down> <nop>

" Quick Save/Quit
nmap <leader>w :w!<cr>
nmap <leader>wq :wq!<cr>
nmap <leader>q :q<cr>
nmap <leader>qq :q!<cr>

" Buffer navigation
map <leader>bd :bd<cr>
map <leader>bn :bn<cr>
map <leader>bb :bb<cr>

" Line navigation
nnoremap B ^
nnoremap E $

" Window navigation
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-h> <C-w>h
map <C-l> <C-w>l

" Search
nmap <space> /
nmap <silent> <leader><space> :nohlsearch<cr>

" Tabs
nnoremap <silent> <C-t> :tabnew<cr>
map <silent> <Left> :tabprevious<cr>
map <silent> <Right> :tabnext<cr>
map <silent> <leader>tc :tabclose<cr>

" delete trailing whitespace
nmap <silent> <leader>dw :%s/\s\+$//<cr>

" move lines with Shift + Up/Down
nnoremap <silent> <S-Up> :m-2<cr>
nnoremap <silent> <S-Down> :m+<cr>
inoremap <silent> <S-Up> <Esc>:m-2<cr>i
inoremap <silent> <S-Down> <Esc>:m+<cr>i

" toggle line number type
nnoremap <silent> <F3> :call ToggleNumber()<cr>

" toggle folding
nnoremap <silent> <leader>f :call ToggleFold()<cr>

" use w!! to save as sudo
cmap w!! w !sudo tee % >/dev/nulli

" shifting in visual mode doesn't unselect
vnoremap < <gv
vnoremap > >gv

" copy component template into current file at cursor
nnoremap <silent> <leader>rnc :read ~/dotfiles/templates/component.js<cr>

" Plugin Mappings
noremap <silent> <leader>cc :TComment<cr>
map <leader>g :Ack!

" close quickfix window
map <silent> <leader>gq :ccl<cr>
"--------------------------------"
"     React Native Mappings      "
"--------------------------------"
autocmd FileType javascript inoremap <leader>tx <Text></Text><Space><++>


"--------------------------------"
"             Colors             "
"--------------------------------"
"syntax on
let python_highlight_all=1
"colorscheme Dark
hi Visual ctermfg=NONE ctermbg=241 cterm=NONE guifg=NONE guibg=#44475a gui=NONE
hi Folded ctermbg=0
hi Search ctermfg=NONE ctermbg=241 cterm=NONE guibg=#44475a gui=NONE

"--------------------------------"
"      Compilation/Execution     "
"--------------------------------"
" C++
autocmd filetype cpp nnoremap <F4> :w<cr> :!clang++-5.0 -std=c++11 -Wall -g *.cpp && ./a.out<cr>
" Java
autocmd filetype java nnoremap <F4> :w<cr> :!javac %<cr>
" Rust
autocmd filetype rust nnoremap <F4> :w<cr> :!rustc % && ./%:r
" Python
autocmd filetype python nnoremap <F4> :w<cr> :!python %<cr>
" Ruby
autocmd filetype ruby nnoremap <F4> :w<cr> :!ruby %<cr>

"================================================"
"                 Plugin Config                  "
"================================================"

"--------------------------------"
"              ack               "
"--------------------------------"
if executable('ag')
	let g:ackprg = 'ag --vimgrep -U'
endif

"--------------------------------"
"            Airline             "
"--------------------------------"
let g:airline_theme='deus'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
set t_RV= " fixes broken first line in rendering

"--------------------------------"
"              ale               "
"--------------------------------"
" let g:ale_fixers = {'javascript': ['prettier', 'eslint'], 'python': ['autopep8']}
" let g:ale_fix_on_save = 1
" let g:ale_lint_on_text_changed = 'never'
" let g:ale_lint_on_enter = 0
" let g:ale_completion_enabled = 1
"
"--------------------------------"
"             CtrlP              "
"--------------------------------"
let g:ctrlp_show_hidden = 1
" use ag
let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'
" ignore node_modules et al
let g:ctrlp_custom_ignore = '\v[\/](node_modules|target|dist)|(\.(swp|ico|git|svn))$'

"--------------------------------"
"            easytags            "
"--------------------------------"
" store tags per project, do not use global
set tags=./.tags;
let g:easytags_dynamic_files = 2
let g:easytags_async = 1
let g:easytags_auto_highlight = 0
let g:easytags_include_members = 1
let g:easytags_events = ['BufWritePost']

"--------------------------------"
"            NERDTree            "
"--------------------------------"
"autocmd vimenter * NERDTree
" open a NERDTree automatically when vim starts up
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'
" default arrow symbols
"let NERDTreeShowHidden=0
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
map <C-n> :NERDTreeToggle<CR>
let NERDTreeShowHidden=1
let g:NERDTreeHijackNetrw=0

" shows hidden files automatically
"--------------------------------"
"           Startify             "
"--------------------------------"
let g:startify_custom_header = [
\ '   ___      ___ ___  _____ ______',
\ '  |\  \    /  /|\  \|\   _ \  _   \ ',
\ '  \ \  \  /  / | \  \ \  \\\__\ \  \ ',
\ '   \ \  \/  / / \ \  \ \  \\|__| \  \ ',
\ '    \ \    / /   \ \  \ \  \    \ \  \',
\ '     \ \__/ /     \ \__\ \__\    \ \__\',
\ '      \|__|/       \|__|\|__|     \|__|',
\ ]

"--------------------------------"
"          YouCompleteMe         "
"--------------------------------"
" let g:ycm_server_python_interpreter = '/usr/bin/python'
" let g:ycm_python_binary_path = '/usr/bin/python3'
" let g:ycm_global_ycm_extra_conf = "~/.vim/.ycm_extra_conf.py"
" set completeopt-=preview
" highlight Pmenu ctermbg=0 ctermfg=5

"--------------------------------"
"           Functions            "
"--------------------------------"
" Toggles folding
 function ToggleFold()
 	if foldlevel('.') == 0
 		normal! 1
 	else
 		if foldclosed('.') < 0
 			. foldclose
 		else
 			. foldopen
 		endif
 	endif
 	echo
 endfunction

 " Toggles hybrid numbering
 function ToggleNumber()
     if (&relativenumber == 1)
         set norelativenumber
         set number
     else
         set relativenumber
     endif
 endfunction

" Cycle through location lists
 function LNext(prev)
 	try
 		try
 			if a:prev | lprev | else | lnext | endif
 		catch /^Vim\%((a\+)\)\=:E553/
 			if a:prev | llast | else | lfirst| endif
 		catch /^Vim\%((\a\+)\)\=:E776/
 		endtry
 	catch /^Vim\%((\a\+)\)\=:E42/
 	endtry
 endfunction

" Spell checker
map<F6> :setlocal spell! spellang=en_us<CR>

" If powerline fonts partially messed up
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_symbols.space = "\ua0"



" Compatible with ranger 1.4.2 through 1.7.*
"
" Add ranger as a file chooser in vim
"
" If you add this code to the .vimrc, ranger can be started using the command
" ":RangerChooser" or the keybinding "<leader>r".  Once you select one or more
" files, press enter and ranger will quit again and vim will open the selected
" files.

function! RangeChooser()
    let temp = tempname()
    " The option "--choosefiles" was added in ranger 1.5.1. Use the next line
    " with ranger 1.4.2 through 1.5.0 instead.
    "exec 'silent !ranger --choosefile=' . shellescape(temp)
    if has("gui_running")
        exec 'silent !xterm -e ranger --choosefiles=' . shellescape(temp)
    else
        exec 'silent !ranger --choosefiles=' . shellescape(temp)
    endif
    if !filereadable(temp)
        redraw!
        " Nothing to read.
        return
    endif
    let names = readfile(temp)
    if empty(names)
        redraw!
        " Nothing to open.
        return
    endif
    " Edit the first item.
    exec 'edit ' . fnameescape(names[0])
    " Add any remaning items to the arg list/buffer list.
    for name in names[1:]
        exec 'argadd ' . fnameescape(name)
    endfor
    redraw!
endfunction
command! -bar RangerChooser call RangeChooser()
nnoremap <leader>r :<C-U>RangerChooser<CR>
