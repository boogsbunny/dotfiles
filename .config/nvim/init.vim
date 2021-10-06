"================================================"
"                  Vim Config                    "
"              Author: Bugi Abdulkarim           "
"           Last Modified: 09/28/2020            "
"================================================"
"
if empty(glob('~/.vim/autoload/plug.vim'))
    silent sh -c '!curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
"
"---------------------------------"
"            Plugins              "
"---------------------------------"
call plug#begin('~/.vim/plugged')

" Language Server Protocol (MSFT) for Neovim
" Plug 'neovim/nvim-lspconfig'
" Plug 'nvim-lua/completion-nvim'
" Plug 'tjdevries/nlua.nvim'
" Plug 'tjdevries/lsp_extensions.nvim'

" Telescope fuzzy finder
Plug 'nvim-lua/telescope.nvim'
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'

" VCS related (version control system)
Plug 'tpope/vim-fugitive' " Git wrapper
Plug 'vim-utils/vim-man' " Vim manual
Plug 'mbbill/undotree' " vcs visualizer
Plug 'sheerun/vim-polyglot' " collection of language packs
Plug 'airblade/vim-gitgutter' "show added/deleted lines in gutter

" FZF wrapper
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'stsewd/fzf-checkout.vim'

" Simplify things
Plug 'mhinz/vim-startify' " fancy start page
Plug 'tomtom/tcomment_vim' " comment toggler
Plug 'tpope/vim-surround' " easily change surrounding brackets, quotes, etc.
Plug 'terryma/vim-expand-region' " highlight increasingly larger regions of text
Plug 'bronson/vim-trailing-whitespace' "show trailing whitespace as red bg
Plug 'Yggdroot/indentLine' " displays thin vertical lines at each indentation

" Style related
Plug 'gruvbox-community/gruvbox'
Plug 'colepeters/spacemacs-theme.vim'
Plug 'sainnhe/gruvbox-material'
Plug 'phanviet/vim-monokai-pro'
Plug 'flazz/vim-colorschemes'
Plug 'chriskempson/base16-vim'

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
let loaded_matchparen = 1
set pastetoggle=<F2>      " switch to paste mode to paste easily
set visualbell            " visual bell
let g:fzf_layout = { 'window': { 'width': 0.8, 'height': 0.8 } }
let $FZF_DEFAULT_OPTS='--reverse'

"--------------------------------"
"            Color Theme         "
"--------------------------------"
let g:gruvbox_contrast_dark = 'hard'
if exists('+termguicolors')
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif
let g:gruvbox_invert_selection='0'
colorscheme gruvbox
set background=dark

"--------------------------------"
"            Style Guide         "
"--------------------------------"
" Python
au BufNewFile, BufRead *.py:
            \ set textwidth=79           " line length doesn't exceed 80 characters
            \ set fileformat=unix        " store file in unix format
" Full stack development
au BufNewFile, BufRead *.js, *.jsx, *.html, *.css:
            \ set tabstop=2
            \ set softtabstop=2
            \ set shiftwidth=2
autocmd Filetype javascript setlocal ts=2 sw=2 sts=0 noexpandtab
" CPP
autocmd FileType cpp setlocal ts=2 sts=2 sw=2

"--------------------------------"
"    Language Server Protocol    "
"--------------------------------"
" lua require'lspconfig'.bashls.setup{ on_attach=require'completion'.on_attach }
" lua require'lspconfig'.clangd.setup{ on_attach=require'completion'.on_attach }
" lua require'lspconfig'.clojure_lsp.setup{ on_attach=require'completion'.on_attach }
" lua require'lspconfig'.cssls.setup{ on_attach=require'completion'.on_attach }
" lua require'lspconfig'.dockerls.setup{ on_attach=require'completion'.on_attach }
" lua require'lspconfig'.gopls.setup{ on_attach=require'completion'.on_attach }
" lua require'lspconfig'.hie.setup{ on_attach=require'completion'.on_attach }
" lua require'lspconfig'.html.setup{ on_attach=require'completion'.on_attach }
" lua require'lspconfig'.jedi_language_server.setup{ on_attach=require'completion'.on_attach }
" lua require'lspconfig'.json_ls.setup{ on_attach=require'completion'.on_attach }
" lua require'lspconfig'.julials.setup{ on_attach=require'completion'.on_attach }
" lua require'lspconfig'.ocamllsp.setup{ on_attach=require'completion'.on_attach }
" lua require'lspconfig'.omnisharp.setup{ on_attach=require'completion'.on_attach }
" lua require'lspconfig'.omnisharp.setup{ on_attach=require'completion'.on_attach }
" lua require'lspconfig'.sqlls.setup{ on_attach=require'completion'.on_attach }
" lua require'lspconfig'.tsserver.setup{ on_attach=require'completion'.on_attach }

" nnoremap <leader>vd :lua vim.lsp.buf.definition()<CR>
" nnoremap <leader>vi :lua vim.lsp.buf.implementation()<CR>
" nnoremap <leader>vsh :lua vim.lsp.buf.signature_help()<CR>
" nnoremap <leader>vrr :lua vim.lsp.buf.references()<CR>
" nnoremap <leader>vrn :lua vim.lsp.buf.rename()<CR>
" nnoremap <leader>vh :lua vim.lsp.buf.hover()<CR>
" nnoremap <leader>vca :lua vim.lsp.buf.code_action()<CR>
"--------------------------------"
"            Telescope           "
"--------------------------------"
" let g:telescope_cache_results = 1
" let g:telescope_prime_fuzzy_find  = 1
" nnoremap <leader>pw :lua require('telescope.builtin').grep_string { search = vim.fn.expand("<cword>") }<CR>
" nnoremap <leader>ps :lua require('telescope.builtin').grep_string({ search = vim.fn.input("Grep For >")})<CR>
" nnoremap <C-p> :lua require('telescope.builtin').git_files()<CR>

"--------------------------------"
"            Mappings            "
"--------------------------------"
let g:completion_matching_strategy_list = ['exact', 'substring', 'fuzzy']
let g:TerminusFocusReporting=0
let mapleader   = "," " remap leader to ,
let g:mapleader = ","
" easy normal mode
imap jk <Esc>
imap kj <Esc>
" easy command input
nnoremap ; :

" Git
nnoremap <leader>gc :GBranches<CR>
nnoremap <leader>ga :Git fetch --all<CR>

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
" move lines with Shift + k/j
nnoremap <silent> <S-k> :m-2<cr>
nnoremap <silent> <S-j> :m+<cr>
inoremap <silent> <S-k> <Esc>:m-2<cr>i
inoremap <silent> <S-j> <Esc>:m+<cr>i

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

" close quickfix window
map <silent> <leader>gq :ccl<cr>

"--------------------------------"
"     React Native Mappings      "
"--------------------------------"
autocmd FileType javascript inoremap <leader>tx <Text></Text><Space><++>

"--------------------------------"
"      Compilation/Execution     "
"--------------------------------"
" C++
autocmd filetype cpp nnoremap <F4> :w<cr> :!clang++-5.0 -std=c++11 -Wall -g *.cpp && ./a.out<cr>
" Rust
autocmd filetype rust nnoremap <F4> :w<cr> :!rustc % && ./%:r
" Python
autocmd filetype python nnoremap <F4> :w<cr> :!python %<cr>

"================================================"
"                 Plugin Config                  "
"================================================"

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
"          Dasht                 "
"--------------------------------"
" search related docsets
" nnoremap <Leader>k :Dasht<Space>
" search ALL the docsets
" nnoremap <Leader><Leader>k :Dasht!<Space>
" use current window to show results
" let g:dasht_results_window = 'enew'

"--------------------------------"
"           Functions            "
"--------------------------------"
" Toggles folding
"  function ToggleFold()
"  	if foldlevel('.') == 0
"  		normal! 1
"  	else
"  		if foldclosed('.') < 0
"  			. foldclose
"  		else
"  			. foldopen
"  		endif
"  	endif
"  	echo
"  endfunction

" Toggles hybrid numbering
"  function ToggleNumber()
"      if (&relativenumber == 1)
"          set norelativenumber
"          set number
"      else
"          set relativenumber
"      endif
"  endfunction

" Cycle through location lists
"  function LNext(prev)
"  	try
"  		try
"  			if a:prev | lprev | else | lnext | endif
"  		catch /^Vim\%((a\+)\)\=:E553/
"  			if a:prev | llast | else | lfirst| endif
"  		catch /^Vim\%((\a\+)\)\=:E776/
"  		endtry
"  	catch /^Vim\%((\a\+)\)\=:E42/
"  	endtry
"  endfunction
