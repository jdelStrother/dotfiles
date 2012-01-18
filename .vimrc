set nocompatible
filetype off " force reloading of filetyp set rtp+=~/.vim/bundle/vundle/

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
Bundle 'gmarik/vundle'

" repos on github
Bundle 'AndrewRadev/linediff.vim'
Bundle 'airblade/vim-rooter'
Bundle 'kana/vim-textobj-user'
Bundle 'msanders/cocoa.vim'
Bundle 'nelstrom/vim-textobj-rubyblock'
Bundle 'sjl/gundo.vim'
Bundle 'sjl/threesome.vim'
Bundle 'topfunky/PeepOpen-EditorSupport', {'rtp': 'vim-peepopen/'}
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-cucumber'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-haml'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'

" repos on Vim-Scripts
Bundle 'bufexplorer.zip'
Bundle 'EasyMotion'
Bundle 'taglist.vim'
Bundle 'vim-coffee-script'
Bundle 'YankRing.vim'


set backupdir=~/.vim/tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim/tmp,~/.tmp,~/tmp,/var/tmp,/tmp

set modelines=0
set scrolloff=3 " show 3 lines of context around scrolling cursor
set ttyfast

set shiftwidth=2
set shiftround
set softtabstop=2
set tabstop=2
set expandtab

set virtualedit+=block

let mapleader=" "
let maplocalleader="\\"

set wildignore+=files/**,public/files/**,*.log
let g:fuzzy_ignore = "*.log,tmp/*,files/*,public/files/*"
let g:fuzzy_matching_limit = 70

map <leader>d :execute 'Explore ' . getcwd()<CR>
map <leader>D :Explore<CR>
map <leader>u :GundoToggle<CR>
" fold everything except the current block
nnoremap <leader>z zMzv

map <C-s> :write<CR>
imap <C-s> <Esc><C-s>
map <C-t> Xp
imap <C-t> <C-o>X<C-o>p

imap jj <Esc>

let g:EasyMotion_leader_key = '\'

" Open a Quickfix window for the last search
nnoremap <silent> <leader>/ :execute 'vimgrep /'.@/.'/g %'<CR>:copen<CR>

" :w!! for sudo-save
cmap w!! w !sudo tee % >/dev/null

" I keep accidentally middle clicking on my trackpad, which pastes shit
nnoremap <MiddleMouse> <Nop>
nnoremap <2-MiddleMouse> <Nop>
nnoremap <3-MiddleMouse> <Nop>
nnoremap <4-MiddleMouse> <Nop>

inoremap <MiddleMouse> <Nop>
inoremap <2-MiddleMouse> <Nop>
inoremap <3-MiddleMouse> <Nop>
inoremap <4-MiddleMouse> <Nop>

" allow buffers to go to the background without forcing you to save them first
" set hidden
" auto-write buffers before switching away
set autowriteall

" show completions on tab
set wildmenu

" Turn off search highlight temporarily
nmap <silent> <leader>n :silent :nohlsearch<CR>

" Show trailing spaces
set listchars=tab:▸\ ,eol:¬,trail:·
nmap <silent> <leader>s :set nolist!<CR>

" don't show unnecessary 'press enter to continue' prompts'
" set shortmess=atI

" Open :help documents in a nice, big vertical split instead of a horizontal one:
au FileType help wincmd L

set foldmethod=indent
set foldlevelstart=999 " don't auto-fold on opening files
nnoremap <leader><space> za
"au BufWinLeave ?* silent! mkview
"au BufWinEnter ?* silent! loadview

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

set nobackup		" do not keep a backup file, use versions instead
set noswapfile
set history=1000	" keep 1000 lines of command line history
set undolevels=1000
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching
if exists('+undofile')
  set undofile
  set undodir=~/.vim/tmp/undo
end

" highlight the cursor's line in the current window:
autocmd WinEnter * setlocal cursorline
autocmd WinLeave * setlocal nocursorline

" ignore case in searching, unless there's a capital letter in the search
" phrase
set ignorecase
set smartcase

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
  set mouse=a
endif

" Setup syntastic for syntax checking
let g:syntastic_enable_signs=1
let g:syntastic_auto_loc_list=1
let g:syntastic_disabled_filetypes = ['haml', 'sass']

let g:rails_statusline=0
set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P
set laststatus=2 "always show status line

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif
if has("gui_running")
  set guifont=Menlo:h11
  colorscheme molokai

  " I can't get 'hi link EasyMotionShade  Comment' to work :(
  hi EasyMotionShade term=bold ctermfg=11 guifg=#5c7176

  " No toolbar
  set guioptions-=T
  " get rid of left/right scrollbars
  set guioptions-=L
  set guioptions-=r

  " Use a line-drawing char for pretty verticl splits
  set fillchars+=vert:│

  " Different cursors for different modes.
  set guicursor=n-c:block-Cursor-blinkon0
  set guicursor+=v:block-vCursor-blinkon0
  set guicursor+=i-ci:ver20-iCursor

  " Make macvim take up the entire screen in fullscreen mode
  if exists('+fuoptions')
    set fuoptions=maxvert,maxhorz
  endif

  map <unique> <silent> <Leader>t <Plug>PeepOpen

else
  colorscheme reliable
end
" needs to be run after loading the color scheme

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  " Also don't do it when the mark is in the first line, that is the default
  " position when opening a file.
  autocmd BufReadPost *
    \ if expand('%.')!='.git/index' && line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  autocmd BufNewFile,BufRead *.rbapi set filetype=ruby

  augroup END

else

  set autoindent  " always set autoindenting on

endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif

