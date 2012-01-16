set mouse=a hlsearch bs=2 sw=2 ts=2 expandtab autoindent smartindent ruler incsearch
set bg=dark
set hidden
set history=5000
set ignorecase smartcase
set backspace=indent,eol,start

syntax on
filetype on
filetype indent on
filetype plugin on
filetype plugin indent on
"TODO: One day, figure out if the above are redundant.

runtime macros/matchit.vim
let mapleader = '\'

" Switch windows more fluidly in normal or insert mode.
" I never used the defaults for these keys anyway (if there are any).
nmap <c-h> <c-w>h
nmap <c-j> <c-w>j
nmap <c-k> <c-w>k
nmap <c-l> <c-w>l

imap <c-h> <c-o><c-w>h
imap <c-j> <c-o><c-w>j
imap <c-k> <c-o><c-w>k
imap <c-l> <c-o><c-w>l

" Fast buffer navigation
nmap <silent> <leader>l :bn<cr>
nmap <silent> <leader>h :bp<cr>

" Quickfix shortcuts
nmap <leader>, :cprevious<cr>
nmap <leader>. :cnext<cr>

"Better runtime paths
call pathogen#infect()

" I don't fully understand omnicomplete.
" set ofu=syntaxcomplete#Complete

set nu wildmenu

runtime! macros/matchit.vim

"Drupal Specific settings
if has("autocmd")
  " Drupal *.module files.
  augroup module
    autocmd BufRead *.module set filetype=php
    autocmd BufRead *.inc set filetype=php
    autocmd BufRead *.install set filetype=php
    autocmd BufRead *.engine set filetype=php
  augroup END
endif

"some rails stuff
augroup myfiletypes
  autocmd!
  autocmd FileType ruby,eruby,yaml set ai sw=2 sts=2 et
augroup END

command SW execute 'w !sudo tee % >/dev/null' | e! %
command DCC execute '! drush cache clear'


"make gd work for drupal
map gd :exe '/\s*function\s*'.expand("<cword>")<CR>
map <F11> :set hlsearch!<CR>
set grepprg=ack\ -a
map gr :grep <cword> site/sites/all

"-s0 allows nc to respond to ^D (EOF); see `man nc`
map <F5> :silent !echo "BrowserReload(); repl.quit();^D" \| nc -s0 192.168.2.122 4242;<CR><C-C><C-L>
set tags=./tags;/


" Rebuild the cScope database and reconnect to the server.
function ReScope()
  cs kill -1

  " Assume the top level of the project contains the .git folder
  let dir = finddir('.git', getcwd())

  execute "!cd " . dir

  "TODO: Read from a file or something; as I add more languages, this is
  "getting ridiculous.
  execute "!cscope -b -q -u -i<(find " . getcwd() . " -path '.git/*' -prune , -path '.svn/*' -prune  -name '*.module' -o -name '*.inc' -o -name '*.php' -o -name '*.install' -o -name '*.engine' -o -name '*.test' -o -name '*.theme' -o -name '*.js' -o -name '*.rb' -o -name '*.rhtml' -o -name '*.py' -o -name '*.yml' -o -name Rakefile -o -name Makefile -o -name '*.c' -o -name '*.cpp' -o -name '*.h' -o -name '*.hpp')"

  execute "cs add  " . getcwd() . "/cscope.out"
endfunction

if has("cscope")
  if findfile(getcwd() . '/cscope.out')
    execute "cs add " . getcwd() . "/cscope.out"
  endif
endif

comm! -nargs=0 RS call ReScope()

"inspired by http://cscope.sourceforge.net/cscope_maps.vim
map <C-\>p :tab split<CR>:exec("tag ".expand("<cword>"))<CR>

"Taken from http://www.thingy-ma-jig.co.uk/comment/7067#comment-7067
function! DrupalImplementsComment(fname)
  let hook = substitute(a:fname,"^[0-9a-zA-Z]\\+_","","")
  set paste

  exe "normal! O/**\<CR>"
  \          . " * Implements hook_" . hook . "().\<CR>"
  \          . " */\<Esc>"

  set nopaste
endfunction
nmap <C-\>o :cs find c <C-R>=expand("<cword>")<CR><CR>

" Easy clean up trailing whitespace.
nmap <silent> <leader>s :%s/[ \t]\+$//g<CR>

" Rope setup
if has("ropevim")
  let ropevim_vim_completion=1
  let ropevim_extend_complete=1
endif


