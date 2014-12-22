set mouse=a hlsearch bs=2 sw=2 ts=2 autoindent smartindent ruler incsearch
set fileformats=unix
set bg=dark
set hidden
set history=5000
set ignorecase smartcase
set backspace=indent,eol,start
let icase=1

"Better runtime paths
call pathogen#infect()
call pathogen#helptags()

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

imap <c-h> <esc><c-w>h
imap <c-j> <esc><c-w>j
imap <c-k> <esc><c-w>k
imap <c-l> <esc><c-w>l

" vim-unimpaired has better shortcuts for these.
"
" Fast buffer navigation
nmap <silent> <leader>l :bn<cr>
nmap <silent> <leader>h :bp<cr>
"
" Quickfix shortcuts
nmap <leader>, :cprevious<cr>
nmap <leader>. :cnext<cr>

" Quickfix window
nmap <leader>q :copen<cr>
nmap <leader>c :ccl<cr>

" I don't fully understand omnicomplete.
" set ofu=syntaxcomplete#Complete

set nu wildmenu

runtime! macros/matchit.vim

"some rails stuff
augroup myfiletypes
  autocmd!
  autocmd FileType ruby,eruby,yaml set ai sw=2 sts=2 et
augroup END

command SW execute 'w !sudo tee % >/dev/null' | e! %
command E execute 'Explore'

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
  execute "!cscope -b -q -u -i<(find " . getcwd() . " -path '.git/*' -prune , -path '.svn/*' -prune  -name '*.module' -o -name '*.inc' -o -name '*.java' -o -name '*.install' -o -name '*.engine' -o -name '*.test' -o -name '*.theme' -o -name '*.js' -o -name '*.rb' -o -name '*.rhtml' -o -name '*.py' -o -name '*.yml' -o -name Rakefile -o -name Makefile -o -name '*.c' -o -name '*.cpp' -o -name '*.h' -o -name '*.hpp' -o -name '*.js' -o -name '*.cs')"

  execute "cs add  " . getcwd() . "/cscope.out"
endfunction

if has("cscope")
	set cscopequickfix=s-,c-,d-,i-,t-,e-,g-
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

" Smart completion, based on CleverTab and Schultz's improvement (Hacking vim
" 2007)
function! AutoComplete() 
  if &omnifunc != ''
    return "\<C-x>\<C-o>"
  " Figure out how to check if tags will work...
  else
    return "\<C-n>"
  endif
endfunction

" Call me eclipse, but I like ctrl-space
imap <Nul> <C-R>=AutoComplete()<cr>

" Customise tasklist.
let g:tlTokenList = ["TODO", "FIXME", "@todo", "@fixme"]

" Custom jslint config
if !exists("g:syntastic_javascript_jslint_conf")
    let g:syntastic_javascript_jslint_conf = "--nomen --white --regexp --plusplus --vars --continue --stupid"
endif
 
" let g:node_usejscomplete = 1
" autocmd FileType javascript :setl omnifunc=jscomplete#CompleteJS

" automatically open and close the popup menu / preview window
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif

" Shortcuts for fireplace.vim
" TODO: look into using autocmd FileType clojure... to prevent shortcut
" collisions.
map <leader>e :Eval<cr>
nmap <leader>E vip:Eval<cr>
comm! -nargs=0 EF %Eval


" vim-javascript
let javascript_enable_domhtmlcss=1
