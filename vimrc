set mouse=a hlsearch bs=2 sw=2 ts=2 expandtab autoindent smartindent
syntax on 
set bg=dark

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
filetype plugin indent on
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

"cscope file-searching alternative
function SetCscope()
  let curdir = getcwd()
  while !filereadable("cscope.out") && getcwd() != "/"
    cd ..
  endwhile
  if filereadable("cscope.out")
    execute "cs add " . getcwd() . "/cscope.out"
  endif
  execute "cd " . curdir
endfunction

if has("cscope")
  call SetCscope()
endif

function ReScope()
  cs kill -1

  let dir = finddir('.git', '.;/var/shared/sites;/var/www;/home')
  execute "cd " . dir 
  cd ..

  execute "!cscope -b -q -u -i<(find " . getcwd() . " -path '.git/*' -prune , -path '.svn/*' -prune ,  \\( -name *.module -o -name *.inc -o -name *.php -o -name *.install -o -name *.engine -o -name *.test -o -name *.theme \\))"
  
  execute "cs add  " . getcwd() . "/cscope.out"
endfunction

comm! -nargs=0 RS call ReScope()

"inspired by http://cscope.sourceforge.net/cscope_maps.vim
map <C-\>p :tab split<CR>:exec("tag ".expand("<cword>"))<CR>
nmap <C-\>o :cs find c <C-R>=expand("<cword>")<CR><CR>	

