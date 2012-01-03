function! LoadRope()
python << EOF
import ropevim
EOF
endfunction

if has("ropevim")
  call LoadRope()
endif
