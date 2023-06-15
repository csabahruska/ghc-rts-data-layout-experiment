set -x -e

DIR=/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/foundation-pak/foundation-pak-ghc-9.2.7-wpc/ghc-wpc/_build/stage1/rts/build/extra-compilation-artifacts/wpc-plugin/cbits-preprocessed-source

find $DIR -name '*.thr_debug_o_i' -type f

globvars `find $DIR -name '*.thr_debug_o_i' -type f` > rts-global-vars.txt
