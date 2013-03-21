./splc --tail --dis-pa $1.spl > out.$1
./svm2 -stk-size 10000 $1 >> out.$1
