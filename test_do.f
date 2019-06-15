: test 10 0 ?do i .  cr loop ;
: test2 3 0 ?do
          3 0 ?do
            3 0 ?do
              i . space j . space k . cr
            loop loop loop ;


: test3 10 0 ?do i dup 5 = if drop unloop exit then . cr loop ;

: TEST test test2 test3 ;
