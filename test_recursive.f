

: testrecur recursive dup if 1- dup . cr testrecur then ;

: TEST 10 testrecur drop ;

