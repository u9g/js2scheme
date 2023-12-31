// transformed to: (define (double x) (* x x))
function double(x) {
  return x * x;
}

// Template strings are printed verbatim
`(double 5) ; returns 25`;

// Function names are printed verbatim.
// transformed to: (define (to_the_fourth x) (double (double x)))
function to_the_fourth(x) {
  return double(double(x));
}

// Function calls are translated.
// Will returns 625.
// transformed to: (to_the_fourth 5)
to_the_fourth(5);

// Will transform ternary to if statement.
// transformed to: (define (dot list1 list2) (if (= (length list1) 0) 0 (+ (* (car list1) (car list2)) (dot (cdr list1) (cdr list2)))))
function dot(list1, list2) {
  return length(list1) === 0
    ? 0
    : car(list1) * car(list2) + dot(cdr(list1), cdr(list2));
}

// Will replace array destructuring with the proper car/cdr as expected.
// If first1 is in quotes, it will not translate additional expressions since it is literally replaced.
// IE: return "first1"      // => car(list1)
//     return first1        // => car(list1)
//     return "first1 + 2"  // => first1 + 2
function dot2(list1, list2) {
  if (length(list1) === 0) {
    return 0;
  }

  const [first1, ...rest1] = list1;
  const [first2, ...rest2] = list2;
  return first1 * first2 + dot2(rest1, rest2);
}

// "x % 2 === 0" will be transformed to "even?" call
// transformed to: (define (is_even x) (even? x))
function is_even(x) {
  return x % 2 === 0;
}

// "true" will be transformed to "#t", and "false" will be transformed to "#f".
// transformed to: (define (is_true x) (= x #t))
function is_true(x) {
  return x === true;
}

// Arrow expressions will be translated to lambdas.
// transformed to: (define (double_list lst) (map (lambda (el) (* el el)) lst))
function double_list(lst) {
  return map((el) => el * el, lst);
}

// "let/const/var" are all transformed to #define's
// transformed to: (define doubler (lambda (el) (* el el)))
let doubler = (el) => el * el;

// Will print 25.
doubler(5);

// transformed to: (define (and_all_bools bools_list) (foldl (lambda (a store) (and a store)) #t bools_list))
function and_all_bools(bools_list) {
  return foldl((a, store) => a && store, true, bools_list);
}

// Will print false.
and_all_bools([true, false, true]);

// Will print true.
and_all_bools([true, true, true]);

// Will print true.
and_all_bools(cons(true, [true, true]));

// Strings are printed directly.
// transformed to: (equal? (list 123) (list 123))
// Will print true.
"equal?"([123], [123]);

// transformed to: (eqv? (list 123) (list 123))
// Will print false.
"eqv?"([123], [123]);

// Strings anywhere are printed directly.
// transformed to: (map = (list 1 2 3) (list 1 2 4))
// Will print '(#t #t #f).
map("=", [1, 2, 3], [1, 2, 4]);

function returns_fn_that_will_add_b_to_first_element_of_a(a) {
  const [first] = a;
  return [
    (b) => first + b,
    (c) => {
      if (c > 9) {
        return 5;
      }
      return 10;
    },
  ];
}

function arith(a) {
  return (b) => (c) => b(a, c);
}

// You can call curried functions.
arith(1)("*")(2);
