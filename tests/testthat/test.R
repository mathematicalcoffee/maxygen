
context("maxygen")

test_that("maxygen works", {

  expect_true(TRUE)

})

context("Maths parsing")
add.test <- function (inp, protected=inp, out=inp)
    list(inp=inp, protected=protected, out=out)

inline.math.tests <- list(
  add.test(
    "Two equations $a_i = b$ and $c = foo *bar* baz$ are cool.",
    "Two equations `inlineMath\u{007F}a_i = b` and `inlineMath\u{007F}c = foo *bar* baz` are cool.",
    "Two equations \\eqn{a_i = b} and \\eqn{c = foo *bar* baz} are cool."),
  add.test('$$fail$'),
  add.test('$fail$$'),
  add.test('$ fail$'),
  add.test('$fail $'),
  add.test('$$'),
  add.test('foo1$bar$baz'),
  add.test('The prices are $1.00 and $2.30'),
  # TOOO: should remove the backslash from the output?
  add.test('$succeed\\$$', '`inlineMath\u{007F}succeed\\$`', '\\eqn{succeed\\$}'),
  add.test('$\\$succeed$', '`inlineMath\u{007F}\\$succeed`', '\\eqn{\\$succeed}'),
  add.test('$succeed\\$succeed$', '`inlineMath\u{007F}succeed\\$succeed`', '\\eqn{succeed\\$succeed}'),
  # multiline
  add.test(
    "This is one line.\nFollowed by an equation $a = 1$\nand $b=2$ but $a \nto$b is ignored.",
    "This is one line.\nFollowed by an equation `inlineMath\u{007F}a = 1`\nand `inlineMath\u{007F}b=2` but $a \nto$b is ignored.",
    "This is one line.\nFollowed by an equation \\eqn{a = 1}\nand \\eqn{b=2} but $a\nto$b is ignored." # note: preceding/trailing space stripped
  )
)
display.math.tests <- list(
  add.test(
    "Here's a display equation $$e = mc^2,$$ and explanation",
    "Here's a display equation `displayMath\u{007F}e = mc^2,` and explanation",
    "Here's a display equation \\deqn{e = mc^2,} and explanation"
    
  ),
    # multiline..
  add.test(
    "Einstein found that\n$$\ne=mc^2,\n$$\n\nwhich is remarkable.",
    "Einstein found that\n`displayMath\u{007F}\ne=mc^2,\n`\n\nwhich is remarkable.",
    # Note: markdown_xml does space munging...
    #  preceding \n -> space, trailing spaces are stripped
    "Einstein found that\n\\deqn{ e=mc^2,}\n\nwhich is remarkable."
  ),
    # embedded dollar signs
  add.test(
    "Equation with $$ a=$b + $\\$c + \\$$d $$",
    "Equation with `displayMath\u{007F} a=$b + $\\$c + \\$$d `",
    # Note: markdown_xml does space munging...
    #  preceding \n -> space, trailing spaces are stripped
    "Equation with \\deqn{ a=$b + $\\$c + \\$$d}"
  )
)

mixed.tests <- list(
  add.test(
    "Mixed inline equation $a = b$ along with $$c = d$$ display equation.",
    "Mixed inline equation `inlineMath\u{007F}a = b` along with `displayMath\u{007F}c = d` display equation.",
    "Mixed inline equation \\eqn{a = b} along with \\deqn{c = d} display equation.")
)
misc.tests <- list(
  add.test("Don't break normal `code blocks`",
           "Don't break normal `code blocks`",
           "Don't break normal \\code{code blocks}")
)
run_tests <- function(tests, type=c('protect', 'replace')) {
    type <- match.arg(type)
    for (t in tests) {
        # TODO add test description
        if (type == 'protect') {
            expect_that(protect_maths(t$inp), equals(protect_maths(t$protected)))
        } else {
            expect_that(markdown(t$inp, tags), equals(protect_maths(t$out)))
        }
    }
}

test_that("protect_maths surrounds inline maths with `inlineMath\u{007F}`", {
    run_tests(inline.math.tests, 'protect')
})
test_that("protect_maths surrounds display maths with `displayMath\u{007F}`", {
    run_tests(display.math.tests, 'protect')
})
test_that("replace_maths replaces inline maths with \\eqn{}", {
    run_tests(inline.math.tests, 'replace')
})
test_that("replace_maths replaces display maths with \\deqn{}", {
    run_tests(display.math.tests, 'replace')
})
test_that("mixed inline and display equations parse appropriately", {
    run_tests(mixed.tests, 'protect')
    run_tests(mixed.tests, 'replace')
})
test_that("we don't break actual code blocks", {
    run_tests(misc.tests, 'protect')
    run_tests(misc.tests, 'replace')
})
