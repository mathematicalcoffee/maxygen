
context("maxygen")

test_that("maxygen works", {

  expect_true(TRUE)

})

context("Maths parsing")
test_that("protect_maths surrounds inline maths with `inlineMath\r`", {
    test.inline <- "Two equations $a_i = b$ and $c = foo *bar* baz$ are cool."
    test.inline.o <- "Two equations `inlineMath\ra_i = b` and `inlineMath\rc = foo *bar* baz` are cool."
    expect_that(protect_maths(test.inline), equals(test.inline.o))

    more.test <- list(fail=c('$$fail$', '$fail$$', '$ fail$', '$fail $', '$$', 'foo1$bar$baz', 'The prices are $1.00 and $2.30'),
                      succeed=c('$succeed\\$$', '$\\$succeed$', '$succeed\\$succeed$'))
    more.test.o <- list(fail=more.test$fail,
                        succeed=paste0('`inlineMath\r',
                                       # TODO: remove the backslash?
                                      c('succeed\\$', '\\$succeed', 'succeed\\$succeed'),
                                      '`'))
    mapply(function (inp, out) {
             expect_that(protect_maths(inp), equals(out))
           }, unlist(more.test), unlist(more.test.o))

    # multiline
    test.multiline.inp <- "This is one line.\nFollowed by an equation $a = 1$\n and $b=2$."
    test.multiline.out <- "This is one line.\nFollowed by an equation `inlineMath\ra = 1`\n and `inlineMath\rb=2`."
    expect_that(protect_maths(test.multiline.inp), equals(test.multiline.out))
})
test_that("protect_maths surrounds display maths with `displayMath\r`", {
    # TODO
})
test_that("replace_maths replaces inline maths with \\eqn{}", {
    # TODO
})
test_that("replace_maths replaces display maths with \\deqn{}", {
    # TODO
})
