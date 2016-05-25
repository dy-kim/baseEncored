context('Converting number bases')

# 2 -> X
test_that("Convert base: 2 -> 2", {
  expect_equal(object = convertBase(11, 2, 2),
               expected =  '11')
})

test_that("Convert base: 2 -> 10", {
  expect_equal(object = convertBase(11, 2, 10),
               expected =  3)
})

test_that("Convert base: 2 -> 16", {
  expect_equal(object = convertBase(11, 2, 16),
               expected =  '3')
})

# 10 -> X
test_that("Convert base: 10 -> 2", {
  expect_equal(object = convertBase(11, 10, 2),
               expected =  '1011')
})

test_that("Convert base: 10 -> 10", {
  expect_equal(object = convertBase(11, 10, 10),
               expected =  11)
})

test_that("Convert base: 10 -> 16", {
  expect_equal(object = convertBase(11, 10, 16),
               expected =  'B')
})

# 16 -> X
test_that("Convert base: 16 -> 2", {
  expect_equal(object = convertBase(11, 16, 2),
               expected =  '10001')
})

test_that("Convert base: 16 -> 10", {
  expect_equal(object = convertBase(11, 16, 10),
               expected =  17)
})

test_that("Convert base: 16 -> 16", {
  expect_equal(object = convertBase(11, 16, 16),
               expected =  '11')
})
