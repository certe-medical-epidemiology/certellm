# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
# ===================================================================== #

# Tests for internal universal helpers: like(), %like%, %unlike%, concat(),
# and read_secret().  None of these require an LLM.

# Import unexported infix operators so they can be used with standard infix
# syntax inside tests (under R CMD check only exported symbols are on the
# search path; operators need to be locally defined here).
`%like%`   <- certellm:::`%like%`
`%unlike%` <- certellm:::`%unlike%`

# ---------------------------------------------------------------------------
# like()
# ---------------------------------------------------------------------------

test_that("like() returns TRUE for a matching pattern", {
  expect_true(certellm:::like("Hello World", "hello"))
  expect_true(certellm:::like("Hello World", "world"))
})

test_that("like() returns FALSE for a non-matching pattern", {
  expect_false(certellm:::like("Hello World", "xyz"))
})

test_that("like() is case-insensitive", {
  expect_true(certellm:::like("APPLE", "apple"))
  expect_true(certellm:::like("apple", "APPLE"))
  expect_true(certellm:::like("ApPlE", "aPpLe"))
})

test_that("like() supports regex patterns", {
  expect_true(certellm:::like("abc123", "[0-9]+"))
  expect_false(certellm:::like("abcdef", "^[0-9]"))
  expect_true(certellm:::like("test@email.com", "@"))
  expect_true(certellm:::like("2024-01-15", "^[0-9]{4}-"))
})

test_that("like() vectorizes over x with a single pattern", {
  x <- c("apple", "banana", "cherry")
  expect_equal(certellm:::like(x, "an"), c(FALSE, TRUE, FALSE))
  expect_equal(certellm:::like(x, "^[ab]"), c(TRUE, TRUE, FALSE))
})

test_that("like() with a single x and multiple patterns returns a logical vector", {
  result <- certellm:::like("apple", c("app", "ban", "cher"))
  expect_equal(result, c(TRUE, FALSE, FALSE))
})

test_that("like() errors when x and pattern have different lengths > 1", {
  expect_error(
    certellm:::like(c("a", "b"), c("a", "b", "c")),
    "'x' and 'pattern' must be of equal length"
  )
})

test_that("like() handles empty strings", {
  expect_true(certellm:::like("", ""))
  expect_false(certellm:::like("abc", "^$"))
  expect_true(certellm:::like("", "^$"))
})

test_that("like() handles NA input gracefully (grepl returns NA)", {
  result <- certellm:::like(NA_character_, "abc")
  expect_true(is.na(result))
})

# ---------------------------------------------------------------------------
# %like% and %unlike%
# ---------------------------------------------------------------------------

test_that("%like% operator matches correctly", {
  expect_true("Hello" %like% "hel")
  expect_false("Hello" %like% "xyz")
})

test_that("%unlike% operator negates correctly", {
  expect_false("Hello" %unlike% "hel")
  expect_true("Hello" %unlike% "xyz")
})

test_that("%like% vectorizes properly", {
  result <- c("cat", "dog", "catfish") %like% "cat"
  expect_equal(result, c(TRUE, FALSE, TRUE))
})

test_that("%unlike% vectorizes properly", {
  result <- c("cat", "dog", "catfish") %unlike% "cat"
  expect_equal(result, c(FALSE, TRUE, FALSE))
})

test_that("%like% and %unlike% are logical inverses", {
  x <- c("foo", "bar", "foobar", "baz")
  pat <- "foo"
  expect_equal(x %like% pat, !(x %unlike% pat))
})

test_that("%like% supports regex", {
  expect_true("abc123" %like% "[0-9]+")
  expect_false("abcdef" %like% "[0-9]+")
})

# ---------------------------------------------------------------------------
# concat()
# ---------------------------------------------------------------------------

test_that("concat() concatenates multiple strings", {
  expect_equal(certellm:::concat("a", "b", "c"), "abc")
  expect_equal(certellm:::concat("hello", " ", "world"), "hello world")
})

test_that("concat() returns empty string for single empty input", {
  expect_equal(certellm:::concat(""), "")
})

test_that("concat() handles a single string", {
  expect_equal(certellm:::concat("only"), "only")
})

test_that("concat() handles a character vector as one argument", {
  expect_equal(certellm:::concat(c("a", "b", "c")), "abc")
})

test_that("concat() mixes vector and scalar arguments", {
  expect_equal(certellm:::concat(c("a", "b"), "c"), "abc")
})

test_that("concat() returns a character of length 1", {
  result <- certellm:::concat("x", "y", "z")
  expect_type(result, "character")
  expect_length(result, 1L)
})

# ---------------------------------------------------------------------------
# read_secret()
# ---------------------------------------------------------------------------

test_that("read_secret() warns and returns '' when secrets_file env var not set", {
  old <- Sys.getenv("secrets_file", unset = NA_character_)
  Sys.unsetenv("secrets_file")
  on.exit({
    if (!is.na(old)) Sys.setenv(secrets_file = old)
  }, add = TRUE)

  expect_warning(
    result <- certellm:::read_secret("anything"),
    "environmental variable 'secrets_file' not set"
  )
  expect_equal(result, "")
})

test_that("read_secret() warns and returns '' when property not found", {
  tmpfile <- tempfile(fileext = ".yaml")
  writeLines("somekey: somevalue", tmpfile)
  on.exit(unlink(tmpfile), add = TRUE)

  expect_warning(
    result <- certellm:::read_secret("nonexistent", file = tmpfile),
    "property 'nonexistent' not found"
  )
  expect_equal(result, "")
})

test_that("read_secret() returns a scalar string value", {
  tmpfile <- tempfile(fileext = ".yaml")
  writeLines("mykey: myvalue", tmpfile)
  on.exit(unlink(tmpfile), add = TRUE)

  result <- certellm:::read_secret("mykey", file = tmpfile)
  expect_equal(result, "myvalue")
})

test_that("read_secret() returns a numeric value as-is", {
  tmpfile <- tempfile(fileext = ".yaml")
  writeLines("mynum: 42", tmpfile)
  on.exit(unlink(tmpfile), add = TRUE)

  result <- certellm:::read_secret("mynum", file = tmpfile)
  expect_equal(result, 42)
})

test_that("read_secret() returns a nested list", {
  tmpfile <- tempfile(fileext = ".yaml")
  writeLines(c("parent:", "  child: hello"), tmpfile)
  on.exit(unlink(tmpfile), add = TRUE)

  result <- certellm:::read_secret("parent", file = tmpfile)
  expect_type(result, "list")
  expect_equal(result$child, "hello")
})

test_that("read_secret() reads the correct property among multiple keys", {
  tmpfile <- tempfile(fileext = ".yaml")
  writeLines(c("key1: value1", "key2: value2", "key3: value3"), tmpfile)
  on.exit(unlink(tmpfile), add = TRUE)

  expect_equal(certellm:::read_secret("key1", file = tmpfile), "value1")
  expect_equal(certellm:::read_secret("key2", file = tmpfile), "value2")
  expect_equal(certellm:::read_secret("key3", file = tmpfile), "value3")
})

test_that("read_secret() uses explicit file argument, bypassing env var check", {
  # Even when secrets_file env var is unset, an explicit file path works
  old <- Sys.getenv("secrets_file", unset = NA_character_)
  Sys.unsetenv("secrets_file")
  on.exit({
    if (!is.na(old)) Sys.setenv(secrets_file = old)
  }, add = TRUE)

  tmpfile <- tempfile(fileext = ".yaml")
  writeLines("explicit: works", tmpfile)
  on.exit(unlink(tmpfile), add = TRUE)

  result <- certellm:::read_secret("explicit", file = tmpfile)
  expect_equal(result, "works")
})
