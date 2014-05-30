context("credentials")

un = "test_user"
key = "0f9es4r6tm"
extra = list(server="https://plot.ly/")

test_that("setting username and key works", {
  set_credentials_file(un, key)
  creds = get_credentials_file()
  expect_equivalent(creds$username, un)
  expect_equivalent(creds$api_key, key)
})

test_that("setting extra keys work", {
  set_credentials_file(un, key, extra=extra)
  creds = get_credentials_file()
  expect_equivalent(creds$username, un)
  expect_equivalent(creds$api_key, key)
  expect_equivalent(creds$server, "https://plot.ly/")
})

test_that("setting extra keys don't overwrite existing keys", {
  set_credentials_file(un, key, extra=extra)
  set_credentials_file(un, key, extra=list(another="key"))
  creds = get_credentials_file()
  expect_equivalent(creds$username, un)
  expect_equivalent(creds$api_key, key)
  expect_equivalent(creds$server, "https://plot.ly/")
  expect_equivalent(creds$another, "key")
})

test_that("overwriting keys work", {
  set_credentials_file(un, key, extra=extra)
  creds = get_credentials_file()
  expect_equivalent(creds$username, un)
  expect_equivalent(creds$api_key, key)
  expect_equivalent(creds$server, "https://plot.ly/")

  set_credentials_file("new_un", "new_key", extra=list(server="new_server"))
  creds = get_credentials_file()
  expect_equivalent(creds$username, "new_un")
  expect_equivalent(creds$api_key, "new_key")
  expect_equivalent(creds$server, "new_server")
})