test_that("l'écrêtage respecte exactement les deux bornes", {
  df <- data.frame(x = c(-10, 0, 10, 25, 50, 100, 150, NA_real_))
  etape <- list(
    type = "arrondir",
    variable = "x",
    mode = "ecreter",
    borne_basse = 10,
    borne_haute = 100
  )

  res <- appliquer_etape(df, etape)

  expect_equal(res$x, c(10, 10, 10, 25, 50, 100, 100, NA_real_))
})

test_that("le code R généré reproduit l'écrêtage", {
  etape <- list(
    type = "arrondir",
    variable = "x",
    mode = "ecreter",
    borne_basse = 10,
    borne_haute = 100
  )

  code <- etape_vers_code(etape)
  expect_equal(code, "df$`x` <- pmin(pmax(df$`x`, 10), 100)")

  df <- data.frame(x = c(-10, 0, 10, 25, 50, 100, 150, NA_real_))
  direct <- appliquer_etape(df, etape)$x
  generated <- eval(parse(text = code), envir = df)$x

  expect_equal(generated, direct)
})

test_that("l'écrêtage accepte des bornes égales", {
  df <- data.frame(x = c(1, 5, 10, NA_real_))
  etape <- list(type="arrondir", variable="x", mode="ecreter", borne_basse=5, borne_haute=5)
  expect_equal(appliquer_etape(df, etape)$x, c(5, 5, 5, NA_real_))
})
