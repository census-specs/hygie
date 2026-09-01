test_that("hygie_parse_date_auto reconnaît plusieurs formats dans une même colonne", {
  x <- c(
    "15/01/2024",
    "2024-01-16",
    "17-01-2024",
    "18 janvier 2024",
    "20240119",
    "20/01/24",
    "2024/01/21 14:35:20",
    "45292",
    "valeur_invalide",
    NA_character_
  )

  res <- hygie_parse_date_auto(x)

  expect_s3_class(res, "Date")
  expect_equal(sum(!is.na(res)), 8)
  expect_equal(as.character(res[1]), "2024-01-15")
  expect_equal(as.character(res[2]), "2024-01-16")
  expect_equal(as.character(res[3]), "2024-01-17")
  expect_equal(as.character(res[4]), "2024-01-18")
  expect_equal(as.character(res[5]), "2024-01-19")
  expect_equal(as.character(res[6]), "2024-01-20")
  expect_equal(as.character(res[7]), "2024-01-21")
  expect_equal(as.character(res[8]), "2024-01-01")
  expect_true(is.na(res[9]))
  expect_true(is.na(res[10]))
})

test_that("les dates ambiguës privilégient le format jour/mois", {
  res <- hygie_parse_date_auto(c("01/02/2024", "02/03/2024"))

  expect_equal(as.character(res), c("2024-02-01", "2024-03-02"))
})

test_that("la formule générée reproduit le parseur automatique", {
  df <- data.frame(
    date = c("15/01/2024", "2024-01-16", "18 janvier 2024", "20240119", "invalide"),
    stringsAsFactors = FALSE
  )

  formule <- hygie_date_auto_formula("date")
  res_code <- eval(parse(text = formule), envir = df)
  res_direct <- hygie_parse_date_auto(df$date)

  expect_identical(res_code, res_direct)
})

test_that("le parseur conserve les NA et produit toujours un vecteur Date", {
  res <- hygie_parse_date_auto(c(NA, "", "N/A", "2024-02-29"))

  expect_s3_class(res, "Date")
  expect_true(all(is.na(res[1:3])))
  expect_equal(as.character(res[4]), "2024-02-29")
})