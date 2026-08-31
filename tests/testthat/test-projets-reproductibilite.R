test_that("le recodage génère du code R explicite", {
  etape <- list(
    type = "recoder",
    variable = "sexe",
    correspondance = c(F = "Féminin", M = "Masculin")
  )

  code <- etape_vers_code(etape)

  expect_match(code, 'dplyr::recode')
  expect_match(code, '"F" = "Féminin"')
  expect_match(code, '"M" = "Masculin"')
  expect_false(grepl("!!!", code, fixed = TRUE))
})

test_that("le générateur conserve les autres types d'étapes", {
  etape <- list(
    type = "espaces",
    variable = "nom",
    mode = "bords"
  )

  code <- etape_vers_code(etape)

  expect_match(code, "trimws")
})
