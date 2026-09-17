test_that(".onLoad's filesystem cache option matches the memoise cache directory", {
  # Regression test: .onLoad() used to create the memoise disk cache in a
  # directory literally named "filesystem" relative to the working directory,
  # while getOption("f1dataR.cache") pointed at the rappdirs location -- the
  # two caches diverged and a stray "filesystem/" directory was left in the CWD.
  skip_on_cran()
  skip_if_not_installed("callr")

  test_wd <- withr::local_tempdir("tst_onload_wd")

  pkg_path <- Sys.getenv("F1DATAR_PKG_ROOT", unset = NA)
  if (is.na(pkg_path)) {
    # Fall back to searching upward from the test file for the DESCRIPTION.
    candidate <- testthat::test_path("..", "..")
    if (file.exists(file.path(candidate, "DESCRIPTION"))) {
      pkg_path <- normalizePath(candidate)
    } else {
      pkg_path <- normalizePath(find.package("f1dataR"))
    }
  }

  result <- callr::r(
    function(wd, pkg_path) {
      setwd(wd)
      options(f1dataR.cache = "filesystem")
      pkgload::load_all(pkg_path, quiet = TRUE)
      list(
        option = getOption("f1dataR.cache"),
        relative_dir_exists = dir.exists(file.path(wd, "filesystem")),
        has_cache = memoise::has_cache(load_circuits)
      )
    },
    args = list(wd = test_wd, pkg_path = pkg_path),
    libpath = .libPaths()
  )

  expect_false(result$relative_dir_exists)
  expect_true(dir.exists(result$option))
  expect_false(basename(result$option) == "filesystem")
})
