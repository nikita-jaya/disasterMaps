# gganimate::animate / anim_save are mocked so no frames are rendered.

local_mock_animate <- function(.env = parent.frame()) {
  calls <- new.env()
  local_mocked_bindings(
    animate = function(plot, ...) {
      calls$plot <- plot
      calls$args <- list(...)
      structure(list(), class = "fake_animation")
    },
    anim_save = function(filename, animation, ...) {
      calls$saved <- filename
      invisible(filename)
    },
    .package = "gganimate",
    .env = .env
  )
  calls
}

test_that("population_plot_animate builds one frame per ds/hour window", {
  local_globals(tiles_3857 = make_tiles_3857())
  calls <- local_mock_animate()

  out <- population_plot_animate(tzone = "UTC", renderer = "stub")
  expect_s3_class(out, "fake_animation")

  p <- calls$plot
  expect_s3_class(p$transition, "TransitionManual")
  expect_identical(p$labels$subtitle, "{frame_label}")

  # the population layer now holds every window, tagged with a frame number
  dat <- p$layers[[1]]$data
  expect_equal(nrow(dat), 8)
  expect_setequal(dat$frame, 1:2)
  expect_setequal(dat$frame_label, c("2025-01-07 0800", "2025-01-08 1600"))
  expect_true(all(dat$frame[dat$hour == "0800"] == 1))
})

test_that("population_plot_animate passes fps / duration / renderer to animate", {
  local_globals(tiles_3857 = make_tiles_3857())
  calls <- local_mock_animate()
  population_plot_animate(tzone = "UTC", fps = 3, duration = 4, renderer = "stub")
  expect_identical(calls$args$fps, 3)
  expect_identical(calls$args$duration, 4)
  expect_identical(calls$args$renderer, "stub")
})

test_that("population_plot_animate picks the plot function from metric", {
  local_globals(tiles_3857 = make_tiles_3857())
  calls <- local_mock_animate()

  population_plot_animate(metric = "crisis", tzone = "UTC", renderer = "stub")
  expect_identical(calls$plot$scales$get_scales("fill")$name, "Users")

  population_plot_animate(metric = "difference", tzone = "UTC", renderer = "stub")
  expect_identical(calls$plot$scales$get_scales("fill")$name, "Users (crisis - baseline)")

  expect_error(population_plot_animate(metric = "zscore", tzone = "UTC", renderer = "stub"))
})

test_that("population_plot_animate filters windows by ds_values and hour_values", {
  local_globals(tiles_3857 = make_tiles_3857())
  calls <- local_mock_animate()

  population_plot_animate(tzone = "UTC", ds_values = as.Date("2025-01-08"), renderer = "stub")
  dat <- calls$plot$layers[[1]]$data
  expect_equal(nrow(dat), 4)
  expect_true(all(dat$frame == 1))
  expect_true(all(dat$hour == "1600"))

  population_plot_animate(tzone = "UTC", hour_values = "0800", renderer = "stub")
  expect_true(all(calls$plot$layers[[1]]$data$hour == "0800"))
})

test_that("population_plot_animate errors when no windows match", {
  local_globals(tiles_3857 = make_tiles_3857())
  local_mock_animate()
  expect_error(
    population_plot_animate(tzone = "UTC", hour_values = "2359", renderer = "stub"),
    "No matching ds/hour combinations"
  )
})

test_that("population_plot_animate forwards extra arguments to the plot function", {
  local_globals(tiles_3857 = make_tiles_3857())
  calls <- local_mock_animate()
  population_plot_animate(tzone = "UTC", plot_title = "Animated", renderer = "stub")
  expect_identical(calls$plot$labels$title, "Animated")
})

test_that("population_plot_animate only saves when a filename is given", {
  local_globals(tiles_3857 = make_tiles_3857())
  calls <- local_mock_animate()

  population_plot_animate(tzone = "UTC", renderer = "stub")
  expect_null(calls$saved)

  population_plot_animate(tzone = "UTC", renderer = "stub", filename = "out.gif")
  expect_identical(calls$saved, "out.gif")
})
