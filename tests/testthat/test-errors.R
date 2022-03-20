test_that("should not work", {
  library(ggplot2)

  expect_snapshot_error(
    print(ggplot(mpg, aes(x = manufacturer, y = displ)) +
      geom_boxplot() +
      stat_signif(
        comparisons = list(c("audi", "ford"), c("hyundai", "nissan")),
        map_signif_level = TRUE,
        inherit.aes = FALSE
      ))
  )

  expect_snapshot_error(
    print(ggplot(mpg, aes(x = manufacturer, y = displ)) +
      geom_boxplot() +
      geom_signif(
        comparisons = list(c("audi", "ford"), c("hyundai", "nissan")),
        map_signif_level = TRUE,
        inherit.aes = FALSE
      ))
  )

  expect_snapshot_error(
    print(ggplot(airquality_trimmed, aes(x = Month, y = Ozone, fill = Temp.f)) +
      geom_boxplot(alpha = 0.7, width = 0.4) +
      geom_signif(
        y_position = 150, xmin = c(0.9), xmax = c(1.1, 2.1),
        annotations = c("***", "NS")
      ))
  )

  expect_snapshot_error(
    print(ggplot(iris, aes(x = Species, y = Sepal.Length)) +
      geom_boxplot() +
      geom_signif(
        comparisons = list(c("versicolor", "virginica")),
        map_signif_level = TRUE,
        manual = TRUE
      ))
  )

  expect_snapshot_error(
    print(ggplot() +
      stat_signif(
        manual = TRUE,
        inherit.aes = FALSE
      ))
  )

  expect_snapshot_error(
    print(ggplot() +
      stat_signif(
        data = mtcars,
        mapping = aes("x", "y"),
        manual = TRUE,
        inherit.aes = FALSE
      ))
  )

  expect_snapshot_error(
    print(ggplot() +
      geom_signif(
        manual = TRUE,
        inherit.aes = FALSE
      ))
  )


  expect_snapshot_error(
    print(ggplot() +
      geom_signif(
        data = mtcars,
        mapping = aes("x", "y"),
        manual = TRUE,
        inherit.aes = FALSE
      ))
  )
})
