library(ggplot2)

test_that("method which return text works - snapshot", {
  magnitude_test <- function(x, y, ...) {
    change <- mean(y) / mean(x)
    p <- t.test(x, y)$p.value
    stars <- if (p < 0.001) {
      "***"
    } else if (p < 0.01) {
      "**"
    } else if (p < 0.05) {
      "*"
    } else {
      ""
    }
    list(p.value = paste0(signif(change, digits = 2), stars))
  }

  p <- ggplot(mpg, aes(x = manufacturer, y = displ)) +
    geom_boxplot() +
    stat_signif(
      comparisons = list(c("audi", "ford"), c("hyundai", "nissan")),
      test = magnitude_test,
      margin_top = 0.02, step_increase = 0, tip_length = 0.01
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~ as.factor(year), scale = "free")

  pb <- ggplot2::ggplot_build(p)

  expect_snapshot(pb$data)
})


test_that("identical annotations are plotted separetly - snapshot", {
  dat <- data.frame(
    Group = c("S1", "S1", "S2", "S2"),
    Sub = c("A", "B", "A", "B"),
    Value = c(3, 5, 7, 8)
  )

  p <- ggplot(dat, aes(Group, Value)) +
    geom_bar(aes(fill = Sub), stat = "identity", position = "dodge", width = .5) +
    geom_signif(
      comparisons = list(c("S1", "S2")), annotations = "***",
      y_position = 9.3, tip_length = 0, vjust = 0.4
    ) +
    geom_signif(
      stat = "identity",
      data = data.frame(
        x = c(0.875, 1.875), xend = c(1.125, 2.125),
        y = c(5.8, 8.5), annotation = c("**", "**")
      ),
      aes(
        x = x,
        xend = xend,
        y = y,
        yend = y,
        annotation = annotation,
        group = c(1, 2)
      )
    ) +
    scale_fill_manual(values = c("grey80", "grey20"))

  pb <- ggplot2::ggplot_build(p)

  expect_snapshot(pb$data)
})
