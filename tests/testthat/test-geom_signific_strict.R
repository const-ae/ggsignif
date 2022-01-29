test_that("the plotting works - strict test", {
  # don't run on CRAN; allows ggplot2 people to make changes to internal dataframes
  skip_on_cran()
  skip_if(getRversion() < "4.0")

  library(ggplot2)

  dat <- data.frame(
    Group = c("S1", "S1", "S2", "S2"),
    Sub = c("A", "B", "A", "B"),
    Value = c(3, 5, 7, 8)
  )

  p <- ggplot(dat, aes(Group, Value)) +
    geom_bar(aes(fill = Sub),
      stat = "identity",
      position = "dodge",
      width = .5
    ) +
    geom_signif(
      stat = "identity",
      data = data.frame(
        m = c(0.875, 1.875),
        xend = c(1.125, 2.125),
        n = c(5.8, 8.5),
        annotation = c("***", "NS")
      ),
      aes(
        x = m,
        xend = xend,
        y = n,
        yend = n,
        annotation = annotation
      )
    ) +
    geom_signif(
      comparisons = list(c("S1", "S2")),
      annotations = "***",
      y_position = 9.3,
      tip_length = 0
    ) +
    scale_fill_manual(values = c("grey80", "grey20"))

  pb <- ggplot_build(p)

  expect_snapshot(pb$data)
})
