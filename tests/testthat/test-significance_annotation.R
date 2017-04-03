

library(ggplot2)

test_that("the plotting works", {
  skip("for now...")
  print(
    ggplot(mpg, aes(x=manufacturer, y=displ)) +
      geom_boxplot()  +
      stat_signif(comparisons=list(c("audi", "ford"), c("hyundai", "nissan")),
                  map_signif_level=TRUE,
                  test="wilcox.test", test.args=list(alternative="two.sided"),
                  vjust=-0.1, margin_top=0.02, step_increase=0, tip_length=0.01) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      facet_wrap(~ as.factor(year), scale="free")
  )
})

test_that("geom works as well as stat works", {
  print(
    ggplot(mpg, aes(x=manufacturer, y=displ)) +
      geom_boxplot()  +
      geom_signif(comparisons=list(c("audi", "ford"), c("hyundai", "nissan")),
                  annotations=c("Interesting", "Too far apart"),
                  # map_signif_level=TRUE,
                  test="wilcox.test", test.args=list(alternative="two.sided"),
                  vjust=-0.1, margin_top=0.02, step_increase=0, tip_length=0.01) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      facet_wrap(~ as.factor(year), scale="free")
  )
})


test_that("non-sense fails", {
  skip("for now...")
  expect_error(print(
    ggplot(mpg, aes(y=hwy, x=displ, group=manufacturer)) +
      geom_point()  +
      stat_signif(comparisons=list(c("audi", "ford"), c("hyundai", "nissan")))
  ))
})
