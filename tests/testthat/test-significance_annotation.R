

library(ggplot2)

test_that("the plotting works", {
  print(
    ggplot(mpg, aes(x=manufacturer, y=displ)) +
      geom_boxplot()  +
      stat_signif(comparisons=list(c("audi", "ford"), c("hyundai", "nissan")),
                  map_signif_level=TRUE,
                  test="wilcox.test", test.args=list(alternative="two.sided"),
                  margin_top=0.02, step_increase=0, tip_length=0.01) +
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
                  margin_top=0.02, step_increase=0, tip_length=0.01) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      facet_wrap(~ as.factor(year), scale="free")
  )
})


test_that("non-sense fails", {
  expect_error(print(
    ggplot(mpg, aes(y=hwy, x=displ, group=manufacturer)) +
      geom_point()  +
      stat_signif(comparisons=list(c("audi", "ford"), c("hyundai", "nissan")))
  ))
})


test_that("geom_signif with identity works", {
  dat <- data.frame(Group = c("S1", "S1", "S2", "S2"),
                    Sub   = c("A", "B", "A", "B"),
                    Value = c(3,5,7,8))

  ## Define base plot
  print(
    ggplot(dat, aes(Group, Value)) +
      geom_bar(aes(fill = Sub), stat="identity", position="dodge", width=.5) +
      geom_signif(stat="identity",
                  data=data.frame(x=c(0.875, 1.875), xend=c(1.125, 2.125), y=c(5.8, 8.5), annotation=c("***", "NS")),
                  aes(x=x,xend=xend, y=y, yend=y, annotation=annotation)) +
      geom_signif(comparisons=list(c("S1", "S2")), annotations="***", y_position = 9.3, tip_length = 0) +
      scale_fill_manual(values = c("grey80", "grey20"))
  )
})

test_that("loading the psych packages does not lead to an error", {
  library(ggplot2)
  library(ggsignif)

  library(psych)

  print(
    ggplot(iris, aes(x=Species, y=Sepal.Length)) +
      geom_boxplot() +
      geom_signif(comparisons = list(c("versicolor", "virginica")),
                  map_signif_level=TRUE)
  )
})



test_that("you can change the linetype", {
  print(
    ggplot(iris, aes(x=Species, y=Sepal.Length)) +
      geom_boxplot() +
      geom_signif(comparisons = list(c("versicolor", "virginica")),
                  map_signif_level=TRUE, linetype=3, alpha=1, color="blue", size=1, textsize=8)
  )
})








