

library(ggplot2)

test_that("the correct columns are used", {
  print(
    ggplot(mpg, aes(x=manufacturer, y=displ)) +
      geom_boxplot()  +
      stat_signif(comparisons=list(c("audi", "ford"), c("dodge", "nissan")),
                  test=t.test) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  )
})
