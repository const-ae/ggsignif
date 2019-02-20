context("Plotting tests")


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
  expect_equal(1,1)
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
  expect_equal(1,1)
})


test_that("non-sense gives warning", {
  expect_warning(print(
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
  expect_equal(1,1)
})



test_that("you can change the linetype", {
  print(
    ggplot(iris, aes(x=Species, y=Sepal.Length)) +
      geom_boxplot() +
      geom_signif(comparisons = list(c("versicolor", "virginica")),
                  map_signif_level=TRUE, linetype=3, alpha=1, color="blue", size=1, textsize=8) +
      ylim(c(NA, 8.5))
  )
  expect_equal(1,1)
})


test_that("identical annotations are plotted separetly", {
  dat <- data.frame(Group = c("S1", "S1", "S2", "S2"),
                    Sub   = c("A", "B", "A", "B"),
                    Value = c(3,5,7,8))

  print(
    ggplot(dat, aes(Group, Value)) +
      geom_bar(aes(fill = Sub), stat="identity", position="dodge", width=.5) +
      geom_signif(comparisons=list(c("S1", "S2")), annotations="***",
                  y_position = 9.3, tip_length = 0, vjust=0.4) +
      geom_signif(stat="identity",
                  data=data.frame(x=c(0.875, 1.875), xend=c(1.125, 2.125),
                                  y=c(5.8, 8.5), annotation=c("**", "**")),
                  aes(x=x,xend=xend, y=y, yend=y, annotation=annotation, group=c(1,2))) +
      scale_fill_manual(values = c("grey80", "grey20"))
  )
  expect_equal(1,1)
})

test_that("multiple comparisons can be made to the same element", {
  set.seed(1)
  print(
    ggplot(data.frame(y=runif(100), x=sample(c("A", "B", "C", "D"), size = 100, replace = TRUE)),
           aes(x = x, y=y)) +
      geom_boxplot() +
      geom_signif(comparisons = list(c(1,2), c(2,3), c(1,3), c(1,4), c(2,4), c(1,2)), step_increase = .1)
  )
  expect_equal(1,1)
})


test_that("plots with xmin, xmax work", {
  library(datasets)

  data(airquality)
  airquality$Month <- factor(airquality$Month,
                             labels = c("May", "Jun", "Jul", "Aug", "Sep"))
  airquality_trimmed <- airquality[which(airquality$Month == "Jul" |
                                           airquality$Month == "Aug" |
                                           airquality$Month == "Sep"), ]
  airquality_trimmed$Temp.f <- factor(ifelse(airquality_trimmed$Temp > mean(airquality_trimmed$Temp), 1, 0),
                                      labels = c("Low temp", "High temp"))

  print(ggplot(airquality_trimmed, aes(x = Month, y = Ozone, fill = Temp.f)) +
          geom_boxplot(alpha=0.7, width=0.4) +
          geom_signif(y_position=150, xmin=c(0.9,1.9), xmax=c(1.1, 2.1),
                      annotations=c("***", "NS"))+
          scale_y_continuous(name = "Mean ozone in\nparts per billion",
                             breaks = seq(0, 175, 25),
                             limits=c(0, 175)) +
          scale_x_discrete(name = "Month") +
          ggtitle("Boxplot of mean ozone by month") +
          theme_bw() +
          theme(plot.title = element_text(size = 14, face = "bold"),
                text = element_text(size = 12),
                axis.title = element_text(face="bold"),
                axis.text.x=element_text(size = 11),
                legend.position = "bottom") +
          scale_fill_brewer(palette = "Accent") +
          labs(fill = "Temperature")
  )
  expect_equal(1,1)
})


test_that("manually annotated plots work", {
  ggplot(data = diamonds,
         aes(x = cut,
             y = price)) +
    geom_boxplot() +
    geom_signif(data=data.frame(color=c("E", "E", "G"), annotations=c("123","abc", "xyz"), xmin=c(1,4,2), xmax=c(2,3,3)),
                aes(annotations=annotations, xmin=xmin, xmax=xmax),
                manual=TRUE,
                y_position = 20000
    ) +
    facet_wrap(~ color) +
    ylim(NA, 22000)
  expect_equal(1,1)
})

test_that("test method which return text work", {
  magnitude_test <- function(x,y, ...){
    change <- mean(y)/mean(x)
    p <- t.test(x,y)$p.value
    stars <- if(p < 0.001)
      "***"
    else if(p < 0.01)
      "**"
    else if(p < 0.05)
      "*"
    else
      ""
    list(p.value=paste0(signif(change, digits=2),stars))
  }

  ggplot(mpg, aes(x=manufacturer, y=displ)) +
    geom_boxplot()  +
    stat_signif(comparisons=list(c("audi", "ford"), c("hyundai", "nissan")),
                test=magnitude_test,
                margin_top=0.02, step_increase=0, tip_length=0.01) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~ as.factor(year), scale="free")
  expect_equal(1,1)
})


