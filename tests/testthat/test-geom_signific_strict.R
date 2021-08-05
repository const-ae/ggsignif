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

  expect_equal(
    pb$data,
    list(
      structure(
        list(
          fill = c("grey80", "grey20", "grey80", "grey20"),
          x = structure(
            c(0.875, 1.125, 1.875, 2.125),
            class = c(
              "mapped_discrete",
              "numeric"
            )
          ),
          y = c(3, 5, 7, 8),
          PANEL = structure(c(
            1L, 1L, 1L,
            1L
          ), .Label = "1", class = "factor"),
          group = c(1L, 3L, 2L, 4L),
          flipped_aes = c(FALSE, FALSE, FALSE, FALSE),
          ymin = c(
            0, 0,
            0, 0
          ),
          ymax = c(3, 5, 7, 8),
          xmin = structure(c(
            0.75, 1, 1.75,
            2
          ), class = c("mapped_discrete", "numeric")),
          xmax = structure(c(
            1,
            1.25, 2, 2.25
          ), class = c("mapped_discrete", "numeric")),
          colour = c(
            NA,
            NA, NA, NA
          ),
          size = c(0.5, 0.5, 0.5, 0.5),
          linetype = c(
            1, 1,
            1, 1
          ),
          alpha = c(NA, NA, NA, NA)
        ),
        row.names = c(NA, -4L),
        class = "data.frame"
      ),
      structure(
        list(
          x = structure(c(0.875, 1.875), class = c(
            "mapped_discrete",
            "numeric"
          )),
          xend = structure(c(1.125, 2.125), class = c(
            "mapped_discrete",
            "numeric"
          )),
          y = c(5.8, 8.5),
          yend = c(5.8, 8.5),
          annotation = c(
            "***",
            "NS"
          ),
          PANEL = structure(c(1L, 1L), class = "factor", .Label = "1"),
          group = structure(1:2, n = 2L),
          shape = c(19, 19),
          colour = c(
            "black",
            "black"
          ),
          textsize = c(3.88, 3.88),
          angle = c(0, 0),
          hjust = c(0.5, 0.5),
          vjust = c(0, 0),
          alpha = c(NA, NA),
          family = c("", ""),
          fontface = c(1, 1),
          lineheight = c(
            1.2,
            1.2
          ),
          linetype = c(1, 1),
          size = c(0.5, 0.5)
        ),
        row.names = c(
          NA,
          -2L
        ),
        class = "data.frame"
      ),
      structure(
        list(
          x = structure(c(
            1L,
            1L, 2L
          ), class = c("mapped_discrete", "numeric")),
          xend = structure(c(
            1L,
            2L, 2L
          ), class = c("mapped_discrete", "numeric")),
          y = c(
            9.575,
            9.575, 9.575
          ),
          yend = c(9.575, 9.575, 9.575),
          annotation = c(
            "***",
            "***", "***"
          ),
          group = c("S1-S2-1", "S1-S2-1", "S1-S2-1"),
          flipped_aes = c(FALSE, FALSE, FALSE),
          PANEL = structure(c(1L, 1L, 1L), .Label = "1", class = "factor"),
          shape = c(19, 19, 19),
          colour = c("black", "black", "black"),
          textsize = c(3.88, 3.88, 3.88),
          angle = c(0, 0, 0),
          hjust = c(0.5, 0.5, 0.5),
          vjust = c(0, 0, 0),
          alpha = c(
            NA,
            NA, NA
          ),
          family = c("", "", ""),
          fontface = c(1, 1, 1),
          lineheight = c(1.2, 1.2, 1.2),
          linetype = c(
            1, 1,
            1
          ),
          size = c(0.5, 0.5, 0.5)
        ),
        row.names = c(NA, -3L),
        class = "data.frame"
      )
    ),
    tolerance = 0.001
  )
})
