

SignifAnnot <- ggplot2::ggproto("SignifAnnot", ggplot2::Stat,
                  required_aes = c("x", "y"),
                  setup_params = function(data, params) {
                    if (is.character(params$test)) params$test <- match.fun(params$test)
                    params$complete_data <- data
                    return(params)
                  },
                  compute_group = function(data, scales, comparisons, test, complete_data) {
                    # if(interactive())browser()
                    i <- -1
                    result <- lapply(comparisons, function(comp){
                      i <<- i + 1
                      # All entries in group should be the same
                      if(scales$x$map(comp[1]) == data$group[1]){
                        # if(interactive())browser()
                        group_1 <- complete_data$y[complete_data$x == scales$x$map(comp[1])]
                        group_2 <- complete_data$y[complete_data$x == scales$x$map(comp[2])]
                        test_result <- do.call(test, list(group_1, group_2))
                        print(test_result)
                        y_scale_range <- (scales$y$range$range[2] - scales$y$range$range[1])
                        y_pos <- scales$y$range$range[2] + y_scale_range * 0.05 + y_scale_range * 0.02 * i
                        data.frame(x=c(min(comp[1],comp[2]),min(comp[1],comp[2]),max(comp[1],comp[2])),
                                   xend=c(min(comp[1],comp[2]),max(comp[1],comp[2]),max(comp[1],comp[2])),
                                   y=c(y_pos - y_scale_range*0.05, y_pos, y_pos),
                                   yend=c(y_pos, y_pos, y_pos-y_scale_range*0.05),
                                   p.value=test_result$p.value)
                      }
                    })

                    do.call(rbind, result)
                  }
)

stat_signif <- function(mapping = NULL, data = NULL,
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, comparisons=NULL, test="wilcox.test",
                    ...) {
  # if(interactive()) browser()
  ggplot2::layer(
    stat = SignifAnnot, data = data, mapping = mapping, geom = "segment",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(comparisons=comparisons, test=test, na.rm = na.rm, ...)
  )
}


