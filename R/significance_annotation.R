

StatSignif <- ggplot2::ggproto("StatSignif", ggplot2::Stat,
                  required_aes = c("x", "y", "group"),
                  setup_params = function(data, params) {
                    if(any(data$group == -1)|| any(data$group != data$x)){
                      stop("Can only handle data with groups that are plotted on the x-axis")
                    }
                    if (is.character(params$test)) params$test <- match.fun(params$test)
                    params$complete_data <- data
                    if(! is.null(params$y_position) && length(params$y_position) == 1)
                      params$y_position <- rep(params$y_position, length(params$comparisons))
                    if(length(params$margin_top) == 1) params$margin_top <- rep(params$margin_top, length(params$comparisons))
                    if(length(params$step_increase) == 1) params$step_increase <- rep(params$step_increase, length(params$comparisons))
                    if(length(params$tip_length) == 1) params$tip_length <- rep(params$tip_length, length(params$comparisons))
                    if(! is.null(params$annotations) && length(params$annotations) == 1)
                      params$annotations <- rep(params$annotations, length(params$comparisons))

                    if(params$map_signif_level == TRUE){
                      params$map_signif_level <- c("***"=0.001, "**"=0.01, "*"=0.05)
                    }else if(is.numeric(params$map_signif_level)){
                      if(is.null(names(params$map_signif_level)) ){
                        if(length(params$map_signif_level) <= 3){
                          names(params$map_signif_level) <- tail(c("***", "**", "*"), n=length(params$map_signif_level))
                        }else{
                          stop('Cannot handle un-named map for significance values, please provide in the following format: c("***"=0.001, "**"=0.01, "*"=0.05)')
                        }
                      }
                    }
                    return(params)
                  },
                  compute_group = function(data, scales, comparisons, test, test.args, complete_data,
                                           annotations, map_signif_level, y_position,
                                           margin_top, step_increase, tip_length) {
                    i <- 0
                    result <- lapply(comparisons, function(comp){
                      i <<- i + 1
                      # All entries in group should be the same
                      if(scales$x$map(comp[1]) == data$group[1]){
                        test_result <- if(is.null(annotations)){
                          group_1 <- complete_data$y[complete_data$x == scales$x$map(comp[1]) & complete_data$PANEL == data$PANEL[1]]
                          group_2 <- complete_data$y[complete_data$x == scales$x$map(comp[2]) & complete_data$PANEL == data$PANEL[1]]
                          p_value <- do.call(test, c(list(group_1, group_2), test.args))$p.value
                          if(is.numeric(map_signif_level)){
                            temp_value <- names(which.min(map_signif_level[which(map_signif_level > p_value)]))
                            if(is.null(temp_value)){
                              "NS."
                            }else{
                              temp_value
                            }
                          }else{
                            as.character(signif(p_value, digits=2))
                          }
                        }else{
                          annotations[i]
                        }
                        y_scale_range <- (scales$y$range$range[2] - scales$y$range$range[1])
                        y_pos <- if(is.null(y_position)){
                          scales$y$range$range[2] + y_scale_range * margin_top[i] + y_scale_range * step_increase[i] * (i-1)
                        }else{
                          y_pos <- y_position[i]
                        }
                        data.frame(x=c(min(comp[1],comp[2]),min(comp[1],comp[2]),max(comp[1],comp[2])),
                                   xend=c(min(comp[1],comp[2]),max(comp[1],comp[2]),max(comp[1],comp[2])),
                                   y=c(y_pos - y_scale_range*tip_length[i], y_pos, y_pos),
                                   yend=c(y_pos, y_pos, y_pos-y_scale_range*tip_length[i]),
                                   annotation=test_result)
                      }
                    })

                    do.call(rbind, result)
                  }
)


#' Create significance layer
#'
#' @param comparisons A list of length-2 vectors.
#'   The entries in the vector are either the names of 2 values on the x-axis
#'   or the 2 integers that correspond to the index of the columns of interest
#' @param test the name of the statistical test that is applied to the values of the 2 columns (e.g. t.test, wilcox.test etc.)
#' @param test.args additional arguments for the test method
#' @param annotations character vector with alternative annotations, if not null test is ignored
#' @param map_signif_level boolean value, if the p-value are directly written as annotation or asterisks are used instead.
#'   Alternatively one can provide a named numeric vector to create custom mappings from p-values to annotation:
#'   For example: c("***"=0.001, "**"=0.01, "*"=0.05)
#' @param y_position numeric vector with the precise locations of the bars
#' @param margin_top numeric vector how much higher that the maximum value that bars start as fraction of total height
#' @param step_increase numeric vector with the increase in fraction of total height for every additional comparison to
#'   minimize overlap.
#' @param tip_length numeric vector with the fraction of total height that the bar goes down to indicate the precise column
#' @examples
#' library(ggplot2)
#' ggplot(mpg, aes(class, hwy)) +
#'  geom_boxplot() +
#'  geom_signif(comparisons = list(c("compact", "pickup"), c("subcompact", "suv")))
#' @export
stat_signif <- function(mapping = NULL, data = NULL,
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, comparisons=NULL, test="wilcox.test", test.args=NULL,
                    annotations=NULL, map_signif_level=FALSE,y_position=NULL,
                    margin_top=0.05, step_increase=0, tip_length=0.03,
                    ...) {
  ggplot2::layer(
    stat = StatSignif, data = data, mapping = mapping, geom = "signif",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(comparisons=comparisons, test=test, test.args=test.args,
                  annotations=annotations, map_signif_level=map_signif_level,y_position=y_position,
                  margin_top=margin_top, step_increase=step_increase,
                  tip_length=tip_length, na.rm = na.rm, ...)
  )
}


GeomSignif <- ggplot2::ggproto("GeomSignif", ggplot2::Geom,
                           required_aes = c("x", "xend", "y", "yend", "annotation"),
                           default_aes = ggplot2::aes(shape = 19, colour = "black", size = 3.88, angle = 0, hjust = 0.5,
                                             vjust = 0, alpha = NA, family = "", fontface = 1, lineheight = 1.2),
                           draw_key = ggplot2::draw_key_point,

                           draw_group = function(data, panel_params, coord) {
                             coords <- coord$transform(data, panel_params)
                             grid::gList(
                               grid::textGrob(
                                 label=as.character(data$annotation),
                                 x=mean(c(coords$x[1], tail(coords$xend, n=1))),
                                 y=max(c(coords$y, coords$yend))+0.01,
                                 default.units = "native",
                                 hjust = data$hjust, vjust = data$vjust,
                                 rot = data$angle,
                                 gp = grid::gpar(
                                   col = alpha(data$colour, data$alpha),
                                   fontsize = data$size * .pt,
                                   fontfamily = data$family,
                                   fontface = data$fontface,
                                   lineheight = data$lineheight
                                 )
                               ),
                               grid::segmentsGrob(
                                 coords$x, coords$y,
                                 default.units = "native",
                                 coords$xend, coords$yend,
                                 gp = grid::gpar(col = coords$colour)
                               )
                             )
                           }
)

#' @rdname stat_signif
#' @export
geom_signif <- function(mapping = NULL, data = NULL, stat = "signif",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, comparisons=NULL, test="wilcox.test", test.args=NULL,
                        annotations=NULL, map_signif_level=FALSE,y_position=NULL,
                        margin_top=0.05, step_increase=0, tip_length=0.03, ...) {
  params <- list(na.rm = na.rm, ...)
  if (identical(stat, "signif")) {
    params <- c(params, list(comparisons=comparisons, test=test, test.args=test.args,
                   annotations=annotations, map_signif_level=map_signif_level,y_position=y_position,
                   margin_top=margin_top, step_increase=step_increase,
                   tip_length=tip_length))
  }
  layer(
    stat = StatSignif, geom = GeomSignif, mapping = mapping,  data = data,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = params
  )
}

