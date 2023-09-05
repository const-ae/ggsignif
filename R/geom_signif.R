#' Create significance layer
#'
#' @param comparisons A list of length-2 vectors. The entries in the vector are
#'   either the names of 2 values on the x-axis or the 2 integers that
#'   correspond to the index of the columns of interest.
#' @param test the name of the statistical test that is applied to the values of
#'   the 2 columns (e.g. `t.test`, `wilcox.test` etc.). If you implement a
#'   custom test make sure that it returns a list that has an entry called
#'   `p.value`.
#' @param test.args additional arguments for the test method
#' @param annotations character vector with alternative annotations, if not null
#'   test is ignored
#' @param map_signif_level Boolean value, if the p-value are directly written as
#'   annotation or asterisks are used instead. Alternatively one can provide a
#'   named numeric vector to create custom mappings from p-values to annotation:
#'   For example: `c("***"=0.001, "**"=0.01, "*"=0.05)`.
#'   Alternatively, one can provide a function that takes a numeric argument
#'   (the p-value) and returns a string.
#' @param xmin,xmax numeric vector with the positions of the left and right
#'   sides of the brackets, respectively
#' @param y_position numeric vector with the y positions of the brackets
#' @param size change the width of the lines of the bracket
#' @param textsize change the size of the text
#' @param family change the font used for the text
#' @param vjust move the text up or down relative to the bracket
#' @param margin_top numeric vector how much higher that the maximum value that
#'   bars start as fraction of total height
#' @param step_increase numeric vector with the increase in fraction of total
#'   height for every additional comparison to minimize overlap.
#' @param extend_line Numeric that allows to shorten (negative values) or extend
#'   (positive value) the horizontal line between groups for each comparison;
#'   defaults to 0.
#' @param tip_length numeric vector with the fraction of total height that the
#'   bar goes down to indicate the precise column
#' @param parse If `TRUE`, the labels will be parsed into expressions and
#'   displayed as described in `?plotmath`.
#' @param manual Boolean flag that indicates that the parameters are provided
#'   with a data.frame. This option is necessary if one wants to plot different
#'   annotations per facet.
#' @param na.rm If `FALSE` (the default), removes missing values with
#'    a warning.  If `TRUE` silently removes missing values.
#' @param correction Should the p value be corrected for multiple testing by false discovery rate ? 
#' "none" by default. p.adjust.methods: c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
#' @param orientation The orientation of the layer. The default (‘NA’)
#' automatically determines the orientation from the aesthetic mapping.
#' In the rare event that this fails it can be given explicitly by setting
#' 'orientation' to either "x" or "y"
#' @param ... other arguments passed on to `layer`. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `color = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#'
#' @inheritParams ggplot2::layer
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(ggsignif)
#'
#' ggplot(mpg, aes(class, hwy)) +
#'   geom_boxplot() +
#'   geom_signif(comparisons = list(
#'     c("compact", "pickup"),
#'     c("subcompact", "suv")
#'   ))
#'
#' ggplot(mpg, aes(class, hwy)) +
#'   geom_boxplot() +
#'   geom_signif(
#'     comparisons = list(
#'       c("compact", "pickup"),
#'       c("subcompact", "suv")
#'     ),
#'     map_signif_level = function(p) sprintf("p = %.2g", p)
#'   )
#'
#' ggplot(mpg, aes(class, hwy)) +
#'   geom_boxplot() +
#'   geom_signif(
#'     annotations = c("First", "Second"),
#'     y_position = c(30, 40), xmin = c(4, 1), xmax = c(5, 3)
#'   )
#' }
#'
#' @export
stat_signif <- function(mapping = NULL,
                        data = NULL,
                        position = "identity",
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE,
                        comparisons = NULL,
                        test = "wilcox.test",
                        test.args = NULL,
                        annotations = NULL,
                        map_signif_level = FALSE,
                        y_position = NULL,
                        xmin = NULL,
                        xmax = NULL,
                        margin_top = 0.05,
                        step_increase = 0,
                        tip_length = 0.03,
                        size = 0.5,
                        textsize = 3.88,
                        family = "",
                        vjust = 0,
                        parse = FALSE,
                        manual = FALSE,
                        correction="none",
                        orientation = NA,
                        ...) {
  if (manual) {
    if (!is.null(data) & !is.null(mapping)) {
      if (!"x" %in% names(data)) mapping$x <- 1
      if (!"y" %in% names(data)) mapping$y <- 1
    } else {
      stop("If manual mode is selected you need to provide the data and mapping parameters")
    }
  }

  ggplot2::layer(
    stat = StatSignif, data = data, mapping = mapping, geom = "signif",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      comparisons = comparisons,
      test = test,
      test.args = test.args,
      annotations = annotations,
      map_signif_level = map_signif_level,
      y_position = y_position,
      xmin = xmin,
      xmax = xmax,
      margin_top = margin_top,
      step_increase = step_increase,
      tip_length = tip_length,
      size = size,
      textsize = textsize,
      family = family,
      vjust = vjust,
      parse = parse,
      manual = manual,
      na.rm = na.rm,
      correction=correction,
      orientation = orientation,
      ...
    )
  )
}


GeomSignif <- ggplot2::ggproto(
  "GeomSignif",
  ggplot2::Geom,
  required_aes = c("x", "xend", "y", "yend", "annotation"),
  default_aes = ggplot2::aes(
    shape = 19,
    colour = "black",
    textsize = 3.88,
    angle = 0,
    hjust = 0.5,
    vjust = 0,
    alpha = NA,
    family = "",
    fontface = 1,
    lineheight = 1.2,
    linetype = 1,
    size = 0.5
  ),
  extra_params = c("na.rm", "orientation"),
  setup_params = function(data, params) {
    params$flipped_aes <- ggplot2::has_flipped_aes(data, params)
    return(params)
  },
  draw_key = function(...) {
    grid::nullGrob()
  },
  draw_group = function(data,
                        panel_params,
                        coord,
                        parse = FALSE,
                        extend_line = 0,
                        flipped_aes = FALSE) {
    lab <- as.character(data$annotation)
    if (parse) {
      lab <- parse_safe(as.character(lab))
    }

    coords <- coord$transform(data, panel_params)
    if (extend_line != 0 && nrow(coords) == 3) {
      if (coords[2, "x"] > coords[2, "xend"]) {
        extend_line <- -extend_line
      }

      # left vertical segment
      coords[1, "x"] <- coords[1, "x"] - extend_line
      coords[1, "xend"] <- coords[1, "xend"] - extend_line

      # horizontal line
      coords[2, "x"] <- coords[2, "x"] - extend_line
      coords[2, "xend"] <- coords[2, "xend"] + extend_line

      # right vertical segment
      coords[3, "x"] <- coords[3, "x"] + extend_line
      coords[3, "xend"] <- coords[3, "xend"] + extend_line
    }

    clp_flag <- inherits(coord, "CoordFlip")

    if (!any(flipped_aes, clp_flag) || all(flipped_aes, clp_flag)) {
      text_x <- mean(c(coords$x[1], tail(coords$xend, n = 1)))
      text_y <- max(c(coords$y, coords$yend)) + 0.01
    } else {
      text_x <- max(c(coords$x, coords$xend)) + 0.01
      text_y <- mean(c(coords$y[1], tail(coords$yend, n = 1)))

      if (all(coords$angle == 0)) {
        coords$angle <- 270
      }
    }

    grid::gList(
      grid::textGrob(
        label = lab,
        x = text_x, # mean(c(coords$x[1], tail(coords$xend, n = 1))),
        y = text_y, # max(c(coords$y, coords$yend)) + 0.01,
        default.units = "native",
        hjust = coords$hjust, vjust = coords$vjust,
        rot = coords$angle,
        gp = grid::gpar(
          col = scales::alpha(coords$colour, coords$alpha),
          fontsize = coords$textsize * ggplot2::.pt,
          fontfamily = coords$family,
          fontface = coords$fontface,
          lineheight = coords$lineheight
        )
      ),
      grid::segmentsGrob(
        coords$x, coords$y,
        default.units = "native",
        coords$xend, coords$yend,
        gp = grid::gpar(
          col = scales::alpha(coords$colour, coords$alpha),
          lty = coords$linetype,
          lwd = coords$size * ggplot2::.pt
        )
      )
    )
  }
)

#' @rdname stat_signif
#' @export
geom_signif <- function(mapping = NULL,
                        data = NULL,
                        stat = "signif",
                        position = "identity",
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE,
                        comparisons = NULL,
                        test = "wilcox.test",
                        test.args = NULL,
                        annotations = NULL,
                        map_signif_level = FALSE,
                        y_position = NULL,
                        xmin = NULL,
                        xmax = NULL,
                        margin_top = 0.05,
                        step_increase = 0,
                        extend_line = 0,
                        tip_length = 0.03,
                        size = 0.5,
                        textsize = 3.88,
                        family = "",
                        vjust = 0,
                        parse = FALSE,
                        manual = FALSE,
                        correction="none",
                        orientation = NA,
                        ...) {
  params <- list(na.rm = na.rm, ...)

  if (identical(stat, "signif")) {
    if (!is.null(data) & !is.null(mapping) & !manual) {
      warning("You have set data and mapping, are you sure that manual = FALSE is correct?")
    }

    if (manual) {
      if (is.null(mapping$annotations)) {
        stop("Manual mode only works if with 'annotations' is provided in mapping")
      }

      if (!is.null(data) & !is.null(mapping)) {
        if (!"x" %in% names(mapping)) {
          if ("xmin" %in% names(mapping)) {
            mapping$x <- mapping$xmin
          } else {
            mapping$x <- xmin
          }
        }

        if (!"y" %in% names(mapping)) {
          if ("y_position" %in% names(mapping)) {
            mapping$y <- mapping$y_position
          } else {
            mapping$y <- y_position
          }
        }
      } else {
        stop("If manual mode is selected you need to provide the data and mapping parameters")
      }
    }

    params <- c(
      params,
      list(
        comparisons = comparisons,
        test = test,
        test.args = test.args,
        annotations = annotations,
        map_signif_level = map_signif_level,
        y_position = y_position,
        xmin = xmin,
        xmax = xmax,
        margin_top = margin_top,
        step_increase = step_increase,
        extend_line = extend_line,
        tip_length = tip_length,
        size = size,
        textsize = textsize,
        family = family,
        vjust = vjust,
        parse = parse,
        manual = manual,
        correction=correction,
        orientation = orientation
      )
    )
  }

  ggplot2::layer(
    stat = stat,
    geom = GeomSignif,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}


StatSignif <- ggplot2::ggproto(
  "StatSignif",
  ggplot2::Stat,
  required_aes = c("x", "y", "group"),
  extra_params = c("na.rm", "orientation"),
  setup_params = function(data, params) {
    # if(any(data$group == -1)|| any(data$group != data$x)){
    params$flipped_aes <- ggplot2::has_flipped_aes(data, params)
    data <- ggplot2::flip_data(data, params$flipped_aes)

    if (any(data$group == -1)) {
      stop("Can only handle data with groups that are plotted on the x-axis")
    }

    if (is.character(params$test)) params$test <- match.fun(params$test)
    params$complete_data <- data

    if (is.null(params$xmin) != is.null(params$xmax) || length(params$xmin) != length(params$xmax)) {
      stop("If xmin or xmax is set, the other one also needs to be set and they need to contain the same number of values")
    }

    if (!is.null(params$xmin) && !is.null(params$comparisons)) {
      stop("Set either the xmin, xmax values or the comparisons")
    }

    if (!is.null(params$xmin) && is.null(params$y_position)) {
      stop("If xmin, xmax are defined also define y_position")
    }

    if (!is.null(params$y_position) && length(params$y_position) == 1) {
      params$y_position <- rep(params$y_position, max(length(params$comparisons), length(params$xmin), 1))
    }

    if (length(params$margin_top) == 1) params$margin_top <- rep(params$margin_top, max(length(params$comparisons), length(params$xmin), 1))

    if (length(params$step_increase) == 1) params$step_increase <- rep(params$step_increase, max(length(params$comparisons), length(params$xmin), 1))

    if (length(params$tip_length) == 1) params$tip_length <- rep(params$tip_length, max(length(params$comparisons), length(params$xmin), 1) * 2)

    if (length(params$tip_length) == length(params$comparisons)) params$tip_length <- rep(params$tip_length, each = 2)

    if (length(params$tip_length) == length(params$xmin)) params$tip_length <- rep(params$tip_length, each = 2)

    if (!is.null(params$annotations) && length(params$annotations) == 1) {
      params$annotations <- rep(params$annotations, max(length(params$comparisons), length(params$xmin), 1))
    }

    if (!is.null(params$annotations) && length(params$annotations) != max(length(params$comparisons), length(params$xmin), 1)) {
      stop(paste0(
        "annotations contains a different number of elements (", length(params$annotations),
        ") than comparisons or xmin (", max(length(params$comparisons), length(params$xmin), 1), ")."
      ))
    }

    if (all(is.logical(params$map_signif_level)) && all(params$map_signif_level == TRUE)) {
      params$map_signif_level <- c("***" = 0.001, "**" = 0.01, "*" = 0.05)
    } else if (is.numeric(params$map_signif_level)) {
      if (is.null(names(params$map_signif_level))) {
        if (length(params$map_signif_level) <= 3) {
          names(params$map_signif_level) <- tail(c("***", "**", "*"), n = length(params$map_signif_level))
        } else {
          stop('Cannot handle un-named map for significance values, please provide in the following format: c("***"=0.001, "**"=0.01, "*"=0.05)')
        }
      }
    }
    return(params)
  },
  compute_group = function(data,
                           scales,
                           comparisons,
                           test,
                           test.args,
                           complete_data,
                           annotations,
                           map_signif_level,
                           y_position,
                           xmax,
                           xmin,
                           margin_top,
                           step_increase,
                           tip_length,
                           manual,
                           correction,
                           flipped_aes = FALSE) {
    data <- ggplot2::flip_data(data, flipped_aes)
    scales <- ggplot2::flip_data(scales, flipped_aes)

    if ("annotations" %in% colnames(data)) {
      annotations <- data[["annotations"]]
    }

    if ("y_position" %in% colnames(data)) {
      y_position <- data[["y_position"]]
    }

    if ("xmax" %in% colnames(data)) {
      xmax <- data[["xmax"]]
    }

    if ("xmin" %in% colnames(data)) {
      xmin <- data[["xmin"]]
    }

    if ("map_signif_level" %in% colnames(data)) {
      map_signif_level <- data[["map_signif_level"]]
    }

    if ("tip_length" %in% colnames(data)) {
      tip_length <- rep(data[["tip_length"]], each = 2)
    }

    if (!is.null(comparisons)) {
      i <- 0
      result <- lapply(comparisons, function(comp) {
        i <<- i + 1
        # All entries in group should be the same
        if (scales$x$map(comp[1]) == data$group[1] | manual) {
          test_result <- if (is.null(annotations)) {
            group_1 <- complete_data$y[complete_data$x == scales$x$map(comp[1]) &
              complete_data$PANEL == data$PANEL[1]]
            group_2 <- complete_data$y[complete_data$x == scales$x$map(comp[2]) &
              complete_data$PANEL == data$PANEL[1]]
            p_value <- do.call(test, c(list(group_1, group_2), test.args))$p.value
            p_value = p.adjust(p_value, method=correction, n=length(comparisons))
            if (is.numeric(map_signif_level)) {
              temp_value <- names(which.min(map_signif_level[which(map_signif_level > p_value)]))
              if (is.null(temp_value)) {
                "NS."
              } else {
                temp_value
              }
            } else if (is.function(map_signif_level)) {
              map_signif_level(p_value)
            } else {
              if (is.numeric(p_value)) {
                if (p_value < .Machine$double.eps) {
                  sprintf("p < %.2e", .Machine$double.eps)
                } else {
                  as.character(sprintf("%.2g", p_value))
                }
              } else {
                as.character(p_value)
              }
            }
          } else {
            annotations[i]
          }
          y_scale_range <- (scales$y$range$range[2] - scales$y$range$range[1])
          if (is.null(y_position)) {
            y_pos <- scales$y$range$range[2] + y_scale_range * margin_top[i] + y_scale_range * step_increase[i] * (i - 1)
          } else {
            y_pos <- y_position[i] + y_scale_range * margin_top[i] + y_scale_range * step_increase[i] * (i - 1)
          }

          data.frame(
            x = c(min(comp[1], comp[2]), min(comp[1], comp[2]), max(comp[1], comp[2])),
            xend = c(min(comp[1], comp[2]), max(comp[1], comp[2]), max(comp[1], comp[2])),
            y = c(y_pos - y_scale_range * tip_length[(i - 1) * 2 + 1], y_pos, y_pos),
            yend = c(y_pos, y_pos, y_pos - y_scale_range * tip_length[(i - 1) * 2 + 2]),
            annotation = test_result,
            group = paste(c(comp, i), collapse = "-")
          )
        }
      })

      df <- do.call(rbind, result)
    } else {
      if ((data$x[1] == min(complete_data$x) & data$group[1] == min(complete_data$group)) | manual) {
        y_scale_range <- (scales$y$range$range[2] - scales$y$range$range[1])

        if (is.character(xmin)) {
          xmin <- scales$x$map(xmin)
        }

        if (is.character(xmax)) {
          xmax <- scales$x$map(xmax)
        }

        if ("expression" %in% class(annotations)) {
          stop("annotations must be a character vector. To use plotmath set parse=TRUE.")
        }

        df <- data.frame(
          x = c(xmin, xmin, xmax),
          xend = c(xmin, xmax, xmax),
          y = c(
            y_position - y_scale_range * tip_length[seq_len(length(tip_length)) %% 2 == 1],
            y_position,
            y_position
          ),
          yend = c(
            y_position,
            y_position,
            y_position - y_scale_range * tip_length[seq_len(length(tip_length)) %% 2 == 0]
          ),
          annotation = rep(annotations, times = 3), group = if (manual) {
            rep(data$group, times = 3)
          } else {
            rep(seq_along(xmin), times = 3)
          }
        )
      } else {
        df <- NULL
      }
    }

    if (!is.null(df)) {
      df$flipped_aes <- flipped_aes
      df <- ggplot2::flip_data(df, flipped_aes)
    }

    return(df)
  }
)
