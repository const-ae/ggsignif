# StatLm <- ggproto("StatLm", Stat,
#                   required_aes = c("x", "y"),
#
#                   compute_group = function(data, scales, params, n = 100, formula = y ~ x) {
#                     rng <- range(data$x, na.rm = TRUE)
#                     grid <- data.frame(x = seq(rng[1], rng[2], length = n))
#
#                     mod <- lm(formula, data = data)
#                     grid$y <- predict(mod, newdata = grid)
#
#                     grid
#                   }
# )
#
# stat_lm <- function(mapping = NULL, data = NULL, geom = "line",
#                     position = "identity", na.rm = FALSE, show.legend = NA,
#                     inherit.aes = TRUE, n = 50, formula = y ~ x,
#                     ...) {
#   layer(
#     stat = StatLm, data = data, mapping = mapping, geom = geom,
#     position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#     params = list(n = n, formula = formula, na.rm = na.rm, ...)
#   )
# }
#
# ggplot(mpg, aes(displ, hwy)) +
#   geom_point() +
#   stat_lm(formula = y ~ poly(x, 10), n=3) +
#   stat_lm(formula = y ~ poly(x, 10), geom = "point", colour = "red", n = 20)
