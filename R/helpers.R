
#' Fit Euclidean model
#'
#' @param formula A formula indicating lhs and rhs vars, e.g. Y ~ A + B + C
#' @param data A \code{data.frame}.
#' @param x_vals labels for x axis
#' @param y_vals labels for y axis
#' @param ideal_color Color for idea points
#' @param ... other arguments passed to estimatr
#' @return A  \code{list}
#' @import fabricatr
#' @import tidyverse
#' @import formula.tools
#' @export
#' @examples
#' library(tidyverse)
#' library(DeclareDesign)
#' data <-
#' fabricatr::fabricate(
#'   ID = add_level(50),
#'   choice = add_level(2,
#'                      A = rnorm(N), B = rnorm(N), C = rnorm(N),
#'                      Y = -(A^2 + (B-.3)^2 + (C --.67)^2) + .1*rnorm(N)))
#' lm_euclid(Y ~ A + B, data, fixed_effects = ~ ID)


lm_euclid <-

  function(formula, data, ...) {

    Xs <- labels(terms(formula))

    rhs <-
      c(Xs,
        paste0("I(", Xs, "^2)"),
        utils::combn(Xs, 2, simplify = FALSE) |> lapply(function(x) paste(x, collapse = ":")) |>
          unlist()) |>
      paste(collapse = " + ")

    f2 <- as.formula(paste(formula.tools::lhs(formula), " ~ ", rhs))

    M <- estimatr::lm_robust(f2, data = data, ...)

    # matrix representation
    m <- sapply(Xs, function(x)
      sapply(Xs, function(y)
        ifelse(x==y,
               M$coefficients[paste0("I(", x, "^2)")],
               M$coefficients[paste0(x, ":", y)])))

    m[is.na(m)] <- 0

    A <- -(m + t(m))/2 # not diag is added to itself before division; off diagonals also need to be divided by 2

    # print matrix
    # Check positive semi definite
    message(paste("The estimated A matrix is", ifelse(all(eigen(A, only.values = TRUE)$values >=  0), "", "not"), "positive semi definite"))
    message("The A matrix:")
    print(as.matrix(A))
    M
  }





#' Get fitted values
#'
#' @param formula A formula indicating lhs and rhs vars, e.g. Y ~ A + B + C
#' @param data A \code{data.frame}.
#' @param x_vals labels for x axis
#' @param y_vals labels for y axis
#' @param ideal_color Color for idea points
#' @param ... other arguments passed to estimatr
#' @return A  \code{list}
#' @import fabricatr
#' @import tidyverse
#' @import formula.tools
#' @export
#' @examples
#' model <- lm_euclid(Y ~ A + B + C, data)
#' predictions_df <- euclid_fits(formula = Y ~ A + B + C, model, data, lengths = c(3,3, 3))

euclid_fits <- function(
    formula,
    model,
    data,
    mins = NULL,
    maxs = NULL,
    lengths = NULL){

  Xs <- labels(terms(formula))

  if(is.null(lengths)) lengths <- rep(25, length(Xs))
  if(length(lengths) != length(Xs)) stop("Check length of steps")
  if(is.null(mins)) mins <- data[Xs] |> apply(2, min, na.rm = TRUE)
  if(is.null(maxs)) maxs <- data[Xs] |> apply(2, max, na.rm = TRUE)

  ranges <- lapply(1:length(Xs), function(j) seq(mins[j], maxs[j], length = lengths[j]))
  names(ranges) <- Xs

  df <- expand_grid(do.call(expand_grid,ranges))

  df |> mutate(utility = predict(model, newdata = df))

  }


#' Plot fitted values with ideal points marked
#'
#' @param perdictions_df  Predictions
#' @param X string: variable on X axis for plot
#' @param Y string: variable on Y axis for plot
#' @param Col variable name for faceting by column
#' @param Row variable name for faceting by row
#' @param x_vals labels for x axis
#' @param y_vals labels for y axis
#' @param ideal_color color for ideal points
#' @param ... other arguments passed to estimatr
#' @return A  \code{list}
#' @import fabricatr
#' @import tidyverse
#' @import formula.tools
#' @export
#' @examples
#' model <- lm_euclid(Y ~ A + B + C, data)
#' predictions_df <- euclid_fits(formula = Y ~ A + B + C, model, data, lengths = c(3,3, 3))
#' predictions_df <- euclid_fits(data, model, formula = Y ~ A + B, lengths = c(3,3))
#'  euclid_plot(predictions_df, X = "A", Y = "B")
#'  euclid_plot(predictions_df, X = "A", Y = "B", R = "C")

euclid_plot <-
  function(predictions_df,
           X, Y,
           outcome = "utility",
           Row = NULL, Col = NULL,
           x_breaks = NULL,
           y_breaks = NULL,
           y_vals = c("L", "M","H"),
           x_vals  = c("l", "c", "r"),
           ideal_color = "blue"){

  if(is.null(x_breaks))
  x_breaks <- c(min(predictions_df[X][[1]]), median(predictions_df[X][[1]]), max(predictions_df[X][[1]]))

  if(is.null(y_breaks))
  y_breaks <- c(min(predictions_df[Y][[1]]), median(predictions_df[Y][[1]]), max(predictions_df[Y][[1]]))


  g <-

    predictions_df |>

    ggplot() +

    geom_tile(aes(x= !!sym(X), y=!!sym(Y), fill=!!sym(outcome))) +

    geom_contour(aes(x=!!sym(X),y=!!sym(Y),z=!!sym(outcome)), color="black") +

    scale_fill_gradientn(outcome, colours = rev(terrain.colors(10))) +

    coord_fixed() +
    theme(plot.title = element_text(size = 20))+
    theme(axis.text=element_text(size = 20)) +
    theme(axis.text.x = element_text(size = 20)) +
    theme(strip.text.x = element_text(size = 20))+
    theme(legend.text=element_text(size=20))+
    theme_bw()  +
    scale_x_continuous(breaks=x_breaks, labels = x_vals)  +
    scale_y_continuous(breaks=y_breaks, labels = y_vals)


  g + facet_grid(X3 ~ X4)

  if(is.null(Row) & is.null(Col))
    g <- g +
    geom_point(data = predictions_df  |>
                 dplyr::filter(utility == max(utility)),
               mapping = aes(!!sym(X), !!sym(Y)),
               color = ideal_color)


  if(!is.null(Row) & !is.null(Col))
    g <- g +
    facet_grid(as.formula(paste(Row, " ~ ", Col))) +
    geom_point(data = predictions_df  |>
                 group_by(!!sym(Row), !!sym(Col)) |>
                 dplyr::filter(utility == max(utility)),
               mapping = aes(!!sym(X), !!sym(Y)),
               color = ideal_color)


  if(!is.null(Row) &  is.null(Col))
    g <- g + facet_grid(as.formula(paste(Row, " ~ .")))  +
    geom_point(data = predictions_df  |>
                 group_by(!!sym(Row)) |>
                 dplyr::filter(utility == max(utility)),
               mapping = aes(!!sym(X), !!sym(Y)),
               color = ideal_color)


  if(is.null(Row) &  !is.null(Col))
    g <- g + facet_grid(as.formula(paste(". ~ ", Col))) +
    geom_point(data = predictions_df  |>
                 group_by(!!sym(Col)) |>
                 dplyr::filter(utility == max(utility)),
               mapping = aes(!!sym(X), !!sym(Y)),
               color = ideal_color)


  g

  }

#' Fit generalized Euclidean preferences model
#'
#' @param data A \code{data.frame}.
#' @param formula A formula
#' @param mins minimal values for fitting variables
#' @param maxs minimal values for fitting variables
#' @param lengths lengths of fitting variables
#' @param X string: variable on X axis for plot
#' @param Y string: variable on Y axis for plot
#' @param Col variable name for faceting by column
#' @param Row variable name for faceting by row
#' @param x_vals labels for x axis
#' @param y_vals labels for y axis
#' @param ideal_color color for ideal points
#' @param ... other arguments passed to estimatr
#' @return A  \code{list}
#' @import fabricatr
#' @import tidyverse
#' @export
#' @examples
#' library(tidyverse)
#' library(DeclareDesign)
#' data <-
#' fabricatr::fabricate(
#'   ID = add_level(20),
#'   choice = add_level(2,
#'                      A = rnorm(N), B = rnorm(N), C = rnorm(N),
#'                      Y = -(A^2 + (B-.3)^2 + (C --.67)^2) + .2*rnorm(N)))
#' out <- cjEuclid(Y ~ A + B + C, data, lengths = c(20, 20, 3))
#' out$graph


cjEuclid <-

  function(formula,
           data,
           X = labels(terms(formula))[[1]],
           Y = labels(terms(formula))[[2]],
           Row = NULL,
           Col = NULL,
           mins = NULL, maxs = NULL, lengths = NULL,
           ideal_color = "blue",
           x_vals = c("L", "M","H"),
           y_vals  = c("A", "B", "C"),
           x_breaks = NULL,
           y_breaks = NULL,
           ...

           ) {

    model <- lm_euclid(formula, data)

    predictions_df <- euclid_fits(formula, model, data,  mins = mins, maxs = maxs, lengths = lengths)

    graph <-
      predictions_df |>
      euclid_plot(
        ideal_color = ideal_color,
        X = X,
        Y = Y,
        R = Row,
        C = Col,
        x_vals = x_vals,
        y_vals  = y_vals,
        x_breaks = x_breaks,
        y_breaks = y_breaks)


    list(model = model, predictions_df = predictions_df, graph = graph)

    }


