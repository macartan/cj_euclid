
#' Fit Euclidean model
#'
#' @param formula A formula indicating lhs and rhs vars, e.g. Y ~ A + B + C
#' @param data A \code{data.frame}.
#' @param fixed_effects optional RHS formula
#' @param ... other arguments passed to fixest::feols
#' @return A  \code{list}
#' @import fixest
#' @import tidyverse
#' @import formula.tools
#' @export
#' @examples
#' library(tidyverse)
#' library(DeclareDesign)
#' data(covid_policy_evaluations)
#' M <- lm_euclid(rating ~ stringency + universal,
#'   data = covid_policy_evaluations,
#'   fixed_effects = "ID")
#' M
#'
#' model <- lm_euclid(rating ~ universal + stringency  + severity,
#'    data = covid_policy_evaluations,
#'    fixed_effects = "ID")$model

lm_euclid <-

  function(formula, data,  fixed_effects = NULL, vcov = "hetero", ...) {

    Xs <- labels(terms(formula))

    rhs <-
      c(Xs,
        paste0("I(", Xs, "^2)"),
        utils::combn(Xs, 2, simplify = FALSE) |>
          lapply(function(x) paste(x, collapse = ":")) |>
          unlist()) |>
      paste(collapse = " + ")

    f2 <- paste(formula.tools::lhs(formula), " ~ ", rhs)

    if(!is.null(fixed_effects)) f2 <- paste(f2, "|", fixed_effects)

    M <- fixest::feols(as.formula(f2), data, vcov = vcov, ...)

    # matrix representation
    m <- sapply(Xs, function(x)
      sapply(Xs, function(y)
        ifelse(x==y,
               M$coefficients[paste0("I(", x, "^2)")],
               M$coefficients[paste0(x, ":", y)])))

    m[is.na(m)] <- 0

    A <- as.matrix(-(m + t(m))/2) # not diag is added to itself before division; off diagonals also need to be divided by 2

    # print matrix
    # Check positive semi definite
    eigen.values <- eigen(A, only.values = TRUE)$values
    psd <- all(eigen.values >=  0)
    if(!psd) warning("The estimated A matrix is not positive semi definite")

    out <-
      list(
      model = M,
      coef= coefficients(M),
      psd = psd,
      eigen.values =  eigen.values,
      A = A)

    if(psd) out$ideals = (M$coefficients[Xs] %*% solve(A))/2

    class(out) <- "euclid"

    out
  }


#' @export
print.euclid <- function(x, ...) {
  print(summary(x))
  invisible(x)
}


#' @export
summary.euclid <- function(object, ...) {
  structure(object, class = c("summary.euclid", "data.frame"))

}

#' @export
print.summary.euclid <- function(x,  ...){

  psd <- x$psd

  cat("-----------------------------------------------------------------------\n")
  cat("\n Coefficients: \n")
  print(x$coef)

if(psd){
  cat("\n -----------------------------------------------------------------------\n")
  cat("\n The implied A matrix is positive semi definite: \n")
  print(x$A)
  cat("\n Eigenvalues: \n")
  print(x$eigen.values)
  cat("\n Ideal points: \n")
  print(x$ideals)
}

  if(!psd){
    cat("\n ----------------------------------------------------------------------\n")
    cat("\n The implied A matrix is not positive semi definite: \n")
    print(x$A)
    cat("\nA Eigenvalues: \n")
    print(x$eigen.values)
    cat("\n Ideal pointss not calculated (edge solution) \n")
  }
  cat("-----------------------------------------------------------------------\n")
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
#' model <- lm_euclid(rating ~ universal + stringency,
#'    data = covid_policy_evaluations,
#'    fixed_effects = "ID")$model
#'
#' predictions_df <-
#'   euclid_fits(
#'     formula = rating ~ universal + stringency,
#'     model = model,
#'     data = covid_policy_evaluations,
#'     fixed_effects = "ID",
#'     lengths = c(5, 5))
#'

euclid_fits <- function(
    formula,
    model,
    data,
    fixed_effects = NULL,
    mins = NULL,
    maxs = NULL,
    lengths = NULL){

  Xs <- labels(terms(formula))

  if(is.null(lengths)) lengths <- rep(50, length(Xs))
  if(length(lengths) != length(Xs)) stop("Check length of steps")
  if(is.null(mins)) mins <- data[Xs] |> apply(2, min, na.rm = TRUE)
  if(is.null(maxs)) maxs <- data[Xs] |> apply(2, max, na.rm = TRUE)

  ranges <- lapply(1:length(Xs), function(j) seq(mins[j], maxs[j], length = lengths[j]))
  names(ranges) <- Xs

  df <- do.call(tidyr::expand_grid, ranges)

  if(!is.null(fixed_effects)) {
    if(length(fixed_effects) > 1) stop("Currently set up for a single fixed effect only")
    df[[fixed_effects]] <- data[[fixed_effects]][1]
  }

  df |> dplyr::mutate(utility = predict(model, newdata = df))

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
#'
#' model <-
#'  lm_euclid(rating ~ universal + stringency,
#'    data = covid_policy_evaluations,
#'    fixed_effects = "ID")
#'
#' predictions_df <-
#'   euclid_fits(
#'     formula = rating ~ universal + stringency,
#'     model = model$model,
#'     fixed_effects = "ID",
#'     data = covid_policy_evaluations,
#'     lengths = c(50, 50))
#'
#'  euclid_plot(predictions_df, "universal", "stringency")


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
                 dplyr::group_by(!!sym(Row), !!sym(Col)) |>
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
#'
#' out <-
#'   cj_euclid(
#'    rating ~ universal + stringency,
#'    data = covid_policy_evaluations)
#' out <-
#'   cj_euclid(
#'    rating ~ universal + stringency + severity,
#'    data = covid_policy_evaluations,
#'    fixed_effects = "ID",
#'    lengths = c(20, 20, 3),
#'    Col = "severity")
#' out$graph

cj_euclid <-

  function(formula,
           data,
           X = labels(terms(formula))[[1]],
           Y = labels(terms(formula))[[2]],
           Row = NULL,
           Col = NULL,
           mins = NULL, maxs = NULL, lengths = NULL,
           fixed_effects = NULL,
           ideal_color = "blue",
           x_vals = c("L", "M","H"),
           y_vals  = c("A", "B", "C"),
           x_breaks = NULL,
           y_breaks = NULL,
           ...

           ) {

    euclid <- lm_euclid(formula, data, fixed_effects = fixed_effects, ...)

    predictions_df <- euclid_fits(formula,
                                  euclid$model,
                                  data,
                                  fixed_effects = fixed_effects,
                                  mins = mins,
                                  maxs = maxs,
                                  lengths = lengths)

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


    euclid$graph <- graph
    euclid$predictions_df <- predictions_df

    euclid

    }


