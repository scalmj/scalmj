#' Scores Adjustment
#'
#' Something here
#' 
#'
#' @examples
#' \donttest{
#' some code here
#'}
#' @param data the data as a data frame
#' @param dep Update this when R package is to be dealt with
#' @param factors Update this when R package is to be dealt with
#' @param covs Update this when R package is to be dealt with
#' @param model_type .
#' @param covsTransformations Update this when R package is to be dealt with
#' @param select .
#' @param method Update this when R package is to be dealt with
#' @param direction .
#' @param forced a named vector of the form \code{c(var1="type",
#'   var2="type2")}
#' @param included .
#' @param es_zscore .
#' @param es_rank .
#' @param perc .
#' @param perc_type .
#' @param score_adjust .
#' @param score_raw .
#' @param score_preds .
#' @param score_sort .
#' @param plots .
#' @param plot_es .
#' @param .caller .
#' @param .interface .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$info} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$extrainfo} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$issues} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$varstab} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$final} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$comb_details} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$univariate} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$multiple} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$plots$adj} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plots$adj_obs} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plots$adj_pred} \tab \tab \tab \tab \tab an array \cr
#'   \code{results$plots$raw_pred} \tab \tab \tab \tab \tab an array \cr
#'   \code{results$plots$issues} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$scores$formula} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$scores$es} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$scores$percentiles} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$scores$raws} \tab \tab \tab \tab \tab a table \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$varstab$asDF}
#'
#' \code{as.data.frame(results$varstab)}
#'
#' @export
adjusting <- function(
    data,
    dep = NULL,
    factors=NULL,
    covs = NULL,
    model_type = "lm",
    covsTransformations = list(
                "lin",
                "ln",
                "log100",
                "rec"),
    select = TRUE,
    method = "step",
    direction = "high",
    forced = NULL,
    included = NULL,
    es_zscore = TRUE,
    es_rank = FALSE,
    perc = TRUE,
    perc_type = "eby5",
    score_adjust = FALSE,
    score_raw = FALSE,
    score_preds = FALSE,
    score_sort = "none",
    plots = list(),
    plot_es = FALSE,
    .caller = "adjusting",
    .interface = "R") {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("adjusting requires jmvcore to be installed (restart may be required)")

    if ( ! missing(dep)) dep <- jmvcore::resolveQuo(jmvcore::enquo(dep))
    if ( ! missing(factors)) factors <- jmvcore::resolveQuo(jmvcore::enquo(factors))
    if ( ! missing(covs)) covs <- jmvcore::resolveQuo(jmvcore::enquo(covs))
    if ( ! missing(included)) included <- jmvcore::resolveQuo(jmvcore::enquo(included))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(dep), dep, NULL),
            `if`( ! missing(factors), factors, NULL),
            `if`( ! missing(covs), covs, NULL),
            `if`( ! missing(included), included, NULL))

    for (v in factors) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- adjustingOptions$new(
        dep = dep,
        factors = factors,
        covs = covs,
        model_type = model_type,
        covsTransformations = covsTransformations,
        select = select,
        method = method,
        direction = direction,
        forced = forced,
        included = included,
        es_zscore = es_zscore,
        es_rank = es_rank,
        perc = perc,
        perc_type = perc_type,
        score_adjust = score_adjust,
        score_raw = score_raw,
        score_preds = score_preds,
        score_sort = score_sort,
        plots = plots,
        plot_es = plot_es,
        .caller = .caller,
        .interface = .interface)

    analysis <- adjustingClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

