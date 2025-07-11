
# This file is automatically generated, you probably don't want to edit this

adjustingOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "adjustingOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            dep = NULL,
            factors = NULL,
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
            .interface = "jamovi", ...) {

            super$initialize(
                package="scalmj",
                name="adjusting",
                requiresData=TRUE,
                ...)

            private$..dep <- jmvcore::OptionVariable$new(
                "dep",
                dep,
                default=NULL,
                suggested=list(
                    "continuous",
                    "ordinal"),
                permitted=list(
                    "numeric"))
            private$..factors <- jmvcore::OptionVariables$new(
                "factors",
                factors,
                suggested=list(
                    "nominal"),
                permitted=list(
                    "factor"))
            private$..covs <- jmvcore::OptionVariables$new(
                "covs",
                covs,
                default=NULL,
                suggested=list(
                    "continuous",
                    "ordinal"),
                permitted=list(
                    "numeric"))
            private$..model_type <- jmvcore::OptionList$new(
                "model_type",
                model_type,
                default="lm",
                options=list(
                    "lm",
                    "nb",
                    "pois",
                    "beta"))
            private$..covsTransformations <- jmvcore::OptionNMXList$new(
                "covsTransformations",
                covsTransformations,
                options=list(
                    "lin",
                    "ln",
                    "log10",
                    "log100",
                    "rec",
                    "sqrt",
                    "quad",
                    "cub"),
                default=list(
                    "lin",
                    "ln",
                    "log100",
                    "rec"))
            private$..select <- jmvcore::OptionBool$new(
                "select",
                select,
                default=TRUE)
            private$..method <- jmvcore::OptionList$new(
                "method",
                method,
                options=list(
                    "sig",
                    "step",
                    "comb"),
                default="step")
            private$..direction <- jmvcore::OptionList$new(
                "direction",
                direction,
                default="high",
                options=list(
                    "low",
                    "high"))
            private$..forced <- jmvcore::OptionArray$new(
                "forced",
                forced,
                items="(covs)",
                default=NULL,
                template=jmvcore::OptionGroup$new(
                    "forced",
                    NULL,
                    elements=list(
                        jmvcore::OptionVariable$new(
                            "var",
                            NULL,
                            content="$key"),
                        jmvcore::OptionList$new(
                            "type",
                            NULL,
                            options=list(
                                "auto",
                                "lin",
                                "ln",
                                "log10",
                                "log100",
                                "sqrt",
                                "rec",
                                "quad",
                                "cub"),
                            default="auto"))))
            private$..included <- jmvcore::OptionVariables$new(
                "included",
                included,
                default=NULL)
            private$..es_zscore <- jmvcore::OptionBool$new(
                "es_zscore",
                es_zscore,
                default=TRUE)
            private$..es_rank <- jmvcore::OptionBool$new(
                "es_rank",
                es_rank,
                default=FALSE)
            private$..perc <- jmvcore::OptionBool$new(
                "perc",
                perc,
                default=TRUE)
            private$..perc_type <- jmvcore::OptionList$new(
                "perc_type",
                perc_type,
                default="eby5",
                options=list(
                    "eby5",
                    "eby10",
                    "by10",
                    "by5",
                    "all"))
            private$..score_adjust <- jmvcore::OptionBool$new(
                "score_adjust",
                score_adjust,
                default=FALSE)
            private$..score_raw <- jmvcore::OptionBool$new(
                "score_raw",
                score_raw,
                default=FALSE)
            private$..score_preds <- jmvcore::OptionBool$new(
                "score_preds",
                score_preds,
                default=FALSE)
            private$..score_sort <- jmvcore::OptionList$new(
                "score_sort",
                score_sort,
                default="none",
                options=list(
                    "none",
                    "inc",
                    "dec"))
            private$..plots <- jmvcore::OptionNMXList$new(
                "plots",
                plots,
                default=list(),
                options=list(
                    "adj",
                    "adj_obs",
                    "adj_pred"))
            private$..plot_es <- jmvcore::OptionBool$new(
                "plot_es",
                plot_es,
                default=FALSE)
            private$...caller <- jmvcore::OptionString$new(
                ".caller",
                .caller,
                default="adjusting",
                hidden=TRUE)
            private$...interface <- jmvcore::OptionString$new(
                ".interface",
                .interface,
                default="jamovi",
                hidden=TRUE)

            self$.addOption(private$..dep)
            self$.addOption(private$..factors)
            self$.addOption(private$..covs)
            self$.addOption(private$..model_type)
            self$.addOption(private$..covsTransformations)
            self$.addOption(private$..select)
            self$.addOption(private$..method)
            self$.addOption(private$..direction)
            self$.addOption(private$..forced)
            self$.addOption(private$..included)
            self$.addOption(private$..es_zscore)
            self$.addOption(private$..es_rank)
            self$.addOption(private$..perc)
            self$.addOption(private$..perc_type)
            self$.addOption(private$..score_adjust)
            self$.addOption(private$..score_raw)
            self$.addOption(private$..score_preds)
            self$.addOption(private$..score_sort)
            self$.addOption(private$..plots)
            self$.addOption(private$..plot_es)
            self$.addOption(private$...caller)
            self$.addOption(private$...interface)
        }),
    active = list(
        dep = function() private$..dep$value,
        factors = function() private$..factors$value,
        covs = function() private$..covs$value,
        model_type = function() private$..model_type$value,
        covsTransformations = function() private$..covsTransformations$value,
        select = function() private$..select$value,
        method = function() private$..method$value,
        direction = function() private$..direction$value,
        forced = function() private$..forced$value,
        included = function() private$..included$value,
        es_zscore = function() private$..es_zscore$value,
        es_rank = function() private$..es_rank$value,
        perc = function() private$..perc$value,
        perc_type = function() private$..perc_type$value,
        score_adjust = function() private$..score_adjust$value,
        score_raw = function() private$..score_raw$value,
        score_preds = function() private$..score_preds$value,
        score_sort = function() private$..score_sort$value,
        plots = function() private$..plots$value,
        plot_es = function() private$..plot_es$value,
        .caller = function() private$...caller$value,
        .interface = function() private$...interface$value),
    private = list(
        ..dep = NA,
        ..factors = NA,
        ..covs = NA,
        ..model_type = NA,
        ..covsTransformations = NA,
        ..select = NA,
        ..method = NA,
        ..direction = NA,
        ..forced = NA,
        ..included = NA,
        ..es_zscore = NA,
        ..es_rank = NA,
        ..perc = NA,
        ..perc_type = NA,
        ..score_adjust = NA,
        ..score_raw = NA,
        ..score_preds = NA,
        ..score_sort = NA,
        ..plots = NA,
        ..plot_es = NA,
        ...caller = NA,
        ...interface = NA)
)

adjustingResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "adjustingResults",
    inherit = jmvcore::Group,
    active = list(
        info = function() private$.items[["info"]],
        extrainfo = function() private$.items[["extrainfo"]],
        issues = function() private$.items[["issues"]],
        varstab = function() private$.items[["varstab"]],
        final = function() private$.items[["final"]],
        comb_details = function() private$.items[["comb_details"]],
        univariate = function() private$.items[["univariate"]],
        multiple = function() private$.items[["multiple"]],
        plots = function() private$.items[["plots"]],
        scores = function() private$.items[["scores"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Scores Adjustment")
            self$add(jmvcore::Html$new(
                options=options,
                name="info",
                title="Introduction"))
            self$add(jmvcore::Html$new(
                options=options,
                name="extrainfo",
                title="Extra Info",
                visible=FALSE))
            self$add(jmvcore::Html$new(
                options=options,
                name="issues",
                title="Issues",
                visible=FALSE))
            self$add(jmvcore::Table$new(
                options=options,
                name="varstab",
                title="Variables",
                columns=list(
                    list(
                        `name`="name", 
                        `type`="text", 
                        `title`="Name"),
                    list(
                        `name`="type", 
                        `type`="text", 
                        `title`="Type"),
                    list(
                        `name`="transf", 
                        `type`="text", 
                        `title`="Transform"),
                    list(
                        `name`="method", 
                        `type`="text", 
                        `title`="Inclusion"),
                    list(
                        `name`="selected", 
                        `type`="text", 
                        `title`="Selected"),
                    list(
                        `name`="selected_trans", 
                        `type`="text", 
                        `title`="Selected Transf."))))
            self$add(jmvcore::Table$new(
                options=options,
                name="final",
                title="Final model",
                refs=list(
                    "arcara"),
                columns=list(
                    list(
                        `name`="name", 
                        `type`="text", 
                        `title`="Predictors"),
                    list(
                        `name`="fun", 
                        `type`="text", 
                        `title`="Transf."),
                    list(
                        `name`="estimate", 
                        `type`="number", 
                        `title`="Coef"),
                    list(
                        `name`="se", 
                        `type`="number", 
                        `title`="SE"),
                    list(
                        `name`="test", 
                        `type`="number", 
                        `title`="z"),
                    list(
                        `name`="df", 
                        `type`="integer", 
                        `title`="df_res"),
                    list(
                        `name`="p", 
                        `title`="p", 
                        `type`="number", 
                        `format`="zto,pvalue"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="comb_details",
                title="Transformations combinations details",
                visible="(method:comb)",
                refs=list(
                    "arcara"),
                columns=list(
                    list(
                        `name`="name", 
                        `type`="text", 
                        `title`="Model"),
                    list(
                        `name`="aic", 
                        `type`="number", 
                        `title`="AIC"),
                    list(
                        `name`="r2", 
                        `type`="number", 
                        `title`="R\u00B2"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="univariate",
                title="Univariate effects",
                visible="(method:step || method:sig)",
                columns=list(
                    list(
                        `name`="name", 
                        `type`="text", 
                        `title`="Predictor"),
                    list(
                        `name`="fun", 
                        `type`="text", 
                        `title`="Transf."),
                    list(
                        `name`="estimate", 
                        `type`="number", 
                        `title`="Coef"),
                    list(
                        `name`="r2", 
                        `type`="number", 
                        `title`="R\u00B2"),
                    list(
                        `name`="aic", 
                        `type`="number", 
                        `title`="AIC"),
                    list(
                        `name`="test", 
                        `type`="number", 
                        `title`="z"),
                    list(
                        `name`="df", 
                        `type`="integer", 
                        `title`="df"),
                    list(
                        `name`="p", 
                        `title`="p", 
                        `type`="number", 
                        `format`="zto,pvalue"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="multiple",
                title="Multiple model",
                columns=list(
                    list(
                        `name`="name", 
                        `type`="text", 
                        `title`="Predictors"),
                    list(
                        `name`="fun", 
                        `type`="text", 
                        `title`="Transf."),
                    list(
                        `name`="estimate", 
                        `type`="number", 
                        `title`="Coef"),
                    list(
                        `name`="se", 
                        `type`="number", 
                        `title`="SE"),
                    list(
                        `name`="test", 
                        `type`="number", 
                        `title`="z"),
                    list(
                        `name`="df", 
                        `type`="integer", 
                        `title`="df"),
                    list(
                        `name`="p", 
                        `title`="p", 
                        `type`="number", 
                        `format`="zto,pvalue"))))
            self$add(R6::R6Class(
                inherit = jmvcore::Group,
                active = list(
                    adj = function() private$.items[["adj"]],
                    adj_obs = function() private$.items[["adj_obs"]],
                    adj_pred = function() private$.items[["adj_pred"]],
                    issues = function() private$.items[["issues"]]),
                private = list(),
                public=list(
                    initialize=function(options) {
                        super$initialize(
                            options=options,
                            name="plots",
                            title="Plots")
                        self$add(jmvcore::Image$new(
                            options=options,
                            name="adj",
                            title="Adj. Scores histogram",
                            renderFun=".plot_adj",
                            width=700,
                            height=400,
                            visible="(plots:adj)"))
                        self$add(jmvcore::Image$new(
                            options=options,
                            name="adj_obs",
                            title="Adj. Scores - Observed",
                            renderFun=".plot_adj_obs",
                            width=700,
                            height=400,
                            visible="(plots:adj_obs)"))
                        self$add(jmvcore::Array$new(
                            options=options,
                            name="adj_pred",
                            title="Adj. Scores - Predictors",
                            visible="(plots:adj_pred)",
                            template=jmvcore::Image$new(
                                options=options,
                                title="",
                                renderFun=".plot_adj_pred",
                                width=700,
                                height=400)))
                        self$add(jmvcore::Html$new(
                            options=options,
                            name="issues",
                            title="Info",
                            visible=FALSE))}))$new(options=options))
            self$add(R6::R6Class(
                inherit = jmvcore::Group,
                active = list(
                    formula = function() private$.items[["formula"]],
                    es = function() private$.items[["es"]],
                    percentiles = function() private$.items[["percentiles"]],
                    raws = function() private$.items[["raws"]]),
                private = list(),
                public=list(
                    initialize=function(options) {
                        super$initialize(
                            options=options,
                            name="scores",
                            title="Scoring Results")
                        self$add(jmvcore::Html$new(
                            options=options,
                            name="formula",
                            title="Formula"))
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="es",
                            title="Equivalent Scores",
                            refs=list(
                                "facchin",
                                "aiello",
                                "capitani"),
                            columns=list(
                                list(
                                    `name`="method", 
                                    `title`="Method", 
                                    `type`="text"),
                                list(
                                    `name`="es0", 
                                    `title`="0", 
                                    `type`="text"),
                                list(
                                    `name`="es1", 
                                    `title`="1", 
                                    `type`="text"),
                                list(
                                    `name`="es2", 
                                    `title`="2", 
                                    `type`="text"),
                                list(
                                    `name`="es3", 
                                    `title`="3", 
                                    `type`="text"),
                                list(
                                    `name`="es4", 
                                    `title`="4", 
                                    `type`="text"),
                                list(
                                    `name`="otl", 
                                    `title`="oTL", 
                                    `type`="number"),
                                list(
                                    `name`="itl", 
                                    `title`="iTL", 
                                    `type`="number"))))
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="percentiles",
                            title="Percentiles",
                            columns=list(
                                list(
                                    `name`="perc", 
                                    `title`="Percentile", 
                                    `type`="integer"),
                                list(
                                    `name`="val", 
                                    `title`="Adj.Value", 
                                    `type`="number"))))
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="raws",
                            title="Raw and Adjusted",
                            visible="(score_adjust)",
                            columns=list(
                                list(
                                    `name`="id", 
                                    `title`="Person row", 
                                    `type`="integer"),
                                list(
                                    `name`="raw", 
                                    `title`="Raw Score", 
                                    `type`="number", 
                                    `visible`="(score_raw)"),
                                list(
                                    `name`="adj", 
                                    `title`="Adj.Value", 
                                    `type`="number"))))}))$new(options=options))}))

adjustingBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "adjustingBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "scalmj",
                name = "adjusting",
                version = c(0,0,3),
                options = options,
                results = adjustingResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

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
    factors,
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
    .interface = "jamovi") {

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

