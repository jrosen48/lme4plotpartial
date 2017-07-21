#' Plot partial pooling estimates for a linear mixed effects model
#' @param df a data.frame containing the variables used in the model
#' @param y_var the raw (unquoted) name of the dependent variable
#' @param x_var the raw (unquoted) name of the independent variable
#' @param group the raw (unquoted) name of the group factor
#' @return a ggplot2 plot
#' @import rlang
#' @import ggplot2
#' @importFrom magrittr '%>%'
#' @examples
#' library(dplyr)
#' plot_partial_pooling(storms, y_var = wind, x_var = pressure, group = year)
#' @export
plot_partial_pooling <- function(df, y_var, x_var, group, ...) {
    # group_enquo <- quo(year)
    # y_var_enquo <- quo(wind)
    # x_var_enquo <- quo(pressure)
    
    group_enquo <- enquo(group)
    x_var_enquo <- enquo(x_var)
    y_var_enquo <- enquo(y_var)
    
    y_var_q <- quo_name(y_var_enquo)
    x_var_q <- quo_name(x_var_enquo)
    group_q <- quo_name(group_enquo)
    
    # Construct model formulas
    rhs_pooled <- quo(UQE(x_var_enquo))
    rhs_not_pooled <- quo(UQE(x_var_enquo) | UQE(group_enquo))
    rhs_lmer <- quo(UQE(x_var_enquo) + (UQE(x_var_enquo) | UQE(group_enquo)))
    
    f_not_pooled <- new_formula(UQE(y_var_enquo), UQE(rhs_not_pooled))
    f_pooled <- new_formula(UQE(y_var_enquo), UQE(rhs_pooled))
    f_lmer <- new_formula(UQE(y_var_enquo), UQE(rhs_lmer))
    
    df <- dplyr::filter(df, !is.na(!!group_enquo))

    # this function was printing a warning 
    # "Unknown or uninitialized column: '(offset)' and '(weights)'
    m_not_pooled <- suppressWarnings(lme4::lmList(f_not_pooled, data = df))

    df_not_pooled <- m_not_pooled %>%
        stats::coef() %>%
        tibble::rownames_to_column(group_q) %>%
        dplyr::rename(Intercept = `(Intercept)`, Slope = !!x_var_enquo) %>%
        tibble::add_column(Model = "No pooling") %>%
        tibble::as.tibble()

    m_pooled <- stats::lm(f_pooled, data = df)

    x <- dplyr::pull(df, !!group_enquo)

    df_pooled <- dplyr::data_frame(
        Model = "Complete pooling",
        tmp = as.character(unique(x)),
        Intercept = stats::coef(m_pooled)[1],
        Slope = stats::coef(m_pooled)[2])

    names(df_pooled)[2] <- group_q

    # partially_pooled_formula <- as.formula(paste(y_var_q, " ~ ", x_var_q, " + (1 |", group_q, ")", sep = ""))
    m <- lme4::lmer(f_lmer, data = df)

    df_partially_pooled <- stats::coef(m)[[group_q]] %>%
        tibble::rownames_to_column(group_q) %>%
        dplyr::rename(Intercept = `(Intercept)`, Slope = !! x_var_enquo) %>%
        tibble::add_column(Model = "Partial pooling") %>%
        tibble::as_tibble()

    df[[group_q]] <- as.character(df[[group_q]])
    
    df_models <- dplyr::bind_rows(df_pooled, df_not_pooled, df_partially_pooled) %>%
        dplyr::left_join(df, by = group_q)

    p_model_comparison <- ggplot(df_models, aes_string(y_var_q, x = x_var_q)) +
        geom_point() +
        geom_abline(ggplot2::aes_(intercept = ~ Intercept, slope = ~ Slope, color = ~ Model)) +
        facet_wrap(group_q) +
        scale_color_brewer(palette = "Dark2") +
        theme(legend.position = "top")

    p_model_comparison

}
