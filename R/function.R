#' Plot partial pooling estimates for a linear mixed effects model
#' @param df a data.frame containing the variables used in the model
#' @param y_var the raw (unquoted) name of the dependent variable
#' @param x_var the raw (unquoted) name of the independent variable
#' @param group the raw (unquoted) name of the group factor
#' @return a ggplot2 plot
#' @examples
#' library(dplyr)
#' storms
#' plot_partial_pooling(storms, y_var = wind, x_var = pressure, group = year)
#' @export

plot_partial_pooling <- function(df, y_var, x_var, group, ...) {

    group_enquo <- enquo(group)
    y_var_q <- as.character(substitute(y_var))
    x_var_q <- as.character(substitute(x_var))
    group_q <- as.character(substitute(group))

    df <- dplyr::filter(df, !is.na(!! group_enquo))

    not_pooled_formula <- as.formula(paste(y_var_q, " ~ ", x_var_q, " | ", group_q, sep = ""))

    # this function was printing a warning "Unknown or uninitialized column: '(offset)' and '(weights)'
    m_not_pooled <- suppressWarnings(lme4::lmList(not_pooled_formula, data = df))

    x_var_enquo <- enquo(x_var)
    y_var_enquo <- enquo(y_var)

    df_not_pooled <- m_not_pooled %>%
        coef() %>%
        tibble::rownames_to_column(group_q) %>%
        dplyr::rename(Intercept = `(Intercept)`, Slope = !!x_var_enquo) %>%
        tibble::add_column(Model = "No pooling") %>%
        tibble::as.tibble()

    pooled_formula <- as.formula(paste(y_var_q, " ~ ", x_var_q, sep = ""))
    m_pooled <- lm(pooled_formula, data = df)

    x <- pull(df, !!group_enquo)

    df_pooled <- tibble::as.tibble(dplyr::data_frame(
        Model = "Complete pooling",
        tmp = as.character(unique(x)),
        Intercept = coef(m_pooled)[1],
        Slope = coef(m_pooled)[2])
    )

    names(df_pooled)[2] <- group_q

    partially_pooled_formula <- as.formula(paste(y_var_q, " ~ ", x_var_q, " + (1 |", group_q, ")", sep = ""))

    m <- lme4::lmer(partially_pooled_formula, data = df)

    df_partially_pooled <- coef(m)[[group_q]] %>%
        tibble::rownames_to_column(group_q) %>%
        dplyr::rename(Intercept = `(Intercept)`, Slope = !! x_var_enquo) %>%
        tibble::add_column(Model = "Partial pooling") %>%
        tibble::as_tibble()

    df <- dplyr::mutate(df, year = as.character(year))

    df_models <- dplyr::bind_rows(df_pooled, df_not_pooled, df_partially_pooled) %>%
        dplyr::left_join(df, by = group_q)

    p_model_comparison <- ggplot2::ggplot(df_models, ggplot2::aes_string(y_var_q, x = x_var_q)) +
        ggplot2::geom_point() +
        ggplot2::geom_abline(ggplot2::aes(intercept = Intercept, slope = Slope, color = Model)) +
        ggplot2::facet_wrap(group_q) +
        ggplot2::scale_color_brewer(palette = "Dark2") +
        ggplot2::theme(legend.position = "top")

    p_model_comparison

}
