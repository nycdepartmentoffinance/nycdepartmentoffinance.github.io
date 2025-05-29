# read env file to get proxy set up to work
readRenviron("C:/Users/BoydClaire/.Renviron")
options(scipen=99999)

library(readxl)
library(stringr)
library(readr)
library(ggplot2)
library(dplyr)
library(sf)
library(rmarkdown)
library(maps)
library(leaflet)
library(scales)
library(DT)
library(assessr)
library(rineq)

train_small = read_csv("models/Data/Coop/train_small.csv") %>%
    select(-1)

quintos = read_csv("models/Data/Coop/quintos_sample.csv")



# Calculate the Gini cofficients needed for KI and MKI
calc_gini <- function(assessed, sale_price) {
    df <- data.frame(av = assessed, sp = sale_price)
    df <- df[order(df$sp), ]
    assessed_price <- df$av
    sale_price <- df$sp
    n <- length(assessed_price)

    av_sum <- sum(assessed_price * seq_len(n))
    g_assessed <- 2 * av_sum / sum(assessed_price) - (n + 1L)
    gini_assessed <- g_assessed / n

    sale_sum <- sum(sale_price * seq_len(n))
    g_sale <- 2 * sale_sum / sum(sale_price) - (n + 1L)
    gini_sale <- g_sale / n

    result <- list(gini_assessed = gini_assessed, gini_sale = gini_sale)

    return(result)
}

compute_inequality_metrics <- function(predicted, actual) {

    # return NAs if n is less than 2
    n <- length(predicted)

    if (n < 2) {
        return(list(
            n = n,

            gini_population = NA,
            gini_sample = NA,
            gini_model = NA,

            ci_population = NA,
            ci_sample = NA,
            ci_model = NA,
            ci_rineq = NA,

            #regression appraoch
            mki_model = NA,
            ki_model = NA
        ))
    }

    # Sanity check: length of vectors
    if (length(predicted) != length(actual)) {
        stop("predicted and actual must be the same length.")
    }

    # Create a dataframe with computed ranks
    dataframe <- data.frame(
        predicted = predicted,
        actual = actual
    ) %>%
        mutate(
            rank = rank(actual, ties.method = "average"),
            frac_rank = rank / n,
            y_gini = 2 * var(frac_rank) * actual / mean(actual),
            y_ci = 2 * var(frac_rank) * predicted / mean(predicted),
            y_ki = 2 * var(frac_rank) * (predicted / mean(predicted) - actual / mean(actual)),
            y_mki_1 = actual / mean(actual),
            y_mki_2 = predicted / mean(predicted)
        )



    ki_model <- lm(y_ki ~ frac_rank, data = dataframe)$coefficients[['frac_rank']]

    mki_first_stage <- lm(y_mki_1 ~ frac_rank, data = dataframe)
    dataframe$first_stage_output = predict(mki_first_stage)
    mki_second_stage <- lm(y_mki_2 ~ first_stage_output, data = dataframe)$coefficients[['first_stage_output']]


    # covariance approach
    gini_sample <- ((2 * cov(as.matrix(dataframe$frac_rank), as.matrix(dataframe$actual)) /
                  mean(dataframe$actual)) * ((n - 1) / n)
    )[1]

    ci_sample <- ((2 * cov(as.matrix(dataframe$frac_rank), as.matrix(dataframe$predicted)) /
                mean(dataframe$predicted)) * ((n - 1) / n)
    )[1]

    # covariance approach
    gini_population <- ((2 * cov(as.matrix(dataframe$frac_rank), as.matrix(dataframe$actual)) /
                         mean(dataframe$actual))
    )[1]

    ci_population <- ((2 * cov(as.matrix(dataframe$frac_rank), as.matrix(dataframe$predicted)) /
                       mean(dataframe$predicted))
    )[1]

    # regression approach
    gini_model <- lm(y_gini ~ frac_rank, data = dataframe)$coefficients[['frac_rank']]
    ci_model <- lm(y_ci ~ frac_rank, data = dataframe)$coefficients[['frac_rank']]

    ci_assessr = calc_gini(predicted, actual)$gini_assessed
    gini_assessr = calc_gini(predicted, actual)$gini_sale

    # Return all results
    list(
        #cov approach
        n = n,

        ci_population = ci_population,
        gini_population = gini_population,
        ki_population = ci_population - gini_population,
        mki_population = ci_population / gini_population,

        gini_sample = gini_sample,
        ci_sample = ci_sample,
        ki_sample = ci_sample - gini_sample,
        mki_sample = ci_sample / gini_sample,


        gini_model = gini_model,
        ci_model = ci_model,
        ki_model = ki_model,
        mki_model = mki_second_stage,

        ci_assessr = ci_assessr,
        gini_assessr = gini_assessr,
        ki_assessr = ci_assessr - gini_assessr,
        mki_assessr = ci_assessr / gini_assessr,

        ci_rineq = rineq::ci(actual, predicted)$concentration_index
    )
}

# for a big sample size, they are the same if we dont use the sample adjustment in the cov approach: * ((n - 1) / n)
# for a smaller sample size,
metrics = compute_inequality_metrics(train_small$prediction, train_small$GROSSCON)

#metrics = compute_inequality_metrics(quintos$estimate, quintos$sale_price)

boro_metrics = train_small %>%
    group_by(boro, decile) %>%
    summarize(
        summary = list(compute_inequality_metrics(
            prediction,
            GROSSCON))
    ) %>%
    tidyr::unnest_wider(summary)

neighborhood_metrics = train_small %>%
    group_by(NEIGHBORHOOD, decile) %>%
    summarize(
        summary = list(compute_inequality_metrics(
            prediction,
            GROSSCON))
    ) %>%
    tidyr::unnest_wider(summary)



write_csv(neighborhood_metrics, "models/Data/Coop/equality_metrics_neighborhood_decile.csv")
write_csv(boro_metrics, "models/Data/Coop/equality_metrics_boro_decile.csv")


si_decile4 = train_small %>%
    filter(boro==5 & decile==4)

neighborhood4250_decile6 = train_small %>%
    filter(NEIGHBORHOOD==4250 & decile==6)

neighborhood1120_decile1 = train_small %>%
    filter(NEIGHBORHOOD==1120 & decile==1)

write_csv(si_decile4, "models/Data/Coop/si_decile4.csv")
write_csv(neighborhood4250_decile6, "models/Data/Coop/neighborhood4250_decile6.csv")
write_csv(neighborhood1120_decile1, "models/Data/Coop/neighborhood1120_decile1.csv")






