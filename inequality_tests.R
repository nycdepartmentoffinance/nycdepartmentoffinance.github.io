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



train_small = read_csv("models/Data/Coop/train_small.csv") %>%
    select(-1)

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
            ci_model = NA
            #mki = NA,
            #ki = NA,
            #regression appraoch
            #mki_model = NA,
            #ki_model = NA
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
            y_ci = 2 * var(frac_rank) * predicted / mean(predicted)
        )

    # covariance approach
    gini <- ((2 * cov(as.matrix(dataframe$frac_rank), as.matrix(dataframe$actual)) /
                  mean(dataframe$actual)) * ((n - 1) / n)
    )[1]

    ci <- ((2 * cov(as.matrix(dataframe$frac_rank), as.matrix(dataframe$predicted)) /
                mean(dataframe$predicted)) * ((n - 1) / n)
    )[1]

    # covariance approach
    gini_sample <- ((2 * cov(as.matrix(dataframe$frac_rank), as.matrix(dataframe$actual)) /
                         mean(dataframe$actual)) #* ((n - 1) / n)
    )[1]

    ci_sample <- ((2 * cov(as.matrix(dataframe$frac_rank), as.matrix(dataframe$predicted)) /
                       mean(dataframe$predicted)) #* ((n - 1) / n)
    )[1]

    # regression approach
    gini_model <- lm(y_gini ~ frac_rank, data = dataframe)$coefficients[['frac_rank']]
    ci_model <- lm(y_ci ~ frac_rank, data = dataframe)$coefficients[['frac_rank']]

    # Return all results
    list(
        #cov approach
        n = n,
        gini_population = gini,
        gini_sample = gini_sample,
        gini_model = gini_model,

        ci_population = ci,
        ci_sample = ci_sample,
        ci_model = ci_model

        #regression approach
        #mki = ci / gini,
        #ki = ci - gini,
        #mki_model = ci_model / gini_model,
        #ki_model = ci_model - gini_model,
    )
}

# for a big sample size, they are the same if we dont use the sample adjustment in the cov approach: * ((n - 1) / n)
# for a smaller sample size,
metrics = compute_inequality_metrics(train_small$prediction, train_small$GROSSCON)


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
    group_modify(~ {
        res <- compute_inequality_metrics(.x$prediction, .x$GROSSCON)

        # Return scalar outputs as columns, and keep list-cols for complex outputs
        tibble(
            n = res$n,
            gini = res$gini,
            ci = res$ci,
            mki = res$mki,
            ki = res$ki,
            gini_model = res$gini_model,
            ci_model = res$ci_model,
            mki_model = res$mki_model,
            ki_model = res$ki_model
        )
    }) %>%
    ungroup()

write_csv(neighborhood_metrics, "Data/Coop/equality_metrics_neighborhood_decile.csv")
write_csv(boro_metrics, "Data/Coop/equality_metrics_boro_decile.csv")
