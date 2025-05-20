# Convert survey response to fractional success/failure
survey_to_sf <- function(response) {
    switch(response,
           "Not a barrier" = c(s = 0, f = 1),
           "Small"         = c(s = 1/3, f = 2/3),
           "Moderate"      = c(s = 2/3, f = 1/3),
           "Significant"   = c(s = 1, f = 0),
           stop("Unknown response: ", response))
}

# Update a single prior value with survey evidence
posterior <- function(prior_val, responses) {
    sf <- vapply(responses, survey_to_sf, numeric(2))
    rbeta(1, 1 + sum(sf[1, ]), 1 + sum(sf[2, ]))
}

# Apply update across all parameters and draws
prior2post <- function(priordraws, surveyresponse_list) {
    stopifnot(length(surveyresponse_list) == ncol(priordraws))
    
    postdraws <- priordraws
    for (j in seq_len(ncol(priordraws))) {
        for (i in seq_len(nrow(priordraws))) {
            postdraws[i, j] <- posterior(priordraws[i, j], surveyresponse_list[[j]])
        }
    }
    postdraws
}
