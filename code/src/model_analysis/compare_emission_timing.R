library(data.table)
library(ggplot2)
library(reshape2)

# Load Moore model and prior2post logic
source("src/model.R")
source("src/model_analysis/prior2post.R")

# Read prior draws and convert to numeric
priordraws <- fread("../results/priordraws_moore.csv")
priordraws <- as.data.frame(lapply(priordraws, as.numeric))

# Parameter names (in order)
param_names <- c("Homophily", "Strong.Force", "Weak.Force", "Evidence",
                 "Pol.Opinion", "Status.Quo.Bias", "Pol.Int.Feedback",
                 "Biased.Assimilation", "Shifting.Baselines")

# Simulate survey responses for each parameter
set.seed(123)
surveyresponse_list <- list(
    Homophily           = sample(c("Not a barrier", "Small", "Moderate", "Significant"), 30, TRUE, c(0.2, 0.4, 0.3, 0.1)),
    Strong.Force        = sample(c("Not a barrier", "Small", "Moderate", "Significant"), 30, TRUE, c(0.25, 0.35, 0.3, 0.1)),
    Weak.Force          = sample(c("Not a barrier", "Small", "Moderate", "Significant"), 30, TRUE, c(0.3, 0.4, 0.2, 0.1)),
    Evidence            = sample(c("Not a barrier", "Small", "Moderate", "Significant"), 30, TRUE, c(0.15, 0.25, 0.4, 0.2)),
    Pol.Opinion         = sample(c("Not a barrier", "Small", "Moderate", "Significant"), 30, TRUE, c(0.25, 0.25, 0.3, 0.2)),
    Status.Quo.Bias     = sample(c("Not a barrier", "Small", "Moderate", "Significant"), 30, TRUE, c(0.3, 0.3, 0.2, 0.2)),
    Pol.Int.Feedback    = sample(c("Not a barrier", "Small", "Moderate", "Significant"), 30, TRUE, c(0.25, 0.35, 0.3, 0.1)),
    Biased.Assimilation = sample(c("Not a barrier", "Small", "Moderate", "Significant"), 30, TRUE, c(0.3, 0.3, 0.3, 0.1)),
    Shifting.Baselines  = sample(c("Not a barrier", "Small", "Moderate", "Significant"), 30, TRUE, c(0.35, 0.3, 0.2, 0.15))
)
names(surveyresponse_list) <- param_names

# Generate posteriors
postdraws <- as.data.frame(prior2post(as.matrix(priordraws), surveyresponse_list))
postdraws$Shifting.Baselines <- as.numeric(postdraws$Shifting.Baselines > 0.5)

# Set up model dimensions
years <- 2020:2100
nyears <- length(years)
ndraws <- nrow(priordraws)

# Initialize delay matrix
delay_matrix <- matrix(NA, nrow = ndraws, ncol = nyears)
colnames(delay_matrix) <- years

# Wrapper to run Moore's model
run_model_from_draw <- function(draw_row) {
    model(
        homophily_param = as.numeric(draw_row[[1]]),
        forcestrong = as.numeric(draw_row[[2]]),
        forceweak = as.numeric(draw_row[[3]]),
        evidenceeffect = as.numeric(draw_row[[4]]),
        policyopinionfeedback_param = as.numeric(draw_row[[5]]),
        pol_response = as.numeric(draw_row[[6]]),
        pol_feedback = as.numeric(draw_row[[7]]),
        biassedassimilation = as.numeric(draw_row[[8]]),
        shiftingbaselines = as.numeric(draw_row[[9]]),
        time = 1:81,
        year0 = 2020,
        frac_opp_0 = 0.07,
        frac_neut_0 = 0.22
    )
}

# Simulate emissions for priors and posteriors
for (i in seq_len(ndraws)) {
    prior_model <- tryCatch(run_model_from_draw(priordraws[i, ]), error = function(e) NULL)
    post_model  <- tryCatch(run_model_from_draw(postdraws[i, ]),  error = function(e) NULL)
    
    if (is.null(prior_model) || is.null(post_model)) next
    
    prior_emissions <- prior_model$totalemissions
    post_emissions  <- post_model$totalemissions
    
    for (y in seq_len(nyears)) {
        e_Y <- prior_emissions[y]
        x_year_index <- which(post_emissions <= e_Y)[1]
        if (!is.na(x_year_index)) delay_matrix[i, y] <- x_year_index - y
    }
    
    if (i %% 10 == 0) print(paste("Completed draw", i))
}

write.csv(delay_matrix, "../results/delay_matrix.csv", row.names = FALSE)

# Collect emissions paths
prior_emissions_mat <- matrix(NA, ndraws, nyears)
post_emissions_mat  <- matrix(NA, ndraws, nyears)

for (i in seq_len(ndraws)) {
    prior_model <- tryCatch(run_model_from_draw(priordraws[i, ]), error = function(e) NULL)
    post_model  <- tryCatch(run_model_from_draw(postdraws[i, ]),  error = function(e) NULL)
    
    if (is.null(prior_model) || is.null(post_model)) next
    
    prior_emissions_mat[i, ] <- prior_model$totalemissions
    post_emissions_mat[i, ]  <- post_model$totalemissions
    
    if (i %% 1000 == 0) print(paste("Storing emissions for draw", i))
}

# Compute mean emissions
mean_prior_emissions <- colMeans(prior_emissions_mat, na.rm = TRUE)
mean_post_emissions  <- colMeans(post_emissions_mat, na.rm = TRUE)

emissions_df <- data.table(
    Year = years,
    Prior = mean_prior_emissions,
    Posterior = mean_post_emissions
)

fwrite(emissions_df, "../results/emissions_compare_prior_posterior.csv")

# Plot mean trajectories
ggplot(emissions_df, aes(x = Year)) +
    geom_line(aes(y = Prior, color = "Moore Prior"), linewidth = 1.2) +
    geom_line(aes(y = Posterior, color = "Posterior (Survey-Informed)"), linewidth = 1.2, linetype = "dashed") +
    scale_color_manual(values = c("Moore Prior" = "#1f78b4", "Posterior (Survey-Informed)" = "#e31a1c")) +
    labs(
        title = "Comparison of Emissions Paths: Prior vs Posterior Beliefs",
        y = "Global Emissions (GtC per year)",
        x = "Year", color = ""
    ) +
    theme_minimal(base_size = 14)

# Density plots for each parameter
priors_long <- melt(priordraws, variable.name = "Parameter")
posts_long  <- melt(postdraws, variable.name = "Parameter")
priors_long$Group <- "Prior"
posts_long$Group  <- "Posterior"

ggplot(rbind(priors_long, posts_long), aes(x = value, fill = Group)) +
    geom_density(alpha = 0.4) +
    facet_wrap(~Parameter, scales = "free") +
    theme_minimal()
