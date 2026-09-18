#!/usr/bin/env Rscript

# Household Food Security and Self-Rated Health
# 2024 National Health Interview Survey Sample Adult File
# Reproducible survey-weighted analysis for the public project report

suppressPackageStartupMessages({
  library(survey)
  library(MASS)
})

options(survey.lonely.psu = "adjust")

input_file <- file.path("data_raw", "adult24.csv")
output_dir <- "analysis_output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(input_file)) {
  stop(
    "The NHIS public-use file was not found at ", input_file, ".\n",
    "Download adult24csv.zip from the 2024 NHIS data page, extract adult24.csv, ",
    "and place it in data_raw/."
  )
}

nhis <- read.csv(input_file, check.names = FALSE)

activity_component <- function(x) {
  # 0 indicates no reported activity; 1-995 and 9995 indicate some activity.
  # Code 9996 means unable to do the activity and is treated as no activity.
  # Refused/not ascertained/don't know (9997-9999) are missing.
  ifelse(x == 0 | x == 9996, 0,
         ifelse((x >= 1 & x <= 995) | x == 9995, 1, NA_real_))
}

vigorous_any <- activity_component(nhis$VIGNR_A)
moderate_any <- activity_component(nhis$MODNR_A)

activity_any <- ifelse(
  vigorous_any == 1 | moderate_any == 1,
  1,
  ifelse(vigorous_any == 0 & moderate_any == 0, 0, NA_real_)
)

analysis_data <- data.frame(
  PPSU = nhis$PPSU,
  PSTRAT = nhis$PSTRAT,
  WTFA_A = nhis$WTFA_A,
  self_rated_health = ordered(
    ifelse(nhis$PHSTAT_A %in% 1:5, nhis$PHSTAT_A, NA_real_),
    levels = 1:5,
    labels = c("Excellent", "Very good", "Good", "Fair", "Poor")
  ),
  food_security = factor(
    ifelse(nhis$FDSCAT4_A %in% 1:4, nhis$FDSCAT4_A, NA_real_),
    levels = 1:4,
    labels = c(
      "High food security",
      "Marginal food security",
      "Low food security",
      "Very low food security"
    )
  ),
  age = ifelse(nhis$AGEP_A >= 18 & nhis$AGEP_A <= 85, nhis$AGEP_A, NA_real_),
  age_group = cut(
    ifelse(nhis$AGEP_A >= 18 & nhis$AGEP_A <= 85, nhis$AGEP_A, NA_real_),
    breaks = c(18, 35, 50, 65, Inf),
    right = FALSE,
    labels = c("18-34", "35-49", "50-64", "65 or older")
  ),
  sex = factor(
    ifelse(nhis$SEX_A %in% 1:2, nhis$SEX_A, NA_real_),
    levels = 1:2,
    labels = c("Male", "Female")
  ),
  race_ethnicity = factor(
    ifelse(nhis$HISPALLP_A %in% 1:7,
           ifelse(nhis$HISPALLP_A %in% 5:7, 5, nhis$HISPALLP_A),
           NA_real_),
    levels = c(2, 3, 1, 4, 5),
    labels = c(
      "Non-Hispanic White",
      "Non-Hispanic Black",
      "Hispanic",
      "Non-Hispanic Asian",
      "Non-Hispanic AIAN, other, or multiple race"
    )
  ),
  education = factor(
    ifelse(
      nhis$EDUCP_A %in% 0:10,
      ifelse(nhis$EDUCP_A <= 2, 4,
             ifelse(nhis$EDUCP_A <= 4, 3,
                    ifelse(nhis$EDUCP_A <= 7, 2, 1))),
      NA_real_
    ),
    levels = 1:4,
    labels = c(
      "Bachelor's degree or higher",
      "Some college or associate degree",
      "High school graduate or GED",
      "Less than high school"
    )
  ),
  marital_status = factor(
    ifelse(nhis$MARITAL_A %in% 1:3, nhis$MARITAL_A, NA_real_),
    levels = 1:3,
    labels = c(
      "Married",
      "Living with partner",
      "Neither married nor living with partner"
    )
  ),
  region = factor(
    ifelse(nhis$REGION %in% 1:4, nhis$REGION, NA_real_),
    levels = 1:4,
    labels = c("Northeast", "Midwest", "South", "West")
  ),
  smoking_status = factor(
    ifelse(nhis$SMKCIGST_A %in% 1:4,
           ifelse(nhis$SMKCIGST_A %in% 1:2, 3,
                  ifelse(nhis$SMKCIGST_A == 3, 2, 1)),
           NA_real_),
    levels = 1:3,
    labels = c("Never smoker", "Former smoker", "Current smoker")
  ),
  alcohol_status = factor(
    ifelse(nhis$DRKSTAT_A %in% 1:9,
           ifelse(nhis$DRKSTAT_A == 1, 1,
                  ifelse(nhis$DRKSTAT_A %in% 2:4, 2, 3)),
           NA_real_),
    levels = 1:3,
    labels = c("Lifetime abstainer", "Former drinker", "Current drinker")
  ),
  physical_activity = factor(
    activity_any,
    levels = 0:1,
    labels = c(
      "No moderate or vigorous leisure-time activity",
      "Any moderate or vigorous leisure-time activity"
    )
  ),
  bmi_category = factor(
    ifelse(nhis$BMICAT_A %in% 1:4, nhis$BMICAT_A, NA_real_),
    levels = c(2, 1, 3, 4),
    labels = c("Healthy weight", "Underweight", "Overweight", "Obesity")
  ),
  sleep_category = factor(
    ifelse(
      nhis$SLPHOURS_A >= 1 & nhis$SLPHOURS_A <= 24,
      ifelse(nhis$SLPHOURS_A < 7, 2,
             ifelse(nhis$SLPHOURS_A <= 9, 1, 3)),
      NA_real_
    ),
    levels = 1:3,
    labels = c("7-9 hours", "Less than 7 hours", "More than 9 hours")
  ),
  insurance_status = factor(
    ifelse(nhis$NOTCOV_A %in% 1:2, nhis$NOTCOV_A, NA_real_),
    levels = c(2, 1),
    labels = c("Covered", "Not covered")
  )
)

model_variables <- c(
  "self_rated_health", "food_security", "age_group", "sex",
  "race_ethnicity", "education", "marital_status", "region",
  "smoking_status", "alcohol_status", "physical_activity",
  "bmi_category", "sleep_category", "insurance_status"
)

analysis_data$complete_case <- complete.cases(analysis_data[, model_variables])

full_design <- svydesign(
  id = ~PPSU,
  strata = ~PSTRAT,
  weights = ~WTFA_A,
  nest = TRUE,
  data = analysis_data
)

analytic_design <- subset(full_design, complete_case)
analytic_data <- subset(analysis_data, complete_case)

adjustment_terms <- paste(
  c(
    "food_security", "age_group", "sex", "race_ethnicity", "education",
    "marital_status", "region", "smoking_status", "alcohol_status",
    "physical_activity", "bmi_category", "sleep_category", "insurance_status"
  ),
  collapse = " + "
)

unadjusted_formula <- self_rated_health ~ food_security
adjusted_formula <- as.formula(paste("self_rated_health ~", adjustment_terms))

unadjusted_model <- svyolr(
  unadjusted_formula,
  design = analytic_design,
  method = "logistic"
)

adjusted_model <- svyolr(
  adjusted_formula,
  design = analytic_design,
  method = "logistic"
)

design_df <- degf(analytic_design)

tidy_svyolr <- function(model, selected_terms = NULL) {
  estimates <- coef(model)
  standard_errors <- sqrt(diag(vcov(model)))[seq_along(estimates)]
  term_names <- names(estimates)
  model_df <- if (!is.null(model$df.residual)) model$df.residual else design_df
  if (!is.null(selected_terms)) {
    keep <- term_names %in% selected_terms
    estimates <- estimates[keep]
    standard_errors <- standard_errors[keep]
    term_names <- term_names[keep]
  }
  critical <- qt(0.975, df = model_df)
  t_stat <- estimates / standard_errors
  data.frame(
    term = term_names,
    estimate = unname(estimates),
    standard_error = unname(standard_errors),
    odds_ratio = exp(unname(estimates)),
    conf_low = exp(unname(estimates) - critical * unname(standard_errors)),
    conf_high = exp(unname(estimates) + critical * unname(standard_errors)),
    p_value = 2 * pt(abs(unname(t_stat)), df = model_df, lower.tail = FALSE),
    stringsAsFactors = FALSE
  )
}

food_terms <- paste0(
  "food_security",
  c("Marginal food security", "Low food security", "Very low food security")
)

primary_unadjusted <- tidy_svyolr(unadjusted_model, food_terms)
primary_adjusted <- tidy_svyolr(adjusted_model, food_terms)

primary_results <- merge(
  primary_unadjusted,
  primary_adjusted,
  by = "term",
  suffixes = c("_unadjusted", "_adjusted"),
  sort = FALSE
)
primary_results <- primary_results[match(food_terms, primary_results$term), ]
primary_results$food_security <- sub("^food_security", "", primary_results$term)

overall_unadjusted <- regTermTest(unadjusted_model, ~food_security, method = "Wald")
overall_adjusted <- regTermTest(adjusted_model, ~food_security, method = "Wald")

overall_results <- data.frame(
  model = c("Unadjusted", "Adjusted"),
  statistic_type = "Design-based Wald F",
  statistic = c(overall_unadjusted$Ftest, overall_adjusted$Ftest),
  numerator_df = c(overall_unadjusted$df, overall_adjusted$df),
  denominator_df = c(overall_unadjusted$ddf, overall_adjusted$ddf),
  p_value = c(overall_unadjusted$p, overall_adjusted$p)
)

# Survey-weighted descriptive table, using one common analytic cohort.
food_levels <- levels(analytic_data$food_security)
designs <- c(list(Overall = analytic_design), setNames(lapply(food_levels, function(x) {
  subset(analytic_design, food_security == x)
}), food_levels))

fmt_count_percent <- function(design, variable, level) {
  variable_values <- design$variables[[variable]]
  n_value <- sum(variable_values == level, na.rm = TRUE)
  indicator_formula <- as.formula(
    paste0("~I(as.numeric(`", variable, "` == ", deparse(level), "))")
  )
  p_value <- as.numeric(coef(svymean(indicator_formula, design, na.rm = TRUE)))
  sprintf("%s (%.1f)", format(n_value, big.mark = ",", scientific = FALSE), 100 * p_value)
}

fmt_age <- function(design) {
  value <- svymean(~age, design, na.rm = TRUE)
  sprintf("%.1f (%.2f)", as.numeric(coef(value)), as.numeric(SE(value)))
}

descriptive_rows <- list()
add_descriptive_row <- function(label, variable = NULL, level = NULL, type = "factor") {
  row <- data.frame(Characteristic = label, stringsAsFactors = FALSE)
  for (design_name in names(designs)) {
    row[[design_name]] <- if (type == "age") {
      fmt_age(designs[[design_name]])
    } else {
      fmt_count_percent(designs[[design_name]], variable, level)
    }
  }
  descriptive_rows[[length(descriptive_rows) + 1]] <<- row
}

add_descriptive_row("Age, weighted mean (SE)", type = "age")

table_variables <- list(
  age_group = levels(analytic_data$age_group),
  sex = c("Female"),
  race_ethnicity = levels(analytic_data$race_ethnicity),
  education = levels(analytic_data$education),
  marital_status = levels(analytic_data$marital_status),
  region = levels(analytic_data$region),
  smoking_status = levels(analytic_data$smoking_status),
  alcohol_status = levels(analytic_data$alcohol_status),
  physical_activity = c("No moderate or vigorous leisure-time activity"),
  bmi_category = levels(analytic_data$bmi_category),
  sleep_category = levels(analytic_data$sleep_category),
  insurance_status = c("Not covered")
)

for (variable in names(table_variables)) {
  for (level in table_variables[[variable]]) {
    add_descriptive_row(as.character(level), variable, as.character(level))
  }
}

table1 <- do.call(rbind, descriptive_rows)

# Weighted self-rated-health percentages within food-security groups.
outcome_by_food <- svyby(
  ~self_rated_health,
  ~food_security,
  analytic_design,
  svymean,
  vartype = c("se", "ci"),
  na.rm = TRUE,
  keep.names = FALSE
)

# Sensitivity analysis for the proportional-odds assumption: fit the same
# survey-weighted model at each cumulative outcome threshold.
threshold_labels <- c(
  "Very good or worse vs excellent",
  "Good or worse vs excellent/very good",
  "Fair or poor vs excellent/very good/good",
  "Poor vs excellent/very good/good/fair"
)

threshold_results <- list()
for (cutpoint in 1:4) {
  analytic_design$variables$worse_binary <-
    as.numeric(analytic_design$variables$self_rated_health) > cutpoint
  threshold_formula <- as.formula(paste("worse_binary ~", adjustment_terms))
  threshold_model <- svyglm(
    threshold_formula,
    design = analytic_design,
    family = quasibinomial()
  )
  coefficients <- coef(threshold_model)[food_terms]
  standard_errors <- sqrt(diag(vcov(threshold_model)))[food_terms]
  critical <- qt(0.975, df = design_df)
  threshold_df <- threshold_model$df.residual
  critical <- qt(0.975, df = threshold_df)
  threshold_results[[cutpoint]] <- data.frame(
    threshold = threshold_labels[cutpoint],
    term = names(coefficients),
    odds_ratio = exp(unname(coefficients)),
    conf_low = exp(unname(coefficients) - critical * unname(standard_errors)),
    conf_high = exp(unname(coefficients) + critical * unname(standard_errors)),
    p_value = 2 * pt(
      abs(unname(coefficients / standard_errors)),
      df = threshold_df,
      lower.tail = FALSE
    ),
    stringsAsFactors = FALSE
  )
}
threshold_results <- do.call(rbind, threshold_results)

# Coefficient-level weighted variance inflation factors for the adjusted model.
design_matrix <- model.matrix(
  ~ food_security + age_group + sex + race_ethnicity + education +
    marital_status + region + smoking_status + alcohol_status +
    physical_activity + bmi_category + sleep_category + insurance_status,
  data = analytic_data
)[, -1, drop = FALSE]

weighted_vif <- sapply(seq_len(ncol(design_matrix)), function(index) {
  response <- design_matrix[, index]
  predictors <- design_matrix[, -index, drop = FALSE]
  fit <- lm.wfit(
    x = cbind(Intercept = 1, predictors),
    y = response,
    w = analytic_data$WTFA_A
  )
  fitted_values <- fit$fitted.values
  weighted_mean <- sum(analytic_data$WTFA_A * response) / sum(analytic_data$WTFA_A)
  total_ss <- sum(analytic_data$WTFA_A * (response - weighted_mean)^2)
  residual_ss <- sum(analytic_data$WTFA_A * (response - fitted_values)^2)
  r_squared <- 1 - residual_ss / total_ss
  1 / (1 - r_squared)
})

vif_results <- data.frame(
  coefficient = colnames(design_matrix),
  vif = weighted_vif,
  stringsAsFactors = FALSE
)
vif_results <- vif_results[order(vif_results$vif, decreasing = TRUE), ]

# Missingness audit for transparency. Counts are not mutually exclusive.
variable_labels <- c(
  self_rated_health = "Self-rated health",
  food_security = "Food security",
  age_group = "Age",
  sex = "Sex",
  race_ethnicity = "Race and ethnicity",
  education = "Education",
  marital_status = "Marital status",
  region = "Region",
  smoking_status = "Smoking status",
  alcohol_status = "Alcohol status",
  physical_activity = "Physical activity",
  bmi_category = "BMI category",
  sleep_category = "Sleep duration",
  insurance_status = "Insurance coverage"
)

missingness <- data.frame(
  variable = unname(variable_labels),
  missing_n = sapply(names(variable_labels), function(x) sum(is.na(analysis_data[[x]]))),
  missing_percent = 100 * sapply(
    names(variable_labels),
    function(x) mean(is.na(analysis_data[[x]]))
  ),
  stringsAsFactors = FALSE
)

full_model_results <- tidy_svyolr(adjusted_model)
full_model_results <- full_model_results[!grepl("\\|", full_model_results$term), ]

# Publication-quality figures for the report.
health_columns <- paste0("self_rated_health", levels(analytic_data$self_rated_health))
health_matrix <- t(as.matrix(outcome_by_food[, health_columns]))
colnames(health_matrix) <- outcome_by_food$food_security
row.names(health_matrix) <- levels(analytic_data$self_rated_health)
food_group_n <- table(analytic_data$food_security)
bar_labels <- paste0(names(food_group_n), "  (n = ", format(food_group_n, big.mark = ","), ")")

png(
  file.path(output_dir, "figure1_weighted_health_distribution.png"),
  width = 2400,
  height = 1500,
  res = 300,
  bg = "white"
)
par(mar = c(5.2, 13.5, 1.2, 1.2), family = "sans")
barplot(
  health_matrix,
  horiz = TRUE,
  beside = FALSE,
  col = c("#1F4E79", "#5B9BD5", "#D9E2F3", "#F4B183", "#C00000"),
  border = "white",
  names.arg = bar_labels,
  las = 1,
  xlim = c(0, 1),
  xaxt = "n",
  cex.names = 0.82,
  xlab = "Survey-weighted percentage"
)
axis(1, at = seq(0, 1, by = 0.2), labels = paste0(seq(0, 100, by = 20), "%"))
legend(
  "bottom",
  inset = c(0, -0.31),
  xpd = TRUE,
  horiz = TRUE,
  legend = row.names(health_matrix),
  fill = c("#1F4E79", "#5B9BD5", "#D9E2F3", "#F4B183", "#C00000"),
  border = NA,
  bty = "n",
  cex = 0.82
)
dev.off()

png(
  file.path(output_dir, "figure2_adjusted_food_security_or.png"),
  width = 2200,
  height = 1250,
  res = 300,
  bg = "white"
)
par(mar = c(5, 10.5, 1.2, 2.0), family = "sans")
plot(
  primary_adjusted$odds_ratio,
  seq_along(primary_adjusted$odds_ratio),
  type = "n",
  log = "x",
  xlim = c(0.9, 5.6),
  ylim = c(0.5, 3.5),
  yaxt = "n",
  xaxt = "n",
  xlab = "Adjusted proportional odds ratio (95% CI)",
  ylab = ""
)
abline(v = 1, lty = 2, col = "#7F8C8D")
segments(
  primary_adjusted$conf_low,
  seq_along(primary_adjusted$odds_ratio),
  primary_adjusted$conf_high,
  seq_along(primary_adjusted$odds_ratio),
  lwd = 2,
  col = "#1F4E79"
)
points(
  primary_adjusted$odds_ratio,
  seq_along(primary_adjusted$odds_ratio),
  pch = 19,
  cex = 1.25,
  col = "#1F4E79"
)
axis(
  2,
  at = seq_along(primary_adjusted$odds_ratio),
  labels = sub("^food_security", "", primary_adjusted$term),
  las = 1,
  tick = FALSE,
  cex.axis = 0.85
)
axis(1, at = c(1, 1.5, 2, 2.5, 3), labels = c("1.0", "1.5", "2.0", "2.5", "3.0"))
text(
  x = 3.55,
  y = seq_along(primary_adjusted$odds_ratio),
  labels = sprintf(
    "%.2f (%.2f-%.2f)",
    primary_adjusted$odds_ratio,
    primary_adjusted$conf_low,
    primary_adjusted$conf_high
  ),
  adj = 0,
  cex = 0.76,
  col = "#2C3E50"
)
dev.off()

sample_summary <- data.frame(
  measure = c(
    "Public-use sample adults",
    "Complete-case analytic sample",
    "Excluded from complete-case analysis",
    "Complete-case retention percent",
    "Weighted analytic population (millions)",
    "Survey design degrees of freedom",
    "Maximum coefficient-level VIF"
  ),
  value = c(
    nrow(analysis_data),
    nrow(analytic_data),
    nrow(analysis_data) - nrow(analytic_data),
    100 * nrow(analytic_data) / nrow(analysis_data),
    sum(weights(analytic_design, type = "sampling")) / 1e6,
    design_df,
    max(vif_results$vif)
  )
)

write.csv(analysis_data, file.path(output_dir, "analysis_data_recode_audit.csv"), row.names = FALSE)
write.csv(table1, file.path(output_dir, "table1_weighted_characteristics.csv"), row.names = FALSE)
write.csv(outcome_by_food, file.path(output_dir, "weighted_outcome_by_food_security.csv"), row.names = FALSE)
write.csv(primary_results, file.path(output_dir, "primary_model_results.csv"), row.names = FALSE)
write.csv(overall_results, file.path(output_dir, "food_security_overall_tests.csv"), row.names = FALSE)
write.csv(full_model_results, file.path(output_dir, "full_adjusted_model.csv"), row.names = FALSE)
write.csv(threshold_results, file.path(output_dir, "threshold_sensitivity_results.csv"), row.names = FALSE)
write.csv(vif_results, file.path(output_dir, "weighted_vif_results.csv"), row.names = FALSE)
write.csv(missingness, file.path(output_dir, "missingness_audit.csv"), row.names = FALSE)
write.csv(sample_summary, file.path(output_dir, "sample_summary.csv"), row.names = FALSE)

saveRDS(
  list(
    unadjusted_model = unadjusted_model,
    adjusted_model = adjusted_model,
    design_df = design_df
  ),
  file.path(output_dir, "fitted_models.rds")
)

cat("Analysis completed successfully.\n")
print(sample_summary, row.names = FALSE)
cat("\nPrimary food-security estimates:\n")
print(
  primary_results[, c(
    "food_security",
    "odds_ratio_unadjusted", "conf_low_unadjusted", "conf_high_unadjusted",
    "odds_ratio_adjusted", "conf_low_adjusted", "conf_high_adjusted",
    "p_value_adjusted"
  )],
  row.names = FALSE
)
cat("\nOverall tests:\n")
print(overall_results, row.names = FALSE)
cat("\nLargest weighted VIFs:\n")
print(head(vif_results, 10), row.names = FALSE)
