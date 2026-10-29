  rm(list = ls())
  library(tidyverse)
  library(psych)
  library(GGally)
  library(corrplot)
  library(car)
  library(factoextra)
  library(broom)
  library(cluster)
  library(estimatr)
  library(modelsummary)
  
  options(scipen = 999)
  
  housing <- read.csv("C:/Users/web/OneDrive/Desktop/DEB/Statistics and machine learning/Wine analysis/data/housing.csv")
  #diagnostics
  head(housing)
  str(housing)
  sum(duplicated(housing))
  colSums(is.na(housing))
  #remove cap at 5000001
  housing1<- housing %>%
    filter(median_house_value < 500001)
  housing1$ocean_proximity <- as.factor(housing1$ocean_proximity)
  summary(housing1)
  describe(housing1)
  #remove missing values
  housing1$total_bedrooms[is.na(housing1$total_bedrooms)] <-
    median(housing1$total_bedrooms, na.rm = TRUE)
 
   housing1 <- housing1 %>%
    mutate(
      rooms_per_household = total_rooms / households,
      bedrooms_per_room = total_bedrooms / total_rooms,
      population_per_household = population / households
    )
  
  
  ggplot(housing1, aes(x = median_house_value)) +
    geom_histogram(bins = 40) +
    labs(title = "Distribution of Median House Value",
         x = "Median House Value",
         y = "Frequency")
  
  summary(housing1$median_house_value)
  #right skewed
  
  housing1$log_house_value <- log(housing1$median_house_value)
  
  ggplot(housing1, aes(x = log_house_value)) +
    geom_histogram(bins = 40) +
    labs(title = "Distribution of House Value",
         x = "House Value",
         y = "Frequency")
  
  summary(housing1$log_house_value)
  #Approximately normal distribution (slight left skew)
  
  ggplot(housing1, aes(x = ocean_proximity, y = log_house_value)) +
    geom_boxplot() +
    labs(title = "House Value by Ocean Proximity",
         x = "Ocean Proximity",
         y = "Median House Value") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplot(housing1, aes(x = median_income, y = log_house_value)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm") +
    labs(title = "Median Income vs House Value")
  
  ggplot(housing1,
         aes(x = factor(housing_median_age),
             y = median_house_value)) +
    geom_boxplot(outlier.alpha = 0.2) +
    labs(
      x = "Housing Median Age",
      y = "Median House Value",
      title = "House Value by Housing Age"
    ) +
    theme(axis.text.x = element_blank())
  
  ggplot(housing1, aes(x = rooms_per_household, y = log_house_value)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm")
  #summary(housing1$rooms_per_household)
  
  #quantile(housing1$rooms_per_household,
           #probs = c(.90, .95, .99, .995))
  
  numeric_vars <- housing1 %>%
    select(where(is.numeric))
  
  cor_matrix <- cor(numeric_vars)
  
  corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)
  
  multiple_model <- lm( log_house_value ~ median_income +
                          housing_median_age +
                          rooms_per_household +
                          bedrooms_per_room +
                          population_per_household +
                          ocean_proximity, data = housing1)
  summary(multiple_model)
  mean(multiple_model$fitted.values)
  mean(housing1$log_house_value)
  myr = var(multiple_model$fitted.values) / var(housing1$log_house_value)
  
  par(mfrow = c(2, 2))
  plot(multiple_model)
  #The model systematically overpredicts or underpredicts certain observations.
  
  ncvTest(multiple_model)
  #heteroscedasticity obseved
  vif(multiple_model)
  
  par(mfrow = c(1, 1))
  which.max(hatvalues(multiple_model))
  
  housing1[18060, ]
  # abnormal population per household (1243)
  summary(housing1$population_per_household)
  housing1[2799, ]
  #bedrooms per room greater than one, can't have this cause bedroom is a subset of rooms
  housing2 <- housing1[-c(18060, 2799), ]
  
  multiple_model1 <- lm(
    log_house_value ~ median_income +
      housing_median_age +
      rooms_per_household +
      bedrooms_per_room +
      ocean_proximity,
    data = housing2
  )
  summary(multiple_model1)
  
  par(mfrow = c(2, 2))
  plot(multiple_model1)
  vif(multiple_model1)
  
  #robust model to control for heteroscedasticity
  
  multiple_model_robust <- lm_robust(
    median_house_value ~ median_income +
      housing_median_age +
      rooms_per_household +
      population_per_household +
      bedrooms_per_room +
      ocean_proximity,
    data = housing2,
    se_type = "HC2"
  )
  summary(multiple_model_robust)
  
  coef_table <- round(as.data.frame(summary(multiple_model_robust)$coefficients), 4)
  View(coef_table)
  
  
  cluster_data <- housing1 %>%
    select(
      longitude,
      latitude,
      median_income,
      housing_median_age,
      rooms_per_household,
      bedrooms_per_room,
      population_per_household
    )
  cluster_scaled <- scale(cluster_data)
  
  set.seed(123)
  
  wss <- sapply(1:10, function(k) {
    kmeans(cluster_scaled, centers = k, nstart = 25)$tot.withinss
  })
  
  plot(
    1:10, wss,
    type = "b",
    xlab = "Number of clusters k",
    ylab = "Total Within Sum of Squares",
    main = "Optimal number of clusters"
  )
  
  k3 <- kmeans(cluster_scaled, centers = 3, nstart = 25)
  
  housing1$cluster <- as.factor(k3$cluster)
  
  housing1 %>%
    group_by(cluster) %>%
    summarise(
      avg_house_value = mean(median_house_value),
      avg_income = mean(median_income),
      avg_age = mean(housing_median_age),
      avg_rooms_per_household = mean(rooms_per_household),
      avg_bedrooms_per_room = mean(bedrooms_per_room),
      avg_population_per_household = mean(population_per_household),
      n = n()
    )
  table(housing1$cluster, housing1$ocean_proximity)
  ggplot(housing1, aes(x = cluster, y = median_house_value)) +
    geom_boxplot() +
    labs(title = "House Value by Cluster",
         x = "Cluster",
         y = "Median House Value")
  
  set.seed(123)
  housing_sample <- housing1 %>%
    sample_n(500)
  
  cluster_sample <- housing_sample %>%
    select(
      longitude,
      latitude,
      housing_median_age,
      median_income,
      rooms_per_household,
      bedrooms_per_room,
      population_per_household
    )
  cluster_sample_scaled <- scale(cluster_sample)
  
  
  # Distance matrix
  dist_matrix <- dist(cluster_sample_scaled)
  view(dist_matrix)
  # Hierarchical clustering
  hc_model <- hclust(dist_matrix, method = "ward.D2")
  
  # Dendrogram
  plot(hc_model, labels = FALSE, main = "Hierarchical Clustering Dendrogram")
  
  # Cut into clusters
  housing_sample$hc_cluster <- as.factor(cutree(hc_model, k = 3))
  
  # Compare hierarchical clusters
  housing_sample %>%
    group_by(hc_cluster) %>%
    summarise(
      avg_house_value = mean(median_house_value),
      avg_income = mean(median_income),
      avg_age = mean(housing_median_age),
      n = n()
    )
  
  # ============================================================
  # 12. FINAL INTERPRETATION TABLES
  # ============================================================
  
  # Regression interpretation table
  regression_table <- tidy(multiple_model_robust)
  regression_table
  
  # Cluster profile table
  cluster_profile <- housing1 %>%
    group_by(cluster) %>%
    summarise(
      avg_house_value = mean(median_house_value),
      avg_income = mean(median_income),
      avg_age = mean(housing_median_age),
      avg_rooms_per_household = mean(rooms_per_household),
      avg_bedrooms_per_room = mean(bedrooms_per_room),
      avg_population_per_household = mean(population_per_household),
      n = n()
    )
  
  cluster_profile
  
  # Quality equivalent: house value by cluster
  housing1 %>%
    group_by(cluster) %>%
    summarise(
      mean_house_value = mean(median_house_value),
      median_house_value = median(median_house_value),
      sd_house_value = sd(median_house_value)
    )
  
  # Ocean proximity by cluster
  table(housing1$cluster, housing1$ocean_proximity)
  
  # ============================================================
  # 13. SAVE OUTPUTS
  # ============================================================
  
  write.csv(housing1, "cleaned_california_housing.csv", row.names = FALSE)
  write.csv(regression_table, "regression_results.csv", row.names = FALSE)
  write.csv(cluster_profile, "cluster_profile.csv", row.names = FALSE)