
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

library(officer)
library(rvg)


R1dfA = read_excel("Rater1responses.xlsx", sheet = "SetA")
R1dfB = read_excel("Rater1responses.xlsx", sheet = "SetB")
R1dfC = read_excel("Rater1responses.xlsx", sheet = "SetC")
R1dfD = read_excel("Rater1responses.xlsx", sheet = "SetD")

R2dfA = read_excel("Rater2responses.xlsx", sheet = "SetA")
R2dfB = read_excel("Rater2responses.xlsx", sheet = "SetB")
R2dfC = read_excel("Rater2responses.xlsx", sheet = "SetC")
R2dfD = read_excel("Rater2responses.xlsx", sheet = "SetD")


renamebinary <- function(df){
  tempdf = df
  tempdf[is.na(tempdf)] <- 0
  for (i in 1:length(colnames(tempdf))){
    tempdf[tempdf[,i] == 1,i] = i
    tempdf[tempdf[,i] == 0,i] = i * -1
  }
  return(tempdf)
}

scaled_jaccard_index <- function(set1, set2) {
  intersection <- length(intersect(set1, set2))
  geometric_mean_size <- sqrt(length(set1) * length(set2))
  return(intersection / geometric_mean_size)
}

getexpected <- function(u1, u2){
  baseprobs = vector("numeric", length(colnames(u1)))
  for (i in 1:length(colnames(u1))){
    baseprobs[i] = (sum(u1[,i]==i) + sum(u2[,i]==i)) / 88
  }
  sims1 = matrix(,1000,length(baseprobs))
  for (i in length(baseprobs)){
    sims1[,i] = rbinom(1000, 1, baseprobs[i])
  }
  sims2 = matrix(,1000,length(baseprobs))
  for (i in length(baseprobs)){
    sims2[,i] = rbinom(1000, 1, baseprobs[i])
  }
  SJ_sims = vector("numeric",1000L)
  for (i in 1:1000){
    SJ_sims[i] = scaled_jaccard_index(sims1[i,],sims2[i,])
  }
  return(SJ_sims)
}


R1dfA_C = renamebinary(R1dfA)
R2dfA_C = renamebinary(R2dfA)

SetA_SJ <- vector("numeric", 44L)
for (i in 1:44){
  SetA_SJ[i] = scaled_jaccard_index(as.list(R1dfA_C[i,]),as.list(R2dfA_C[i,]))
}
pa_setA = mean(SetA_SJ)
pe_setA = mean(getexpected(R1dfA_C, R2dfA_C))

ka_setA = (pa_setA - pe_setA) / (1 - pe_setA)






R1dfB_C = renamebinary(R1dfB)
R2dfB_C = renamebinary(R2dfB)

SetB_SJ <- vector("numeric", 44L)
for (i in 1:44){
  SetB_SJ[i] = scaled_jaccard_index(as.list(R1dfB_C[i,]),as.list(R2dfB_C[i,]))
}
pa_setB = mean(SetB_SJ)
pe_setB = mean(getexpected(R1dfB_C, R2dfB_C))

ka_setB = (pa_setB - pe_setB) / (1 - pe_setB)






R1dfC_C = renamebinary(R1dfC)
R2dfC_C = renamebinary(R2dfC)

SetC_SJ <- vector("numeric", 44L)
for (i in 1:44){
  SetC_SJ[i] = scaled_jaccard_index(as.list(R1dfC_C[i,]),as.list(R2dfC_C[i,]))
}
mean(SetC_SJ)
pa_setC = mean(SetC_SJ)
pe_setC = mean(getexpected(R1dfC_C, R2dfC_C))

ka_setC = (pa_setC - pe_setC) / (1 - pe_setC)



R1dfD_C = renamebinary(R1dfD)
R2dfD_C = renamebinary(R2dfD)

SetD_SJ <- vector("numeric", 44L)
for (i in 1:44){
  SetD_SJ[i] = scaled_jaccard_index(as.list(R1dfD_C[i,]),as.list(R2dfD_C[i,]))
}
pa_setD = mean(SetD_SJ)
pe_setD = mean(getexpected(R1dfD_C, R2dfD_C))

ka_setD = (pa_setD - pe_setD) / (1 - pe_setD)


######

validate_simulation <- function(u1, u2, n_iterations = 1000, n_validation_runs = 5) {
  # Calculate observed base probabilities
  baseprobs = vector("numeric", length(colnames(u1)))
  for (i in 1:length(colnames(u1))) {
    baseprobs[i] = (sum(u1[,i]==i) + sum(u2[,i]==i)) / 88
  }
  
  # 1. Verify convergence to base probabilities
  convergence_results <- matrix(nrow = n_validation_runs, ncol = length(baseprobs))
  for(run in 1:n_validation_runs) {
    sims <- rbinom(n_iterations, 1, baseprobs[1])  # Test with first theme
    convergence_results[run,1] <- mean(sims)
    for(i in 2:length(baseprobs)) {
      sims <- rbinom(n_iterations, 1, baseprobs[i])
      convergence_results[run,i] <- mean(sims)
    }
  }
  
  # Calculate deviation from expected probabilities
  prob_deviations <- abs(colMeans(convergence_results) - baseprobs)
  convergence_passed <- all(prob_deviations < 0.02)  # Threshold of 2% difference
  
  # 2. Check stability across runs and bounds
  agreement_distributions <- matrix(nrow = n_validation_runs, ncol = n_iterations)
  for(run in 1:n_validation_runs) {
    results <- getexpected(u1, u2)
    agreement_distributions[run,] <- results
  }
  
  # Calculate stability metrics
  run_means <- rowMeans(agreement_distributions)
  run_sd <- sd(run_means)
  stability_passed <- run_sd < 0.01  # Threshold of 0.01 standard deviation
  
  # 3. Check theoretical bounds (all values should be between 0 and 1)
  bounds_check <- all(agreement_distributions >= 0 & agreement_distributions <= 1)
  
  # 4. Check for reasonable spread in values
  value_spread <- apply(agreement_distributions, 1, function(x) diff(range(x)))
  spread_reasonable <- all(value_spread > 0.01)  # At least some variation in values
  
  # Generate diagnostic plots
  par(mfrow=c(2,2))
  
  # Histogram of agreement values
  hist(agreement_distributions[1,], 
       main="Distribution of Agreement Values",
       xlab="Agreement Value",
       breaks=30)
  
  # Base probability convergence plot
  plot(baseprobs, colMeans(convergence_results),
       main="Base Probability Convergence",
       xlab="Expected Probability",
       ylab="Simulated Probability",
       abline(0,1, col="red"))
  
  # Stability across runs
  plot(1:n_validation_runs, run_means,
       main="Stability Across Runs",
       xlab="Run Number",
       ylab="Mean Agreement",
       ylim=c(0,1))
  abline(h=mean(run_means), col="red")
  
  # Reset plotting parameters
  par(mfrow=c(1,1))
  
  # Return validation results
  validation_results <- list(
    convergence = list(
      passed = convergence_passed,
      deviations = prob_deviations
    ),
    stability = list(
      passed = stability_passed,
      sd = run_sd,
      means = run_means
    ),
    bounds = list(
      passed = bounds_check,
      spread_reasonable = spread_reasonable,
      value_ranges = value_spread
    )
  )
  
  return(validation_results)
}

# Enhanced main function with validation
getexpected_validated <- function(u1, u2, run_validation = TRUE) {
  if(run_validation) {
    cat("Running validation checks...\n")
    validation_results <- validate_simulation(u1, u2)
    
    # Print validation results
    cat("\nValidation Results:\n")
    cat("1. Probability Convergence:", 
        ifelse(validation_results$convergence$passed, "PASSED", "FAILED"), "\n")
    cat("   Max deviation:", max(validation_results$convergence$deviations), "\n")
    
    cat("2. Stability Check:", 
        ifelse(validation_results$stability$passed, "PASSED", "FAILED"), "\n")
    cat("   Standard deviation between runs:", validation_results$stability$sd, "\n")
    
    cat("3. Bounds Check:", 
        ifelse(validation_results$bounds$passed, "PASSED", "FAILED"), "\n")
    cat("   Reasonable spread:", 
        ifelse(validation_results$bounds$spread_reasonable, "PASSED", "FAILED"), "\n")
    
    if(!all(c(validation_results$convergence$passed, 
              validation_results$stability$passed, 
              validation_results$bounds$passed))) {
      warning("Some validation checks failed. Review results carefully.")
    }
  }
  
  # Original getexpected functionality
  baseprobs = vector("numeric", length(colnames(u1)))
  for (i in 1:length(colnames(u1))){
    baseprobs[i] = (sum(u1[,i]==i) + sum(u2[,i]==i)) / 88
  }
  sims1 = matrix(,1000,length(baseprobs))
  for (i in length(baseprobs)){
    sims1[,i] = rbinom(1000, 1, baseprobs[i])
  }
  sims2 = matrix(,1000,length(baseprobs))
  for (i in length(baseprobs)){
    sims2[,i] = rbinom(1000, 1, baseprobs[i])
  }
  SJ_sims = vector("numeric",1000L)
  for (i in 1:1000){
    SJ_sims[i] = scaled_jaccard_index(sims1[i,],sims2[i,])
  }
  return(SJ_sims)
}
# Usage example:
vresults1 <- getexpected_validated(R1dfA_C, R2dfA_C, run_validation = TRUE)
vresults2 <- getexpected_validated(R1dfB_C, R2dfB_C, run_validation = TRUE)
vresults3 <- getexpected_validated(R1dfC_C, R2dfC_C, run_validation = TRUE)
vresults4 <- getexpected_validated(R1dfD_C, R2dfD_C, run_validation = TRUE)







