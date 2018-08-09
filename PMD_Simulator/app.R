#install.packages("shiny")
#install.packages("simsem")
#install.packages("ggplot2")
#install.packages("plyr")
#install.packages("lavaan")

library(shiny)
library(plyr)
library(simsem)
library(ggplot2)

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


# Function to generate percent_miss vector
generate_percent_miss <- function(input) {
  if (input$n_cheap_methods_ == 1){
  n_cc <-
    floor(input$'budget_' / (input$'price_cheap_1_' + input$'price_exp_'))
  
  # first PMD design
  n_cheap <-
    round_any(n_cc + 1, accuracy = input$'increment_', f = ceiling)
  n_expensive <-
    (input$'budget_' - (n_cheap * (
      input$'price_cheap_1_'
    ))) / input$'price_exp_'
  
  while (tail(n_expensive, 1) >= 20) {
    n_cheap <-
      c(n_cheap,
        round_any((tail(n_cheap, 1) + 1), accuracy = input$'increment_', f = ceiling))
    n_expensive <-
      c(n_expensive, ((input$'budget_' - (
        tail(n_cheap, 1) * (input$'price_cheap_1_')
      )) / input$'price_exp_'))
    n_expensive <- floor(n_expensive)
  }
  
  if (tail(n_expensive, 1) <= 20) {
    n_cheap <- head(n_cheap,-1)
    n_expensive <- head(n_expensive,-1)
  }  
  }
  if (input$n_cheap_methods_ == 2){
    n_cc <-
      floor(input$'budget_' / (input$'price_cheap_1_' + input$'price_cheap_2_' + input$'price_exp_'))
    
    # first PMD design
    n_cheap <-
      round_any(n_cc + 1, accuracy = input$'increment_', f = ceiling)
    n_expensive <-
      (input$'budget_' - (n_cheap * (
        input$'price_cheap_1_' + input$'price_cheap_2_'
      ))) / input$'price_exp_'
    
    while (tail(n_expensive, 1) >= 20) {
      n_cheap <-
        c(n_cheap,
          round_any((tail(n_cheap, 1) + 1), accuracy = input$'increment_', f = ceiling))
      n_expensive <-
        c(n_expensive, ((input$'budget_' - (
          tail(n_cheap, 1) * (input$'price_cheap_1_' + input$'price_cheap_2_')
        )) / input$'price_exp_'))
      n_expensive <- floor(n_expensive)
    }
    
    if (tail(n_expensive, 1) <= 20) {
      n_cheap <- head(n_cheap,-1)
      n_expensive <- head(n_expensive,-1)
    }
  }
  n_cheap<-c(n_cc,n_cheap)
  n_expensive<-c(n_cc,n_expensive)
  data.frame(
    n_cheap = as.integer(n_cheap),
    n_expensive = as.integer(n_expensive),
    percent_miss = 1 - (n_expensive / n_cheap)
  )
}

generate_percent_miss_simu <- function(input) {
  if (input$n_cheap_methods == 1){
  n_cc <-
    floor(input$'budget' / (input$'price_cheap_1' + input$'price_exp'))
  
  # first PMD design
  n_cheap <-
    round_any(n_cc + 1, accuracy = input$'increment', f = ceiling)
  n_expensive <-
    (input$'budget' - (n_cheap * (
      input$'price_cheap_1'
    ))) / input$'price_exp'
  
  while (tail(n_expensive, 1) >= 20) {
    n_cheap <-
      c(n_cheap,
        round_any((tail(n_cheap, 1) + 1), accuracy = input$'increment', f = ceiling))
    n_expensive <-
      c(n_expensive, ((input$'budget' - (
        tail(n_cheap, 1) * (input$'price_cheap_1')
      )) / input$'price_exp'))
    n_expensive <- floor(n_expensive)
  }
  
  if (tail(n_expensive, 1) <= 20) {
    n_cheap <- head(n_cheap,-1)
    n_expensive <- head(n_expensive,-1)
  }
  }
  if (input$n_cheap_methods != 1){
  
  n_cc <-
    floor(input$'budget' / (input$'price_cheap_1' + input$'price_cheap_2' + input$'price_exp'))
  
  # first PMD design
  n_cheap <-
    round_any(n_cc + 1, accuracy = input$'increment', f = ceiling)
  n_expensive <-
    (input$'budget' - (n_cheap * (
      input$'price_cheap_1' + input$'price_cheap_2'
    ))) / input$'price_exp'
  
  while (tail(n_expensive, 1) >= 20) {
    n_cheap <-
      c(n_cheap,
        round_any((tail(n_cheap, 1) + 1), accuracy = input$'increment', f = ceiling))
    n_expensive <-
      c(n_expensive, ((input$'budget' - (
        tail(n_cheap, 1) * (input$'price_cheap_1' + input$'price_cheap_2')
      )) / input$'price_exp'))
    n_expensive <- floor(n_expensive)
  }
  
  if (tail(n_expensive, 1) <= 20) {
    n_cheap <- head(n_cheap,-1)
    n_expensive <- head(n_expensive,-1)
  }
  }
  n_cheap<-c(n_cc,n_cheap)
  n_expensive<-c(n_cc,n_expensive)
  data.frame(
    n_cheap = as.integer(n_cheap),
    n_expensive = as.integer(n_expensive),
    percent_miss = 1 - (n_expensive / n_cheap)
  )
}


sanity_check <- function(input){
  miss_table<-generate_percent_miss_simu(input)
  if (input$n_cheap_methods != 3){
  sample_size_range<-input$nRange
  lower_bound_N<-sample_size_range[1]
  upper_bound_N<-sample_size_range[2]
  miss_table<-miss_table[which(miss_table$n_cheap >=lower_bound_N),]
  miss_table<-miss_table[which(miss_table$n_cheap <=upper_bound_N),]
  }
  if (input$n_cheap_methods == 3){
    sample_size_range_3MM<-input$nRange_3MM_comp
    lower_bound_N_3MM<-sample_size_range_3MM[1]
    upper_bound_N_3MM<-sample_size_range_3MM[2]
    miss_table<-miss_table[which(miss_table$n_cheap >=lower_bound_N_3MM),]
    miss_table<-miss_table[which(miss_table$n_cheap <=upper_bound_N_3MM),]
    
    sample_size_range_2MM<-input$nRange_2MM_comp
    lower_bound_N_2MM<-sample_size_range_2MM[1]
    upper_bound_N_2MM<-sample_size_range_2MM[2]
    miss_table_comparison <- generate_percent_miss_simu_comparison_2MM(input)
    miss_table_comparison<-miss_table_comparison[which(miss_table_comparison$n_cheap >=lower_bound_N_2MM),]
    miss_table_comparison<-miss_table_comparison[which(miss_table_comparison$n_cheap <=upper_bound_N_2MM),]
  }

  
  if (input$n_cheap_methods != 3){
  if(
  abs(input$reg_weight) <= 1 &&
  abs(input$loading_outcome) <= 1 &&
  abs(input$loading_expensive) <= 1 &&
  input$loading_cheap1^2 + input$bias_cheap1^2 <= 1
  ) {
    text_output<-paste("Models to simulate:", nrow(miss_table))
    } else{
      if(abs(input$reg_weight) > 1){
        text_output<-"Model is miss-specified: Please adjust the regression weight. range= -1 to 1"
      }
      if(abs(input$loading_outcome) > 1){
        text_output<-"Model is miss-specified: Please adjust the loading of the outcome factor. range= -1 to 1"
      }
      if(abs(input$loading_expensive) > 1){
        text_output<-"Model is miss-specified: Please adjust the loading of the expensive method. range= -1 to 1"
      }
      if((input$loading_cheap1^2 + input$bias_cheap1^2) > 1){
        text_output<-"Model is miss-specified: Please adjust the loadings of the cheap method. Squared loadings of bias and construct should be less or equal to 1"
      }
    }
  }
  if (input$n_cheap_methods == 3){
    if(
      abs(input$bias_cor) <= 1 && 
      abs(input$reg_weight) <= 1 &&
      abs(input$loading_outcome) <= 1 &&
      abs(input$loading_expensive) <= 1 &&
      (input$loading_cheap1^2 + input$bias_cheap1^2 <= 1) &&
      (input$loading_cheap2^2 + input$bias_cheap2^2 <= 1)
    ) {
      text_output<-paste0("2-MM Models to simulate: ", nrow(miss_table_comparison),
                          "\n 3-MM Models to simulate: ", nrow(miss_table),
                          "\n Total Number of Models: ", nrow(miss_table_comparison)+ nrow(miss_table))
      } else{
        if(abs(input$bias_cor) > 1){
          text_output<-"Model is miss-specified: Please adjust the correlation of the bias factors. range= -1 to 1"
        }
        if(abs(input$reg_weight) > 1){
        text_output<-"Model is miss-specified: Please adjust the regression weight. range= -1 to 1"
      }
        if(abs(input$loading_outcome) > 1){
          text_output<-"Model is miss-specified: Please adjust the loading for the outcome factor. range= -1 to 1"
        }
        if(abs(input$loading_expensive) > 1){
          text_output<-"Model is miss-specified: Please adjust the loading of the expensive method. range= -1 to 1"
        }
        if((input$loading_cheap1^2 + input$bias_cheap1^2) > 1){
          text_output<-"Model is miss-specified: Please adjust the loadings of the first cheap method. Squared loadings of bias and construct should be less or equal to 1"
        }
        if((input$loading_cheap2^2 + input$bias_cheap2^2) > 1){
          text_output<-"Model is miss-specified: Please adjust the loadings of the second cheap method. Squared loadings of bias and construct should be less or equal to 1"
        }
      }
  }
  if(input$price_cheap_1 < 0 ){
    text_output<-"Cost for Cheap Method must be greater than 0"
  }
  if(input$price_cheap_2 < 0 ){
    text_output<-"Cost for additional Cheap Method must be greater than 0"
  }
  if(input$price_exp < 0 ){
    text_output<-"Cost for Expensive Method must be greater than 0"
  }
  text_output
}

get_gen_model <- function(input) {
  if (input$n_cheap_methods == 1) {
    gen_model <- paste0(
          "construct =~ ",
          input$loading_cheap1,
          "*c11 + ",
          input$loading_cheap1,
          "*c12 + ",
          input$loading_expensive,
          "*e1 + ",
          input$loading_expensive,
          "*e2\n",
          "bias_c1 =~ ",
          input$bias_cheap1,
          "*c11 + ",
          input$bias_cheap1,
          "*c12\n",
          "outcome =~ ",
          input$loading_outcome,
          "*o1 + ",
          input$loading_outcome,
          "*o2 + ",
          input$loading_outcome,
          "*o3 + ",
          input$loading_outcome,
          "*o4\n",
          "construct ~~ 1*construct\n",
          "bias_c1 ~~ 1*bias_c1\n",
          "outcome ~~ 1*outcome\n",
          "construct ~~ 0*bias_c1\n",
          "outcome ~~ 0*bias_c1\n",
          "outcome ~~ (",
          input$reg_weight,
          ")*construct\n",
          "c11 ~~ ",
          1 - (input$loading_cheap1 ^ 2 + input$bias_cheap1 ^ 2),
          "*c11\n",
          "c12 ~~ ",
          1 - (input$loading_cheap1 ^ 2 + input$bias_cheap1 ^ 2),
          "*c12\n",
          "e1 ~~ ",
          1 - (input$loading_expensive ^ 2),
          "*e1\n",
          "e2 ~~ ",
          1 - (input$loading_expensive ^ 2),
          "*e2\n",
          "o1 ~~ ",
          1 - (input$loading_outcome ^ 2),
          "*o1\n",
          "o2 ~~ ",
          1 - (input$loading_outcome ^ 2),
          "*o2\n",
          "o3 ~~ ",
          1 - (input$loading_outcome ^ 2),
          "*o3\n",
          "o4 ~~ ",
          1 - (input$loading_outcome ^ 2),
          "*o4\n"
        )
  }
  if (input$n_cheap_methods != 1) {
    gen_model <- paste0(
          "construct =~ ",
          input$loading_cheap1,
          "*c11 + ",
          input$loading_cheap1,
          "*c12 + ",
          input$loading_cheap2,
          "*c21 + ",
          input$loading_cheap2,
          "*c22 + ",
          input$loading_expensive,
          "*e1 + ",
          input$loading_expensive,
          "*e2\n",
          "bias_c1 =~ ",
          input$bias_cheap1,
          "*c11 + ",
          input$bias_cheap1,
          "*c12\n",
          "bias_c2 =~ ",
          input$bias_cheap2,
          "*c21 + ",
          input$bias_cheap2,
          "*c22\n",
          "outcome =~ ",
          input$loading_outcome,
          "*o1 + ",
          input$loading_outcome,
          "*o2 + ",
          input$loading_outcome,
          "*o3 + ",
          input$loading_outcome,
          "*o4\n",
          "construct ~~ 1*construct\n",
          "bias_c1 ~~ 1*bias_c1\n",
          "bias_c2 ~~ 1*bias_c2\n",
          "outcome ~~ 1*outcome\n",
          "construct ~~ 0*bias_c1\n",
          "construct ~~ 0*bias_c2\n",
          "outcome ~~ 0*bias_c1\n",
          "outcome ~~ 0*bias_c2\n",
          "outcome ~~ (",
          input$reg_weight,
          ")*construct\n",
          "bias_c1 ~~ ",
          input$bias_cor,
          "*bias_c2\n",
          "c11 ~~ ",
          1 - (input$loading_cheap1 ^ 2 + input$bias_cheap1 ^ 2),
          "*c11\n",
          "c12 ~~ ",
          1 - (input$loading_cheap1 ^ 2 + input$bias_cheap1 ^ 2),
          "*c12\n",
          "c21 ~~ ",
          1 - (input$loading_cheap2 ^ 2 + input$bias_cheap2 ^ 2),
          "*c21\n",
          "c22 ~~ ",
          1 - (input$loading_cheap2 ^ 2 + input$bias_cheap2 ^ 2),
          "*c22\n",
          "e1 ~~ ",
          1 - (input$loading_expensive ^ 2),
          "*e1\n",
          "e2 ~~ ",
          1 - (input$loading_expensive ^ 2),
          "*e2\n",
          "o1 ~~ ",
          1 - (input$loading_outcome ^ 2),
          "*o1\n",
          "o2 ~~ ",
          1 - (input$loading_outcome ^ 2),
          "*o2\n",
          "o3 ~~ ",
          1 - (input$loading_outcome ^ 2),
          "*o3\n",
          "o4 ~~ ",
          1 - (input$loading_outcome ^ 2),
          "*o4\n"
        )
  }
  gen_model
}

get_simu <- function(input) {
  if (input$n_cheap_methods == 1) {
    withProgress(message = "Simulation running", value =0,{
    se_list <- c()
    results_simu <- c()
    pm_df<-generate_percent_miss_simu(input)
    sample_size_range<-input$nRange
    lower_bound_N<-sample_size_range[1]
    upper_bound_N<-sample_size_range[2]
    pm_df<-pm_df[which(pm_df$n_cheap >=lower_bound_N),]
    pm_df<-pm_df[which(pm_df$n_cheap <=upper_bound_N),]
    percent_miss <- pm_df$percent_miss
    n_cheap <- pm_df$n_cheap
    n_expensive <- pm_df$n_expensive
    gen_model <- get_gen_model(input)
    analyze_model <- "construct =~ c11 + c12 + e1 + e2
    bias_c1 =~ lam_bias_c1*c11 + lam_bias_c1*c12
    outcome =~ 1*o1 + o2 + o3 + o4
    construct ~~ 0*bias_c1
    outcome ~~ 0*bias_c1
    construct ~~ 1*construct
    bias_c1 ~~ 1*bias_c1
    outcome ~ construct"
    number_of_models<-length(percent_miss)
    for (i in 1:length(percent_miss)) {
      incProgress(1/number_of_models, detail= paste("Simulating Model", i, "of", number_of_models))
      if(percent_miss[i] == 0){
        miss_model <- NULL
      } else{
        miss_model <- miss(twoMethod = list(c(3:4), percent_miss[i]))
      }
      dataset <-
        sim(
          nRep = input$number_of_replications,
          model = analyze_model,
          n = n_cheap[i],
          generate = gen_model,
          miss = miss_model,
          lavaanfun = "lavaan",
          seed = input$seed,
          dataOnly = TRUE,
          #multicore = TRUE,
          auto.fix.first = FALSE,
          int.ov.free = TRUE,
          int.lv.free = FALSE,
          auto.fix.single = TRUE,
          auto.var = TRUE,
          auto.cov.lv.x = TRUE,
          auto.th = TRUE,
          auto.delta = TRUE,
          auto.cov.y = TRUE
        )
      out <-
        lapply(dataset, function(x) {
          lavaan(
            analyze_model,
            x,
            missing = 'fiml',
            auto.fix.first = FALSE,
            int.ov.free = TRUE,
            int.lv.free = FALSE,
            auto.fix.single = TRUE,
            auto.var = TRUE,
            auto.cov.lv.x = TRUE,
            auto.th = TRUE,
            auto.delta = TRUE,
            auto.cov.y = TRUE
          )
        })
      ses <- c()
      for (j in 1:length(out)) {
        se <- round(out[[j]]@ParTable$se[which(out[[j]]@ParTable$op == "~")],5)
        converged <- lavInspect(out[[j]], 'converged')
        proper <- lavInspect(out[[j]], 'post.check')
        single_rep <- data.frame(
          nCheap = n_cheap[i],
          nExpensive = n_expensive[i],
          percentMiss = percent_miss[i],
          se = se,
          converged = converged,
          proper = proper,
          modeltype = "Two-Method Measurement"
        )
        results_simu <- rbind(results_simu, single_rep)
      }
    }
  })
  } 
  if (input$n_cheap_methods != 1) {
    withProgress(message = "Simulation running", value =0,{
    se_list <- c()
    results_simu <- c()
    pm_df<-generate_percent_miss_simu(input)
    sample_size_range<-input$nRange
    lower_bound_N<-sample_size_range[1]
    upper_bound_N<-sample_size_range[2]
    pm_df<-pm_df[which(pm_df$n_cheap >=lower_bound_N),]
    pm_df<-pm_df[which(pm_df$n_cheap <=upper_bound_N),]
    percent_miss <- pm_df$percent_miss
    n_cheap <- pm_df$n_cheap
    n_expensive <- pm_df$n_expensive
    gen_model <- get_gen_model(input)
    analyze_model <- "
    construct =~ c11 + c12 + c21 + c22 + e1 + e2
    bias_c1 =~ lam_bias_c1*c11 + lam_bias_c1*c12
    bias_c2 =~ lam_bias_c2*c21 + lam_bias_c2*c22
    outcome =~ lam_o1*o1 + o2 + o3 + o4
    construct ~~ 0*bias_c1
    construct ~~ 0*bias_c2
    outcome ~~ 0*bias_c1
    outcome ~~ 0*bias_c2
    construct ~~var_construct*construct
    bias_c1 ~~ var_bias_c1 *bias_c1
    bias_c2 ~~ var_bias_c2 *bias_c2
    outcome ~ construct
    lam_o1 == 1
    var_construct == 1
    var_bias_c1 == 1
    var_bias_c2 == 1"
    number_of_models<-length(percent_miss)
    for (i in 1:length(percent_miss)) {
      incProgress(1/number_of_models, detail= paste("Simulating Model", i, "of", number_of_models))
      if(percent_miss[i] == 0){
        miss_model <- NULL
      } else{
        miss_model <- miss(twoMethod = list(c(5:6), percent_miss[i]))
      } 
      dataset <-
        sim(
          nRep = input$number_of_replications,
          model = analyze_model,
          n = n_cheap[i],
          generate = gen_model,
          miss = miss_model,
          lavaanfun = "lavaan",
          seed = input$seed,
          dataOnly = TRUE,
          #multicore = TRUE,
          auto.fix.first = FALSE,
          int.ov.free = TRUE,
          int.lv.free = FALSE,
          auto.fix.single = TRUE,
          auto.var = TRUE,
          auto.cov.lv.x = TRUE,
          auto.th = TRUE,
          auto.delta = TRUE,
          auto.cov.y = TRUE
        )
      out <-
        lapply(dataset, function(x) {
          lavaan(
            analyze_model,
            x,
            missing = 'fiml',
            auto.fix.first = FALSE,
            int.ov.free = TRUE,
            int.lv.free = FALSE,
            auto.fix.single = TRUE,
            auto.var = TRUE,
            auto.cov.lv.x = TRUE,
            auto.th = TRUE,
            auto.delta = TRUE,
            auto.cov.y = TRUE
          )
        })
      ses <- c()
      for (j in 1:length(out)) {
        se <- out[[j]]@ParTable$se[which(out[[j]]@ParTable$op == "~")]
        converged <- lavInspect(out[[j]], 'converged')
        proper <- lavInspect(out[[j]], 'post.check')
        single_rep <- data.frame(
          nCheap = n_cheap[i],
          nExpensive = n_expensive[i],
          percentMiss = percent_miss[i],
          se = se,
          converged = converged,
          proper = proper,
          modeltype = "Three-Method Measurement"
        )
        results_simu <- rbind(results_simu, single_rep)
      }
    }
    })
  }
  results_simu
}

get_mean_se <- function(results_df){
  clean_results <- results_df[results_df[,"converged"]==TRUE,]
  clean_results <- clean_results[clean_results[,"proper"]==TRUE,]
  colnames(clean_results)<-c("nCheap", "nExpensive","percentMiss", "SE", "converged","proper","modeltype")
  summary_table<-summarySE(clean_results, measurevar = "SE", groupvars = "nCheap")
  nExpensive<-as.integer(levels(factor(clean_results$nExpensive)))
  percentMiss<-as.numeric(levels(factor(clean_results$percentMiss)))
  Modeltype<-rep(clean_results$modeltype[1],nrow(summary_table))
  summary_table<-cbind(rev(nExpensive),summary_table,percentMiss,Modeltype)
  colnames(summary_table)<-c("nExpensive","nCheap", "N_rep","mean_SE","sd of SEs", "se_of_SEs","ci for SEs","percentMiss","modeltype")
  summary_table
  } 

plot_se <- function(summary_table, input){

  plot_df<-ggplot(data=summary_table, aes(x = nCheap, y= mean_SE)) +
    geom_point() +
    #geom_point(data = summary_table[summary_table$mean_SE==min(summary_table[,"mean_SE"]),], fill="springgreen2", size=10, shape = 22) +
    geom_line() +
    geom_errorbar(aes(ymin=mean_SE-se_of_SEs, ymax=mean_SE+se_of_SEs), colour = "red", width = 30, size = .9) +
    #geom_text(data = summary_table[1,], aes(label = "nCC"), colour="blue") +
    scale_x_continuous(breaks=summary_table[,"nCheap"]) +
    theme_bw()+
    labs(x = "N for Cheap Measures",
         y = "SE")
  plot_df 
}

get_text_result <- function(summary_table){
  optimal_row<-summary_table[summary_table$mean_SE==min(summary_table$mean_SE),]
  expensive<-optimal_row$nExpensive
  cheap<-optimal_row$nCheap
  paste0("The optimal composition is a sample size of: ", cheap, " for the cheap methods
         and a sample size of: ", expensive, " for the expensive method." )
}

all_functions_combined <- function(input){
  results_df<-get_simu(input)
  mean_df<-get_mean_se(results_df)
  plot_df<-plot_se(mean_df,input)
  text_result<-get_text_result(mean_df)
  results_list <- list("results_table" = results_df, "mean_table" = mean_df, "plot" = plot_df, "text_result"= text_result)
  results_list
}

generate_percent_miss_simu_comparison_2MM <- function(input) {

    n_cc <-
      floor(input$'budget' / (input$'price_cheap_1' + input$'price_exp'))
    
    # first PMD design
    n_cheap <-
      round_any(n_cc + 1, accuracy = input$'increment', f = ceiling)
    n_expensive <-
      (input$'budget' - (n_cheap * (
        input$'price_cheap_1'
      ))) / input$'price_exp'
    
    while (tail(n_expensive, 1) >= 20) {
      n_cheap <-
        c(n_cheap,
          round_any((tail(n_cheap, 1) + 1), accuracy = input$'increment', f = ceiling))
      n_expensive <-
        c(n_expensive, ((input$'budget' - (
          tail(n_cheap, 1) * (input$'price_cheap_1')
        )) / input$'price_exp'))
      n_expensive <- floor(n_expensive)
    }
    
    if (tail(n_expensive, 1) <= 20) {
      n_cheap <- head(n_cheap,-1)
      n_expensive <- head(n_expensive,-1)
    }
  n_cheap<-c(n_cc,n_cheap)
  n_expensive<-c(n_cc,n_expensive)
  data.frame(
    n_cheap = as.integer(n_cheap),
    n_expensive = as.integer(n_expensive),
    percent_miss = 1 - (n_expensive / n_cheap)
  )
}

get_gen_model_comparison_2MM <- function(input) {
    gen_model <- paste0(
      "construct =~ ",
      input$loading_cheap1,
      "*c11 + ",
      input$loading_cheap1,
      "*c12 + ",
      input$loading_expensive,
      "*e1 + ",
      input$loading_expensive,
      "*e2\n",
      "bias_c1 =~ ",
      input$bias_cheap1,
      "*c11 + ",
      input$bias_cheap1,
      "*c12\n",
      "outcome =~ ",
      input$loading_outcome,
      "*o1 + ",
      input$loading_outcome,
      "*o2 + ",
      input$loading_outcome,
      "*o3 + ",
      input$loading_outcome,
      "*o4\n",
      "construct ~~ 1*construct\n",
      "bias_c1 ~~ 1*bias_c1\n",
      "outcome ~~ 1*outcome\n",
      "construct ~~ 0*bias_c1\n",
      "outcome ~~ 0*bias_c1\n",
      "outcome ~~ (",
      input$reg_weight,
      ")*construct\n",
      "c11 ~~ ",
      1 - (input$loading_cheap1 ^ 2 + input$bias_cheap1 ^ 2),
      "*c11\n",
      "c12 ~~ ",
      1 - (input$loading_cheap1 ^ 2 + input$bias_cheap1 ^ 2),
      "*c12\n",
      "e1 ~~ ",
      1 - (input$loading_expensive ^ 2),
      "*e1\n",
      "e2 ~~ ",
      1 - (input$loading_expensive ^ 2),
      "*e2\n",
      "o1 ~~ ",
      1 - (input$loading_outcome ^ 2),
      "*o1\n",
      "o2 ~~ ",
      1 - (input$loading_outcome ^ 2),
      "*o2\n",
      "o3 ~~ ",
      1 - (input$loading_outcome ^ 2),
      "*o3\n",
      "o4 ~~ ",
      1 - (input$loading_outcome ^ 2),
      "*o4\n"
    )
  gen_model
}

get_simu_comparison <- function(input) {
  withProgress(message = "Overall Progress of Simulation.", value =0,{
  incProgress(1/3)
  withProgress(message = "Simulating 2MM models", value =0,{
      se_list_2MM <- c()
      results_simu_2MM <- c()
      pm_df<-generate_percent_miss_simu_comparison_2MM(input)
      sample_size_range<-input$nRange_2MM_comp
      lower_bound_N<-sample_size_range[1]
      upper_bound_N<-sample_size_range[2]
      pm_df<-pm_df[which(pm_df$n_cheap >=lower_bound_N),]
      pm_df<-pm_df[which(pm_df$n_cheap <=upper_bound_N),]
      percent_miss_2MM <- pm_df$percent_miss
      n_cheap_2MM <- pm_df$n_cheap
      n_expensive_2MM <- pm_df$n_expensive
      gen_model_2MM <- get_gen_model_comparison_2MM(input)
      analyze_model_2MM <- "construct =~ c11 + c12 + e1 + e2
      bias_c1 =~ lam_bias_c1*c11 + lam_bias_c1*c12
      outcome =~ 1*o1 + o2 + o3 + o4
      construct ~~ 0*bias_c1
      outcome ~~ 0*bias_c1
      construct ~~ 1*construct
      bias_c1 ~~ 1*bias_c1
      outcome ~ construct"
      number_of_models_2MM<-length(percent_miss_2MM)
      for (i in 1:length(percent_miss_2MM)) {
        incProgress(1/number_of_models_2MM, detail= paste("Simulating Model", i, "of", number_of_models_2MM))
        if(percent_miss_2MM[i] == 0){
          miss_model_2MM <- NULL
        } else{
          miss_model_2MM <- miss(twoMethod = list(c(3:4), percent_miss_2MM[i]))
        }
        dataset <-
          sim(
            nRep = input$number_of_replications,
            model = analyze_model_2MM,
            n = n_cheap_2MM[i],
            generate = gen_model_2MM,
            miss = miss_model_2MM,
            lavaanfun = "lavaan",
            seed = input$seed,
            dataOnly = TRUE,
            #multicore = TRUE,
            auto.fix.first = FALSE,
            int.ov.free = TRUE,
            int.lv.free = FALSE,
            auto.fix.single = TRUE,
            auto.var = TRUE,
            auto.cov.lv.x = TRUE,
            auto.th = TRUE,
            auto.delta = TRUE,
            auto.cov.y = TRUE
          )
        out <-
          lapply(dataset, function(x) {
            lavaan(
              analyze_model_2MM,
              x,
              missing = 'fiml',
              auto.fix.first = FALSE,
              int.ov.free = TRUE,
              int.lv.free = FALSE,
              auto.fix.single = TRUE,
              auto.var = TRUE,
              auto.cov.lv.x = TRUE,
              auto.th = TRUE,
              auto.delta = TRUE,
              auto.cov.y = TRUE
            )
          })
        ses <- c()
        for (j in 1:length(out)) {
          se <- round(out[[j]]@ParTable$se[which(out[[j]]@ParTable$op == "~")],5)
          converged <- lavInspect(out[[j]], 'converged')
          proper <- lavInspect(out[[j]], 'post.check')
          single_rep <- data.frame(
            nCheap = n_cheap_2MM[i],
            nExpensive = n_expensive_2MM[i],
            percentMiss = percent_miss_2MM[i],
            se = se,
            converged = converged,
            proper = proper,
            modeltype = "Two-Method Measurement"
          )
          results_simu_2MM <- rbind(results_simu_2MM, single_rep)
        }
      }
  })
  incProgress(1/2, detail= paste("Almost there..."))
  withProgress(message = "Simulating 3MM models", value =0,{
      se_list <- c()
      results_simu_3MM <- c()
      pm_df<-generate_percent_miss_simu(input)
      sample_size_range<-input$nRange_3MM_comp
      lower_bound_N<-sample_size_range[1]
      upper_bound_N<-sample_size_range[2]
      pm_df<-pm_df[which(pm_df$n_cheap >=lower_bound_N),]
      pm_df<-pm_df[which(pm_df$n_cheap <=upper_bound_N),]
      percent_miss <- pm_df$percent_miss
      n_cheap <- pm_df$n_cheap
      n_expensive <- pm_df$n_expensive
      gen_model <- get_gen_model(input)
      analyze_model <- "
      construct =~ c11 + c12 + c21 + c22 + e1 + e2
      bias_c1 =~ lam_bias_c1*c11 + lam_bias_c1*c12
      bias_c2 =~ lam_bias_c2*c21 + lam_bias_c2*c22
      outcome =~ lam_o1*o1 + o2 + o3 + o4
      construct ~~ 0*bias_c1
      construct ~~ 0*bias_c2
      outcome ~~ 0*bias_c1
      outcome ~~ 0*bias_c2
      construct ~~var_construct*construct
      bias_c1 ~~ var_bias_c1 *bias_c1
      bias_c2 ~~ var_bias_c2 *bias_c2
      outcome ~ construct
      lam_o1 == 1
      var_construct == 1
      var_bias_c1 == 1
      var_bias_c2 == 1"
      number_of_models<-length(percent_miss)
      for (i in 1:length(percent_miss)) {
        incProgress(1/number_of_models, detail= paste("Simulating Model", i, "of", number_of_models))
        if(percent_miss[i] == 0){
          miss_model <- NULL
        } else{
          miss_model <- miss(twoMethod = list(c(5:6), percent_miss[i]))
        } 
        dataset <-
          sim(
            nRep = input$number_of_replications,
            model = analyze_model,
            n = n_cheap[i],
            generate = gen_model,
            miss = miss_model,
            lavaanfun = "lavaan",
            seed = input$seed,
            dataOnly = TRUE,
            #multicore = TRUE,
            auto.fix.first = FALSE,
            int.ov.free = TRUE,
            int.lv.free = FALSE,
            auto.fix.single = TRUE,
            auto.var = TRUE,
            auto.cov.lv.x = TRUE,
            auto.th = TRUE,
            auto.delta = TRUE,
            auto.cov.y = TRUE
          )
        out <-
          lapply(dataset, function(x) {
            lavaan(
              analyze_model,
              x,
              missing = 'fiml',
              auto.fix.first = FALSE,
              int.ov.free = TRUE,
              int.lv.free = FALSE,
              auto.fix.single = TRUE,
              auto.var = TRUE,
              auto.cov.lv.x = TRUE,
              auto.th = TRUE,
              auto.delta = TRUE,
              auto.cov.y = TRUE
            )
          })
        ses <- c()
        for (j in 1:length(out)) {
          se <- out[[j]]@ParTable$se[which(out[[j]]@ParTable$op == "~")]
          converged <- lavInspect(out[[j]], 'converged')
          proper <- lavInspect(out[[j]], 'post.check')
          single_rep <- data.frame(
            nCheap = n_cheap[i],
            nExpensive = n_expensive[i],
            percentMiss = percent_miss[i],
            se = se,
            converged = converged,
            proper = proper,
            modeltype = "Three-Method Measurement"
          )
          results_simu_3MM <- rbind(results_simu_3MM, single_rep)
        }
      }
  })
  })
      results_simu_complete<-rbind(results_simu_2MM,results_simu_3MM)
      results_simu <- list("results_simu_2MM" = results_simu_2MM, "results_simu_3MM" = results_simu_3MM, "results_table" = results_simu_complete)
      results_simu
  }

get_mean_se_comparison <- function(results_dfs){
  results_2MM<-results_dfs$results_simu_2MM
  clean_results_2MM <- results_2MM[results_2MM[,"converged"]==TRUE,]
  clean_results_2MM <- clean_results_2MM[clean_results_2MM[,"proper"]==TRUE,]
  colnames(clean_results_2MM)<-c("nCheap", "nExpensive","percentMiss", "SE", "converged","proper")
  summary_table_2MM<-summarySE(clean_results_2MM, measurevar = "SE", groupvars = "nCheap")
  nExpensive_2MM<-as.integer(levels(factor(clean_results_2MM$nExpensive)))
  percentMiss_2MM<-as.numeric(levels(factor(clean_results_2MM$percentMiss)))
  summary_table_2MM<-cbind(rev(nExpensive_2MM),summary_table_2MM,"Two-Method Measurement",percentMiss_2MM)
  colnames(summary_table_2MM)<-c("nExpensive","nCheap", "N_rep","mean_SE","sd of SEs", "se_of_SEs","ci for SEs","modeltype","percentMiss")
  
  results_3MM<-results_dfs$results_simu_3MM
  clean_results_3MM <- results_3MM[results_3MM[,"converged"]==TRUE,]
  clean_results_3MM <- clean_results_3MM[clean_results_3MM[,"proper"]==TRUE,]
  colnames(clean_results_3MM)<-c("nCheap", "nExpensive","percentMiss", "SE", "converged","proper")
  summary_table_3MM<-summarySE(clean_results_3MM, measurevar = "SE", groupvars = "nCheap")
  nExpensive_3MM<-as.integer(levels(factor(clean_results_3MM$nExpensive)))
  percentMiss_3MM<-as.numeric(levels(factor(clean_results_3MM$percentMiss)))
  summary_table_3MM<-cbind(rev(nExpensive_3MM),summary_table_3MM,"Three-Method Measurement",percentMiss_3MM)
  colnames(summary_table_3MM)<-c("nExpensive","nCheap", "N_rep", "mean_SE","sd of SEs","se_of_SEs","ci for SEs","modeltype","percentMiss")

  summary_table<-rbind(summary_table_2MM,summary_table_3MM)
  summary_table
  } 

plot_se_comparison <- function(summary_table){
  df_plot<-summary_table
  plot<-ggplot(data=df_plot, aes(x = nCheap, y= mean_SE,color=modeltype, shape = modeltype)) +
    geom_point() +
    #geom_point(data = df_plot[df_plot$mean_SE==min(df_plot[,"mean_SE"]),], fill="springgreen2", size=10, shape = 22) +
    geom_line() +
    geom_errorbar(aes(ymin= mean_SE- se_of_SEs, ymax=mean_SE + se_of_SEs), width = 30, size = .9) +
    #geom_text(data = summary_table_2MM[1,], aes(label = "nCC"), colour="blue", hjust= 1.5) +
    #geom_text(data = summary_table_3MM[1,], aes(label = "nCC"), colour="red", hjust= 1.5) +
    scale_x_continuous(breaks=df_plot[,"nCheap"]) +
    theme_bw()+
    labs(x = "N for Cheap Measures",
         y = "SE")
  plot 
}

get_text_result_comparison <- function(summary_table){
  optimal_row<-summary_table[summary_table$mean_SE==min(summary_table$mean_SE),]
  expensive<-optimal_row$nExpensive
  cheap<-optimal_row$nCheap
  model <- optimal_row$modeltype
  paste0("A ",model," model is advised. The optimal composition is a sample size of: ", cheap, " for the cheap methods
         and a sample size of: ", expensive, " for the expensive method." )
}


all_functions_combined_comparison <- function(input){
  results_dfs<-get_simu_comparison(input)
  results_table<-results_dfs$results_table
  summary_table<-get_mean_se_comparison(results_dfs)
  plot_df<-plot_se_comparison(summary_table)
  text_result<-get_text_result_comparison(summary_table)
  results_list <- list("results_table" = results_table , "mean_table" = summary_table, "plot" = plot_df, "text_result"= text_result)
  results_list
}



####### Define UI #######
ui <- navbarPage(
  "PMD Simulator",
  tabPanel(
    "Compute Sample Size Constallations",
    fluidRow(column(
      3,
      wellPanel(
        selectInput(
          inputId = "n_cheap_methods_",
          label = "Number of cheap methods",
          choices = c(1, 2)
        ),
        hr(),
        numericInput(
          inputId = "price_cheap_1_",
          label = "Price of cheap method:",
          value = 7.3,
          min = 0.01,
          step=1
        ),
        conditionalPanel(
          condition = "input.n_cheap_methods_ == 2",
          numericInput(
            inputId = "price_cheap_2_",
            label = "Price of second cheap method:",
            value = 0,
            min = 0,
            step=1
          )
        ),
        hr(),
        numericInput(
          inputId = "price_exp_",
          label = "Price of expensive method:",
          value = 16.78,
          min = 0.01,
          step=1
        ),
        hr(),
        numericInput(
          inputId = "budget_",
          label = "Budget:",
          value = 15050,
          min = 10,
          step=100
        ),
        hr(),
        numericInput(
          inputId = "increment_",
          label = "Increment:",
          value = 100,
          min = 10,
          step=10
        )
      ),
      wellPanel(
        selectInput(
          inputId = "data_type",
          label = "Select data type to download",
          choices = c(
            "Whole table (data.frame)" = "df",
            "Percent missings only (vector)" = "vec"
          )
        ),
        selectInput(
          inputId = "file_type",
          label = "Select file type to download",
          choices = c("RData" = ".RData", "CSV" = ".csv")
        ),
        downloadButton(outputId = "download_missing_data_constallations",
                       label = "Download")
      )),    
      column(9,
             imageOutput("model_pic"),
             tableOutput("PercentMiss"),
             align = "center"
      )
    )),
    tabPanel("Finding the Most Efficient Design",
               fluidRow(column(
                 2,
                 wellPanel(
                   selectInput(
                     inputId = "n_cheap_methods",
                     label = "Choose Model",
                     choices = c("2-MM"= 1,
                                 "3-MM"=2,
                                 "2-MM vs. 3-MM"=3)
                   ),
                   hr(),
                   numericInput(
                     inputId = "price_cheap_1",
                     label = "Price of cheap method:",
                     value = 7.3,
                     min=0.01,
                     step=1
                   ),
                   conditionalPanel(
                     condition = "input.n_cheap_methods != 1",
                     numericInput(
                       inputId = "price_cheap_2",
                       label = "Price of second cheap method:",
                       value = 0,
                       min=0,
                       step=1
                     )
                   ),
                   hr(),
                   numericInput(
                     inputId = "price_exp",
                     label = "Price of expensive method:",
                     value = 16.78,
                     min=0.01,
                     step=1
                   ),
                   hr(),
                   numericInput(
                     inputId = "budget",
                     label = "Budget:",
                     value = 15050,
                     min=10,
                     step=1000
                   ),
                   hr(),
                   numericInput(
                     inputId = "increment",
                     label = "Increment:",
                     value = 100,
                     min=10,
                     step=10
                     
                   ),
                   hr(),
                   conditionalPanel(
                     condition = "input.n_cheap_methods != 3",
                     uiOutput("slider")
                   ),
                 conditionalPanel(
                   condition = "input.n_cheap_methods == 3",
                   uiOutput("slider_2MM_comp"),
                   uiOutput("slider_3MM_comp")
                   ))),
                   column(2,
                           wellPanel(
                 numericInput(
                   inputId = "loading_cheap1",
                   label = "loading first cheap method (c1)",
                   value = 0.5,
                   min = -1,
                   max = 1,
                   step = 0.1
                 ),
                 conditionalPanel(
                   condition = "input.n_cheap_methods != 1",
                   numericInput(
                     inputId = "loading_cheap2",
                     label = "loading second cheap method (c2)",
                     value = 0.5,
                     min = -1,
                     max = 1,
                     step = 0.1
                   )
                 ),
                 numericInput(
                   inputId = "bias_cheap1",
                   label = "bias loading first cheap method (b1)",
                   value = 0.5,
                   min = -1,
                   max = 1,
                   step = 0.1
                 ),
                 conditionalPanel(
                   condition = "input.n_cheap_methods != 1",
                   numericInput(
                     inputId = "bias_cheap2",
                     label = "bias loading second cheap method (b2)",
                     value = 0.5,
                     min = -1,
                     max = 1,
                     step = 0.1
                   )
                 ),
                 numericInput(
                   inputId = "loading_expensive",
                   label = "loading expensive method (e)",
                   value = 0.7,
                   min = -1,
                   max = 1,
                   step = 0.1
                 ),
                 numericInput(
                   inputId = "loading_outcome",
                   label = "loadings for outcome (o)",
                   value = 0.7,
                   min = -1,
                   max = 1,
                   step = 0.1
                 ),
                 numericInput(
                   inputId = "reg_weight",
                   label = "size of regression weight (r1)",
                   value = -0.4,
                   min = -1,
                   max = 1,
                   step = 0.1
                 ),
                 conditionalPanel(
                   condition = "input.n_cheap_methods != 1",
                   numericInput(
                     inputId = "bias_cor",
                     label = "correlation of Bias-Factors (r2)",
                     value = 0.5,
                     min = -1,
                     max = 1,
                     step = 0.1
                   )
                 ),
                 numericInput(
                   inputId = "number_of_replications",
                   label = "Number of replications",
                   value = 10,
                   min = 1
                 ),
                 numericInput(
                   inputId = "seed",
                   label = "Select seed",
                   value = 123456
                 )
               )),
               column(8,
                      imageOutput("model_pic_simu"),
                      verbatimTextOutput("number_of_models"),
                      hr(),
                      conditionalPanel(
                        condition = "!$('html').hasClass('shiny-busy') && input.n_cheap_methods == 1",
                      actionButton("simulate_2MM", label = "Simulate 2MM", width = '50%' ,icon("paper-plane"), 
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                      ),
                      conditionalPanel(
                        condition = "!$('html').hasClass('shiny-busy') && input.n_cheap_methods == 2",
                        actionButton("simulate_3MM", label = "Simulate 3MM", width = '50%' ,icon("paper-plane"), 
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                      ),
                      conditionalPanel(
                        condition = "!$('html').hasClass('shiny-busy') && input.n_cheap_methods == 3",
                        actionButton("simulate_comparison", label = "Simulate 2MM vs. 3MM", width = '50%' ,icon("paper-plane"), 
                                   style="color: #fff; background-color: red; border-color: #2e6da4")),
                      #tableOutput("mean_se"),
                      #tableOutput("table_percentMiss"),
                      plotOutput("se_plot"),
                      textOutput("optimalNumber"),
                      tags$head(tags$style("#optimalNumber{color: red;
                                 font-size: 20px;
                                           font-style: italic;
                                           }"
                         )),
                      hr(),
                      #tableOutput("simu"),
                      conditionalPanel(
                        condition = "!$('html').hasClass('shiny-busy') && (input.simulate_2MM != 0 || input.simulate_3MM != 0 || input.simulate_comparison != 0) && output.simufinished", 
                        selectInput(
                          inputId = "data_type_simu",
                          label = "Select data you want to download",
                          choices = c(
                            "Result Table with each replication" = "results_table",
                            "Mean SEs for each sample size" = "mean_table",
                            "Plot" = "plot"
                          )
                        ),
                        conditionalPanel(
                          condition = "input.data_type_simu != 'plot'",
                          selectInput(
                            inputId = "file_type_simu",
                            label = "Select file type to download",
                            choices = c("RData" = ".RData", "CSV" = ".csv")
                          )),
                        downloadButton(outputId = "download_simu",
                                       label = "Start Download")
                      ),
                      align = "center"))
             ))
    





###### Define server logic#####

server <- function(input, output) {
  
  
  output$slider <- renderUI({
    table<-generate_percent_miss_simu(input)
    min_N<-table[1,"n_cheap"]
    max_N<-table[nrow(table),"n_cheap"]
    sliderInput("nRange", "Range of Sample Size", min=min_N, max=max_N, value=c(0,max_N))
  })
  
  output$slider_2MM_comp <- renderUI({
    table<-generate_percent_miss_simu_comparison_2MM(input)
    min_N<-table[1,"n_cheap"]
    max_N<-table[nrow(table),"n_cheap"]
    sliderInput("nRange_2MM_comp", "Range of Sample Size for 2-MM Design", min=min_N, max=max_N, value=c(0,max_N))
  })
  
  output$slider_3MM_comp <- renderUI({
    table<-generate_percent_miss_simu(input)
    min_N<-table[1,"n_cheap"]
    max_N<-table[nrow(table),"n_cheap"]
    sliderInput("nRange_3MM_comp", "Range of Sample Size for 3-MM Design", min=min_N, max=max_N, value=c(0,max_N))
  })
  

  v <- reactiveValues(data = NULL)
  
  
  output$simufinished <- reactive({!is.null(v$results_list)})
  outputOptions(output, "simufinished", suspendWhenHidden = FALSE)
  
  observeEvent(input$simulate_2MM, {
    v$results_list <- all_functions_combined(input)})
  
  observeEvent(input$simulate_3MM, {
    v$results_list <- all_functions_combined(input)})

  
  observeEvent(input$simulate_comparison, {
    v$results_list <- all_functions_combined_comparison(input)})
  
  output$se_plot <- renderPlot({
    v$results_list$plot
  })
  
  output$table_percentMiss<-renderTable({
    table<-generate_percent_miss_simu(input)
  })
  
  output$mean_se<- renderTable({
    names(results_list) <- c("Sample Size for cheap method", "Amount of Missingness",	"SE", "converged",	"proper")
    v$results_list$mean_table
  }, digits = 10)
  
  
  output$simu <- renderTable({
    v$results_list$results_table
  }, digits = 10)
  
  
  output$PercentMiss <- renderTable({
    percent_miss_df <- generate_percent_miss(input)
    names(percent_miss_df) <- c("Sample Size for cheap method",
                                "Sample Size for expensive method",
                                "Amount of Missingness")
    percent_miss_df
  })
  
  output$number_of_models<-renderText({
    sanity_check(input)
  })
  
  output$optimalNumber<-renderText({
    v$results_list$text_result
  })
  
  output$model_pic_simu <- renderImage({
    if (input$n_cheap_methods == 1) {
      return(list(
        src = "2MM_letters.png",
        contentType = "image/png",
        width="650",
        alt = "2MM"
      ))
    } else if (input$n_cheap_methods != 1 ) {
      return(list(
        src = "3MM_letters.png",
        filetype = "image/jpeg",
        width="650",
        alt = "3MM"
      ))
    }
  }, deleteFile = FALSE)
  
  output$model_pic <- renderImage({
    if (input$n_cheap_methods_ == 1) {
      return(list(
        src = "2MM.png",
        contentType = "image/png",
        width="650",
        alt = "2MM"
      ))
    } else if (input$n_cheap_methods_ == 2 ) {
      return(list(
        src = "3MM.png",
        filetype = "image/jpeg",
        width="650",
        alt = "3MM"
      ))
    }
  }, deleteFile = FALSE)
  
  output$download_simu <- 
    downloadHandler(
      filename = function() {
        filename <- switch(input$data_type_simu,
                           "results_table" = paste0("results_table", input$file_type_simu),
                           "mean_table" = paste0("mean_table", input$file_type_simu),
                           "plot" = "simu_plot.png"
        )
        filename
      },
      content = function(file) {
        results_list <- v$results_list
        to_save <- switch(input$data_type_simu,
                          "results_table" = results_list$results_table,
                          "mean_table" = results_list$mean_table,
                          "plot" = results_list$plot
        )
        
        if (input$data_type_simu != "plot") {
          assign(paste0("simu_", input$data_type_simu), to_save)
          
          switch(input$file_type_simu,
                 ".RData" = save(list = paste0("simu_", input$data_type_simu),
                                 file = file),
                 ".csv" = write.table(to_save,
                                      file = file,
                                      sep = ",",
                                      dec = ".",
                                      row.names = F,
                                      col.names = T
                 )
          )
        } else {
          ggsave(filename = file,
                 plot = to_save)
        }
      }
    )
  
  
  
  output$download_missing_data_constallations <-
    downloadHandler(
      filename = function() {
        paste0("pm_",
               input$data_type,
               "_",
               Sys.Date(),
               input$file_type)
      },
      content = function(file) {
        pm_table <- generate_percent_miss(input)
        
        to_save <-
          switch(input$data_type,
                 "df" = pm_table,
                 "vec" = pm_table$percent_miss)
        
        assign(paste0("pm_", input$data_type), to_save)
        
        switch(
          input$file_type,
          ".RData" = save(list = paste0("pm_", input$data_type),
                          file = file),
          ".csv" = write.table(
            to_save,
            file = file,
            sep = ",",
            dec = ".",
            row.names = F,
            col.names = switch(input$data_type,
                               "vec" = "percent_miss",
                               "df" = T)
          )
        )
      }
    )
}
# Run the application
shinyApp(ui = ui, server = server)
