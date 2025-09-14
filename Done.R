####Importing libraries -----------
packages <-
  c(
    "dplyr",
    "readxl",
    "rjson",
    "haven",
    "tools",
    "matlib",
    "ggplot2",
    "tidyr",
    "grid",
    "gridExtra",
    "jsonlite"
  )
# Loop through each package and check if it's installed
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    # If it's not installed, install it
    install.packages(pkg)
  }
  # Load the package
  library(pkg, character.only = TRUE)
}


#Taking the path of the file from the user and extracting the extension------
new_file <-
  readline(prompt = "Enter the path of file ,Note that we seprate between each directory by /: ")
new_file <- as.character(new_file)
file_ext <-  file_ext(new_file)


# simple linear regression-----
SLR <- function(df) {
  # reading column names for x and y
  x_col_name = readline(prompt = "Enter the name of X column : ")
  df_x = select(df, c(x_col_name))
  
  y_col_name = readline(prompt = "Enter the name of Y column : ")
  df_y = select(df, y_col_name)
  
  # selecting the x and y column from the data frame then storing it in x and y
  X = df_x[, x_col_name]
  Y = df_y[, y_col_name]
  
  # Calculating degree of freedoms
  n <- length(df_x[, x_col_name])
  df_reg <- 1
  df_error <- n - 2
  df_total <- n - 1
  
  # Calculating sum of x and sum of y
  sum_x <- sum(X)
  sum_Y <- sum(Y)
  
  # Calculating sum of x^2 and sum of y^2
  sum_x_square <- sum(X ^ 2)
  sum_Y_square <- sum(Y ^ 2)
  
  # Calculating sum of xy
  sum_xy <- sum(X * Y)
  
  # Calculating mean of x and mean of y
  X_bar <- sum_x / n
  Y_bar <- sum_Y / n
  
  # Calculating Sxx and Sxy
  Sxx <- sum_x_square - (n * X_bar ^ 2)
  Sxy <- sum_xy - (n * X_bar * Y_bar)
  
  # Calculating β1_hat and β0_hat (hat)
  β1_hat <- Sxy / Sxx
  β0_hat <- Y_bar - (β1_hat * X_bar)
  
  # Calculating sum of squares for error, regression and total
  SST <- sum_Y_square - (n * (Y_bar ^ 2))
  SSR <- β1_hat * Sxy
  SSE <- SST - SSR
  
  # Calculating mean square error and mean square regression
  MSR <- SSR / df_reg
  MSE <- SSE / df_error
  
  # Calculating f statistic
  F_stat <- MSR / MSE
  
  # Calculating f from the distribution table to do so we need to get alpha from the user
  alpha <- readline(prompt = "Enter the significance level : ")
  alpha <- as.numeric(alpha)
  f_tabulated <- qf(1 - alpha, df_reg, df_error)
  
  # Conclusion of the hypothesis
  if (F_stat > f_tabulated) {
    print(
      "F > f_tabulated , ∴ we reject the H0 , in other words there is a relation between the two variables and β1 isn't equal zero"
    )
  } else if (F_stat < f_tabulated) {
    print(
      "F < f_tabulated , ∴ we don't reject the H0, , in other words there is no  relation between the two variables and β1 is equal zero"
    )
  }
  
  # Calculating t-value from the table
  t_value <- qt(1 - (alpha / 2), df_error, lower.tail = TRUE)
  
  # confidence interval for β1
  lower_bound_β1 <- β1_hat - (t_value * sqrt(MSE / Sxx))
  upper_bound_β1 <- β1_hat + (t_value * sqrt(MSE / Sxx))
  β1_interval <- paste(lower_bound_β1, upper_bound_β1, sep = " , ")
  
  # confidence interval for β0
  lower_bound_β0 <-
    β0_hat - (t_value * sqrt(MSE * ((1 / n) + X_bar ^ 2 / Sxx)))
  upper_bound_β0 <-
    β0_hat + (t_value * sqrt(MSE * ((1 / n) + X_bar ^ 2 / Sxx)))
  β0_interval <- paste(lower_bound_β0, upper_bound_β0, sep = " , ")
  
  # confidence interval for the mean response
  given_x <- readline(prompt = "Enter the value of X0 :")
  given_x <- as.numeric(given_x)
  y_hat <- β0_hat + (β1_hat * given_x)
  lower_bound_MR <-
    y_hat - (t_value * sqrt(MSE * ((1 / n) + ((given_x - X_bar) ^ 2
    ) / Sxx)))
  upper_bound_MR <-
    y_hat + (t_value * sqrt(MSE * ((1 / n) + ((given_x - X_bar) ^ 2
    ) / Sxx)))
  MR_interval <- paste(lower_bound_MR, upper_bound_MR, sep = " , ")
  
  # confidence interval for the new observations
  lower_bound_new_obs <-
    y_hat - (t_value * sqrt(MSE * (1 + (1 / n) + ((given_x - X_bar) ^ 2
    ) / Sxx)))
  upper_bound_new_obs <-
    y_hat + (t_value * sqrt(MSE * (1 + (1 / n) + ((given_x - X_bar) ^ 2
    ) / Sxx)))
  new_obs_interval <-
    paste(lower_bound_new_obs, upper_bound_new_obs, sep = " , ")
  
  # Constructing the anova table
  anova_table <- data.frame(
    source_of_var <- c("Between Groups", "Error", "Total"),
    sum_of_squares <- c(SSR, SSE, SST),
    degrees_of_freedom <- c(df_reg, df_error, df_total),
    mean_squares <- c(MSR, MSE, "-"),
    F_value <- c(MSR / MSE, "-", "-")
  )
  
  table_of_checklist <- data.frame(
    Statistic <-
      c(
        "Sxx",
        "Syy",
        "Sxy",
        "β0",
        "β1",
        "SSR",
        "SSE",
        "Sample Size",
        "T-value",
        "Confidence Interval β1",
        "Confidence Interval β0",
        "MR_interval",
        "new_obs_interval"
      ),
    
    value_of_stat <-
      c(
        Sxx,
        SST,
        Sxy,
        β0_hat,
        β1_hat,
        SSR,
        SSE,
        n,
        t_value,
        β1_interval,
        β0_interval,
        MR_interval,
        new_obs_interval
      )
  )
  
  # Anova table
  View(anova_table)
  
  # All required calculations
  View(table_of_checklist)
  
  # Scatter plot with a regression line of the fitted data
  plot(X,
       Y,
       main = "Scatter plot with regression line",
       xlab = x_col_name,
       ylab = y_col_name)
  abline(β0_hat, β1_hat, col = "red", lwd = 3)
}










###Confidence Intervals on the Regression Coefficients ------------
CI_Beta <- function(X, Y, Beta, C, p, n, MSE) {
  #take alpha from the user
  alpha <-
    readline(prompt = "Enter the significance level for the Regression Coefficients interval : ")
  alpha <- as.numeric(alpha)
  
  #take the index of the β in the matrix (ii)from the user
  indexR <- readline(prompt = "Enter the Row index : ")
  indexR <- as.numeric(indexR)
  
  
  #calc t value from the table
  t_table = qt(1 - (alpha / 2), n - p, lower.tail = TRUE)
  
  #select the desired β and  it's opposite C
  βHat = Beta[indexR, 1]
  
  #Cmatrix=C
  CHat = C[indexR, indexR]
  
  #print the data
  lowerBound <- βHat - (t_table * sqrt(MSE * CHat))
  upperBound <- βHat + (t_table * sqrt(MSE * CHat))
  
  β_interval <- paste(lowerBound, upperBound, sep = " , ")
  print(β_interval)
  
}






#CI Estimation of the Mean Response--------
CI_MR <- function(X, Y, p, n, MSE, C) {
  #take alpha from the user
  alpha <-
    readline(prompt = "Enter the significance level for the Mean Response interval : ")
  alpha <- as.numeric(alpha)
  
  #calc t value from the table
  t_table = qt(1 - (alpha / 2), n - p, lower.tail = TRUE)
  
  #take the x node vector from the user
  nrows <- readline(prompt = "Enter the number of rows for X node: ")
  
  
  # Read the values from the console as a single vector
  values <- scan("", what = numeric(), n = as.integer(nrows))
  
  # Convert the vector to a matrix with 1 column
  Xnode <- matrix(values, nrow = as.integer(nrows), ncol = 1)
  
  
  #calc the data which are under the root
  XnodeT <- as.matrix(t(Xnode))
  val_under_sqrt = MSE %*% (XnodeT %*% C %*% Xnode)
  
  YHat = XnodeT %*% Beta
  
  
  #print the data
  lowerBound <- YHat - (t_table * sqrt(val_under_sqrt))
  upperBound <- YHat + (t_table * sqrt(val_under_sqrt))
  MR_interval <- paste(lowerBound, upperBound, sep = " , ")
  print(MR_interval)
  
  
}




#CI Estimation of new observation--------
CI_NO <- function(X, Y, p, n, MSE, C) {
  #take alpha from the user
  alpha <-
    readline(prompt = "Enter the significance level for the Mean Response interval : ")
  alpha <- as.numeric(alpha)
  
  #take the x node vector from the user
  nrows <- readline(prompt = "Enter the number of rows for X nod: ")
  
  
  # Read the values from the console as a single vector
  values <- scan("", what = numeric(), n = as.integer(nrows))
  
  # Convert the vector to a matrix with 1 column
  Xnode <- matrix(values, nrow = as.integer(nrows), ncol = 1)
  
  #calc t value from the table
  t_table = qt(1 - (alpha / 2), n - p, lower.tail = TRUE)
  
  
  #calc the data which are under the root
  XnodeT <- as.matrix(t(Xnode))
  val_under_sqrt = MSE %*% (1 + (XnodeT %*% C %*% Xnode))
  
  YHat = XnodeT %*% Beta
  #print the data
  lowerBound <- YHat - (t_table * sqrt(val_under_sqrt))
  upperBound <- YHat + (t_table * sqrt(val_under_sqrt))
  New_Obs_interval <- paste(lowerBound, upperBound, sep = " , ")
  print(New_Obs_interval)
}





# Main multiple linear regression function--------
MLR <- function(df) {
  # Extracting dimensions of the data frame
  rows_dim <- dim(df)[1]
  col_dim <- dim(df)[2]
  
  # Extracting the target variable (Y) Independent
  print(names(df))
  Y_column_name <-
    readline(prompt = "Please enter the target (Y) column name: ")
  Y <- select(df, c(Y_column_name))
  Y <- Y[, Y_column_name]
  
  
  # Extracting all independent variables (X1..Xn) from the file and storing it in a matrix
  X_vars_names <-
    readline(prompt =
               " Plz enter names of independent variables(regressors):
               \n Warning: split variables by \'-\' ")
  X_vars_names <-
    strsplit(x = X_vars_names, split = '-', fixed = TRUE)[[1]]
  X_matrix <- select(df, c(X_vars_names))
  X = X_matrix[, X_vars_names]
  # Calculating degree of freedom , sample size and number of independent variables
  n <- dim(df)[1]
  k <- dim(X_matrix)[2]
  DF_total <- n - 1
  p <- k + 1
  DF_error <- n - p
  DF_reg <- k
  
  # adding a column of 1s to the matrix of independent variables
  X <- as.matrix(cbind(rep(1, times = rows_dim), X_matrix))
  
  # visualizing the relation between the dependent and independent variables
  par(mfrow = c(2, 1))
  plots <- list()
  for (i in 1:k) {
    q1 = qplot(y = Y, x = X[, i])
    plots[[i]] <- q1
  }
  
  # Calculating Y_bar
  Y_bar <- mean(Y)

  
  # Calculating X transpose
  X_trans <- t(X)
  
  # Calculating X transpose*X
  X_transX <- as.matrix(X_trans %*% X)
  typeof(X_transX)
  # Calculating inverse of ( X transpose*X )
  
  C <- as.matrix(matlib::inv(X_transX))
  # Calculating the Hat function
  hat <- X %*% C %*% X_trans
  
  # Calculating the beta matrix
  Beta <- C %*% X_trans %*% Y
  # Calculating Y_hat
  Y_hat <- X %*% Beta
  
  # Calculating errors
  e <- (Y - Y_hat)
  
  
  #visualization of normality
  ggplot(data = as.data.frame(e), aes(x = e, fill = 'red')) +
    geom_density(alpha = 0.6) +
    theme_bw() + labs(
      title = "Errors Normality test",
      subtitle = "If the dependent variable is normally distributed for a fixed set of predictor values, then the residual values
should be normally distributed with a mean of 0."
    )
  # Calculating sum of squares for error, total and regression
  SSE <- t(e) %*% e
  SST = sum((Y - Y_bar) ^ 2)
  SSR = SST - SSE
  # Calculating MSR and MSE
  MSR <- SSR / DF_reg
  MSE <- as.numeric(SSE / DF_error)
  # Calculating f statistic
  F_stat <- MSR / MSE
  #displaying  the data 
  tabData <- data.frame(
    Statistic <-
      c(
        "DF_total",
        "p",
        "DF_error",
        "DF_reg",
        "Ybar",
        "SSE",
        "SSR",
        "SST",
        "F_stat"
      ),
    
    value_of_stat <-
      c(
        DF_total,
        p,
        DF_error,
        DF_reg,
        Y_bar,
        SSE,
        SSR,
        SST,
        F_stat
      )
  )
  View(tabData)
  # Calculating f from the distribution table to do so we need to get alpha from the user
  alpha <- readline(prompt = "Enter the significance level for F test: ")
  alpha <- as.numeric(alpha)
  f_tabulated <- qf(1 - alpha, DF_reg, DF_error)
  
  # Conclusion of the hypothesis
  if (F_stat > f_tabulated) {
    print(
      "F > f_tabulated , ∴ we reject the H0 , in other words there is at least one of   βi isn't equal zero"
    )
  } else if (F_stat < f_tabulated) {
    print(
      "F < f_tabulated , ∴ we don't reject the H0, , in other words all the values of βi is equal zero"
    )
  }
  # Standardizing errors of our model ,
  # For example it can be used to compare it with other models errors
  standardized_errors = c()
  for (i in 1:n) {
    standardized_errors[i] = e[i] / sqrt(MSE)
    i <- +1
  }
  
  
  # Student error standardization , Studentized residuals
  studentized_errors = c()
  for (i in 1:n) {
    studentized_errors[i] = e[i] / sqrt((1 - hat[i, i]) * MSE)
    i = +1
  }
  plot(studentized_errors)
  
  # ANOVA table of the model
  anova_table <-
    data.frame(
      Source.Of.variation <- c("Between Groups", "Error", "Total"),
      sum_of_squares <- c(SSR, SSE, SST),
      degrees_of_freedom <-
        c(DF_reg, DF_error, DF_total),
      mean_squares <- c(MSR, MSE, "-"),
      F_value <-
        c(MSR / MSE, "-", "-")
    )
  # calculating the variance of Beta
  Beta_variance <- C * MSE
  
  #calculating the R square
  R_sqr <- 1 - (SSE / SST)
  View(anova_table)
  
  
  .GlobalEnv$X <- X
  .GlobalEnv$Y <- Y
  .GlobalEnv$Beta <- Beta
  .GlobalEnv$Bet_variance
  .GlobalEnv$C <- C
  .GlobalEnv$p <- p
  .GlobalEnv$n <- n
  
  CI_Beta(X, Y, Beta, C, p, n, MSE)
  CI_MR(X, Y, p, n, MSE, C)
  CI_NO(X, Y, p, n, MSE, C) 


  print("Program finished!")
}


#Identifying the type of the file then passing it to the SLR function as a df-------
if (file.exists(new_file)) {
  print("File imported successfully ")
  
  if (file_ext == "csv") {
    csv_file <- read.csv(new_file)
    df <- data.frame(csv_file)
    if (ncol(df)==2) {
      SLR(df)
    } else if (ncol(df)>2) {
      MLR(df)
    } else{
      print("Error in excel Sheet")
    }
    
  } else if (file_ext == "xlsx" ||
             file_ext == "xls") {
    # here we have two conditions because a csv file can have either of these two formats
    excel_file <- read_excel(new_file)
    df <- data.frame(excel_file)
    if (ncol(df)==2) {
      SLR(df)
    } else if (ncol(df)>2) {
      MLR(df)
    } else{
      print("Error in excel Sheet")
    }
    
  } else if (file_ext == "json") {
    json_file = rjson::fromJSON(file = new_file)
    df = as.data.frame(json_file)
    if (ncol(df)==2) {
      SLR(df)
    } else if (ncol(df)>2) {
      MLR(df)
    } else{
      print("Error in  Sheet")
    }
    
  } else if (file_ext == "sas7bdat") {
    sas_file <- read_sas(new_file)
    df = data.frame(sas_file)
    if (ncol(df)==2) {
      SLR(df)
    } else if (ncol(df)>2) {
      MLR(df)
    } else{
      print("Error in  Sheet")
    }
    
  } else if (file_ext == "sav") {
    sas_file <- read_spss(new_file)
    df = data.dta(sas_file)
    if (ncol(df)==2) {
      SLR(df)
    } else if (ncol(df)>2) {
      MLR(df)
    } else{
      print("Error in  Sheet")
    }
  } else if (file_ext == "dta") {
    sas_file <- read_sas(new_file)
    df = data.dta(sas_file)
    if (ncol(df)==2) {
      SLR(df)
    } else if (ncol(df)>2) {
      MLR(df)
    } else{
      print("Error in  Sheet")
    }
  }
  
}else{
  print("Invalid file path !")
}




