# Updated June 19th, 2018
# Program to generate SGA using IG
# Description: Estimates SGA according to Intergrowth-21st Standards (https://intergrowth21.tghn.org/standards-tools/)
# Original program  sgaIntergrowth was created by Luke C Mullany (lmullany@jhu.edu)
                                                                   
# sgaIntergrowth2.R is a modified version of the original program (sgaIntergrowth)
# Modified by Eric Ohuma (eric.ohuma@lshtm.ac.uk) on 30th March, 2021
                                                                   
                                                                   
#' Function returns a list of Intergrowth-21st standards
#' https://intergrowth21.tghn.org/standards-tools/
#'
#' @import data.table
#' @keywords internal
intergrowth_constants <- function() {
  boys_10 = c(
    0.50,0.51,0.52,0.53,0.54,0.55,0.56,0.57,0.58,0.59,0.60,0.61,0.63,
    0.64,0.65,0.66,0.67,0.69,0.70,0.71,0.72,0.74,0.75,0.77,0.78,0.79,
    0.81,0.82,0.84,0.85,0.87,0.88,0.90,0.92,0.93,0.95,0.97,0.98,1.00,
    1.02,1.03,1.05,1.07,1.09,1.11,1.13,1.15,1.17,1.19,1.21,1.23,1.25,
    1.27, 1.29,1.31,1.34,1.36,1.38,1.41,1.43,1.45,1.48,1.50,1.43,1.47,
    1.51,1.55, 1.59,1.63,1.67,1.71,1.74,1.78,1.82,1.85,1.89,1.92,1.95,
    1.99,2.02,2.05,2.09,2.12,2.15,2.18,2.21,2.24,2.27,2.30,2.33,2.36,
    2.38,2.41,2.44,2.47,2.49,2.52,2.54,2.57,2.59,2.62,2.64,2.67,2.69,
    2.71,2.73,2.76,2.78,2.80,2.82,2.84,2.86,2.88,2.90,2.92,2.94,2.96,
    2.98,2.99,3.01,3.03,3.05,3.06, 3.08,3.09,3.11,3.12,3.14,3.15,3.17,
    3.18,3.20,3.21
  )
  
  
  girls_10 = c(
    0.47,0.48,0.49,0.50,0.51,0.52,0.53,0.54,0.55,0.56,0.57,0.58,0.59,
    0.60,0.61,0.62,0.64,0.65,0.66,0.67,0.68,0.70,0.71,0.72,0.74,0.75,
    0.76,0.78,0.79,0.81,0.82,0.83,0.85,0.86,0.88,0.90,0.91,0.93,0.94,
    0.96,0.98,0.99,1.01,1.03,1.05,1.07,1.08,1.10,1.12,1.14,1.16,1.18,
    1.20,1.22,1.24,1.26,1.28,1.31,1.33,1.35,1.37,1.40,1.42,1.41,1.45,
    1.49,1.53,1.57,1.61,1.65,1.68,1.72,1.75,1.79,1.82,1.86,1.89,1.92,
    1.96,1.99,2.02,2.05,2.08,2.11,2.14,2.17,2.20,2.23,2.25,2.28,2.31,
    2.33,2.36,2.38,2.41,2.43,2.46,2.48,2.50,2.53,2.55,2.57,2.59,2.61,
    2.63,2.65,2.67,2.69,2.71,2.73,2.74,2.76,2.78,2.80,2.81,2.83,2.84,
    2.86,2.87,2.89,2.90,2.91,2.93,2.94,2.95,2.96,2.98,2.99,3.00,3.01,
    3.02,3.03,3.04
  )
  
  
  boys_90 = c(0.82, 0.83, 0.85, 0.87, 0.88, 0.90, 0.92, 0.93, 0.95, 0.97, 0.99, 
              1.01, 1.03, 1.04, 1.06, 1.08, 1.10, 1.13, 1.15, 1.17, 1.19, 1.21, 
              1.23, 1.26, 1.28, 1.30, 1.33, 1.35, 1.37, 1.40, 1.42, 1.45, 1.48, 
              1.50, 1.53, 1.56, 1.58, 1.61, 1.64, 1.67, 1.70, 1.73, 1.76, 1.79, 
              1.82, 1.85, 1.88, 1.92, 1.95, 1.98, 2.02, 2.05, 2.09, 2.12, 2.16, 
              2.19, 2.23, 2.27, 2.31, 2.35, 2.38, 2.42, 2.46, 2.52, 2.56, 2.60, 
              2.64, 2.67, 2.71, 2.75, 2.79, 2.82, 2.86, 2.89, 2.93, 2.96, 3.00, 
              3.03, 3.06, 3.09, 3.13, 3.16, 3.19, 3.22, 3.25, 3.28, 3.31, 3.34, 
              3.37, 3.39, 3.42, 3.45, 3.48, 3.50, 3.53, 3.55, 3.58, 3.61, 3.63, 
              3.65, 3.68, 3.70, 3.72, 3.75, 3.77, 3.79, 3.81, 3.83, 3.86, 3.88, 
              3.90, 3.92, 3.94, 3.95, 3.97, 3.99, 4.01, 4.03, 4.04, 4.06, 4.08, 
              4.09, 4.11, 4.13, 4.14, 4.16, 4.17, 4.19, 4.20, 4.21, 4.23, 4.24, 
              4.25
  )
  
  girls_90 = c(0.77, 0.79, 0.80, 0.82, 0.83, 0.85, 0.87, 0.88, 0.90, 
               0.92, 0.93, 0.95, 0.97, 0.99, 1.01, 1.02, 1.04, 1.06, 
               1.08, 1.10, 1.12, 1.14, 1.16, 1.19, 1.21, 1.23, 1.25, 
               1.27, 1.30, 1.32, 1.34, 1.37, 1.39, 1.42, 1.44, 1.47, 
               1.50, 1.52, 1.55, 1.58, 1.60, 1.63, 1.66, 1.69, 1.72, 
               1.75, 1.78, 1.81, 1.84, 1.87, 1.90, 1.94, 1.97, 2.00, 
               2.04, 2.07, 2.11, 2.14, 2.18, 2.21, 2.25, 2.29, 2.33, 
               2.35, 2.40, 2.44, 2.48, 2.52, 2.56, 2.60, 2.64, 2.67, 
               2.71, 2.75, 2.79, 2.82, 2.86, 2.89, 2.93, 2.96, 2.99, 
               3.03, 3.06, 3.09, 3.12, 3.15, 3.18, 3.21, 3.24, 3.27, 
               3.30, 3.32, 3.35, 3.38, 3.40, 3.43, 3.46, 3.48, 3.51, 
               3.53, 3.55, 3.58, 3.60, 3.62, 3.64, 3.66, 3.68, 3.70, 
               3.72, 3.74, 3.76, 3.78, 3.80, 3.82, 3.84, 3.85, 3.87, 
               3.89, 3.90, 3.92, 3.93, 3.95, 3.96, 3.97, 3.99, 4.00, 
               4.01, 4.03, 4.04, 4.05, 4.06, 4.07, 4.08
  )
  
  return(
    list(
      boys_10 = boys_10*1000,
      girls_10 = girls_10*1000,
      boys_90 = boys_90*1000,
      girls_90 = girls_90*1000
    )
  )
}

#' Function returns a list of cutpoints, given a min, max, and length out
#' @param min starting point for gestational age cutpoint (default = 24)
#' @param max ending point for gestational age cutpoints (default = 43)
#' @param length number of cutpoints (default = 134)
#' @keywords internal
gest_age_cutpoints = function(min=24,max=43,length=134) {
  return(
    seq(min,max,length.out = length)
  )
}

#' Function returns a data table of intergrowth standards
#'
#' This function will return a data.table of intergrowth standards, with
#' column g_start and g_end indicating the start and end of a gestational
#' age range, and columns boys_10, boys_90, girls_10, and girls_90 indicating
#' the weight at which infants falling within that range must meet or exceed
#' in order to avoid being classified as SGA.
#'
#' The function will use intergrowth standards by default; however a custom
#' set of standards can be generate by passing in parameters as below
#' @param growth_constants (default is NULL); this should be a function that
#' returns a names list of weights. The named list must be length=4, and the
#' names must be "boys_10","boys_90","girls_10", and "girls_90". Each element
#' of the list must be vector of weights in grams, and each should correspond
#' to a gestational age cutpoints. The cutpoints must be equally spaced sequence
#' that is generated by passing in min, max, and length.
#' @param min starting point for gestational age cutpoint (default = 24)
#' @param max ending point for gestational age cutpoints (default = 43)
#' @param length number of cutpoints (default = 134)
#' @export
#'
igs <- function(growth_constants = NULL, min=24, max=43, length=134) {
  
  gac = gest_age_cutpoints(min,max,length)
  gst = data.table::data.table(g_start = gac, g_end = data.table::shift(gac,-1))
  gst = gst[1:(nrow(gst)-1)]
  
  if(is.null(growth_constants)) {
    growth_constants = intergrowth_constants
  }
  igc = data.table::data.table()[,growth_constants()]
  
  return(cbind(gst,igc))
}

#' Return SGA given a dataframe of sex, weight, and gestational age at birth
#'
#' Function will return an integer vector of class `factor` holding values
#' AGA (0), SGA 3-10% (1) or SGA <3% (2), given a data frame / tibble/ or
#' data.table that has (at least) columns named 'gestage','weight',and 'sex'.
#'
#' There are some requirements: (1) the 'gestage' column must not be negative, and will
#' be assumed to be expressed in weeks (fractional weeks are allowed); (2) the
#' 'sex' column can only contain values 1,2 or NA. Values equal to 1 will be
#' assumed to indicate males, while values equal to 2 will be assumed to indicate
#' females; (3) all three required variables must be numeric; (4) the 'weight'
#' variable will be assumed to be in grams. Multiply by 1000 if your weights
#' are in kilograms.
#' @param df The dataframe containing the three required variables, as above
#' @param include_outliers (logical, default = FALSE); set to true if individuals
#' with non-missing gestational age value below the minimum value (24) or above
#' the maximum value (default 43) should be assumed to have gestage set to those
#' bounds
#' @param growth_standard This is a growth standard as produced by the `igs()` function
#' The default value is igs() (i.e. without any parameters), which is the intergrowth
#' standard
#' @return factor vector
#' @export
#' @examples
#' estimate_sga_intergrowth(data)
estimate_sga_intergrowth <- function(df, include_outliers = FALSE, growth_standard = igs()) {
  
  tryCatch(runchecks(df))
  
  #make a data.table of weight, sex, gestage,
  d <- data.table::data.table(weight=df[["weight"]],
                              sex = df[["sex"]],
                              gestage=df[["gestage"]]
  )
  
  #update the gestage to the end points if gestage not missing
  # but outside the limit of growstandad
  
  if(include_outliers) {
    d <- fix_outliers(d,growth_standard)
  }
  
  # do an non-equi left outer join on the data (to retain rows)
  d <- growth_standard[d, on = .(g_start<=gestage, g_end>gestage)] # original code had an error - Eric modified
  
  # use case when to create
  d[,sga:=fcase(
    sex==1 & weight>boys_90, 2,
    sex==2 & weight>girls_90, 2,
    sex==1 & weight<boys_10, 1,
    sex==2 & weight<girls_10, 1,
    sex==1 & weight>=boys_10 & weight<=boys_90,0,
    sex==2 & weight>=girls_10 & weight<=girls_90,0
  )]
  
  sga <- factor(d[["sga"]],levels=c("0","1","2"),labels = c("AGA","SGA", "LGA"))
  return(sga)
}

#' Function to change outliers so they equal the endpoints of a growth standard
#' @keywords internal
fix_outliers <- function(df,gs) {
  bounds = c(gs[,min(g_start)],gs[,max(g_end)])
  # update outliers
  df[gestage<bounds[1], gestage:=bounds[1]]
  df[gestage>bounds[2], gestage:=bounds[2]]
  
  return(df)
  
}

#' Function to run checks on the inputted df
#' @keywords internal
runchecks <- function(df) {
  
  # The input df must have three key variables
  if(!isTRUE(
    all.equal(
      intersect(c("weight","sex","gestage"),colnames(df)),
      c("weight","sex","gestage")
    )
  )
  ) {
    stop("df must contain columns 'weight','sex','gestage'",call.=F)
  }
  
  # The input columns must be numeric
  n_num_cols = sum(
    sapply(c("weight","sex","gestage"), function(x) is.numeric(df[[x]]))
  )
  if(n_num_cols!=3) {
    stop("columns 'weight','sex','gestage' must all be numeric",call.=F)
  }
  
  # gestage column cannot be negative
  if(sum(df[["gestage"]]<0, na.rm=T)) {
    stop("column 'gestage' must not contain negative values",call.=F)
  }
  
  # sex can only contain 1,2,or NA
  if(length(union(unique(df[["sex"]]),c(1,2,NA)))!=3) {
    stop("column 'sex' can only contain 1,2, or NA",call.=F)
  }
  
}








