# HELPER FUNCTIONS for Modeling Blood Donation Workflows
# Author: Ryland Mortlock
# Date created: 3/28/2020

# SIMULATE_HCW_RISK ######################################
# Function to simulate the number of hcw obtaining COVID from donors
# Outputs a vector of the proportion of hcw COVID+ after each day
simulate_hcw_risk <- function(n_hcw,
                              n_donors,
                              n_days,
                              interaction_risk) {
  
  # Initiliaze the health status of healthcare workers
  hc_vec <- rep(FALSE,times = n_hcw)
  
  # Initiliaze result vector to store poportion of COVID+ hcw at end of each day
  result_vec <- rep(0, times = n_days)
  
  # simulate across time
  for (days in 1:n_days){
    
    # simulate each donor
    for (donors in 1:n_donors){
      
      # Choose the screener (S)
      S_choice <- sample(1:n_hcw, 1, replace = TRUE)
      
      # Roll the dice on the interaction
      if (runif(1,0,1) < interaction_risk){
        hc_vec[S_choice] <- TRUE
      }
      
      # Choose the phlebotomist (P)
      P_choice <- sample(1:n_hcw, 1, replace = TRUE)
      
      # Roll the dice on the interaction
      if (runif(1,0,1) < interaction_risk){
        hc_vec[P_choice] <- TRUE
      }

    }
    # Store result for that day
    result_vec[days] <- sum(hc_vec)/length(hc_vec)
  }
  
  invisible(result_vec)
}


# SIMULATE_DONOR_RISK ######################################
# Function to simulate the number of donors obtaining COVID from their visit to blood donation site
# Outputs a vector of the donation site acquired sites each day
simulate_donor_risk <- function(n_hcw,
                                n_donors,
                                n_days,
                                interaction_risk,
                                proportion_positive) {
  
  # Calculate donor interaction risk
  donor_interaction_risk <- interaction_risk/proportion_positive
  
  # Initiliaze the health status of healthcare workers
  hc_vec <- rep(FALSE,times = n_hcw)
  
  # Initiliaze count of new donation center obtained cases
  new_case_count <- 0
  result_vec <- rep(0, times = n_days)
  
  # simulate across time
  for (days in 1:n_days){
 
    # simulate the donor's interactions with screeners and phlebotomists
    for (donors in 1:n_donors){
      
      # Initialize each donor as positive or negative based on the proportion_positive parameter
      donor_status_initial <- FALSE
      if (runif(1,0,1) < proportion_positive){
        donor_status_initial <- TRUE
      }
      donor_status <- donor_status_initial
      
      # Choose the screener (S)
      S_choice <- sample(1:n_hcw, 1, replace = TRUE)
      
      # Roll the dice on the interaction for hcw to obtain virus
      # Only roll it if the donor is positive
      if (donor_status == TRUE){
        if (runif(1,0,1) < donor_interaction_risk){
          hc_vec[S_choice] <- TRUE
        }
      }

      # If the chosen hcw is positive and donor is negative, they have a chance to give it to the donor
      if (hc_vec[S_choice] ==  TRUE && donor_status == FALSE){
        if (runif(1,0,1) < donor_interaction_risk){
          donor_status = TRUE
        }
      }
      
      # Repeat same process for interaction with phlebotomist
      P_choice <- sample(1:n_hcw, 1, replace = TRUE)
      
      # Roll the dice on the interaction for hcw to obtain virus
      # Only roll it if the donor is positive
      if (donor_status == TRUE){
        if (runif(1,0,1) < donor_interaction_risk){
          hc_vec[P_choice] <- TRUE
        }
      }
      
      # If the chosen hcw is positive and donor is negative, they have a chance to give it to the donor
      if (hc_vec[P_choice] ==  TRUE && donor_status == FALSE){
        if (runif(1,0,1) < donor_interaction_risk){
          donor_status = TRUE
        }
      }
      
      # Count increases by 1 if donor went from negative to positive
      if (donor_status_initial == FALSE && donor_status == TRUE){
        new_case_count <- new_case_count + 1
      }
      
    }
    
    # Store cumulative number of new cases
    result_vec[days] <- new_case_count
  }
  
  invisible(result_vec)
}

## Below for case 1 as described in R Markdown
# SIMULATE_HCW_RISK ######################################
# Function to simulate the number of hcw obtaining COVID from donors
# Outputs a vector of the proportion of hcw COVID+ after each day
simulate_hcw_risk_case1 <- function(n_s,
                                    n_p,
                                    n_donors,
                                    n_days,
                                    interaction_risk) {
  
  # Initiliaze the health status of healthcare workers
  S_vec <- rep(FALSE,times = n_s)
  P_vec <- rep(FALSE,times = n_p)
  
  # Initiliaze result vector to store poportion of COVID+ hcw at end of each day
  result_vec <- rep(0, times = n_days)
  
  # simulate across time
  for (days in 1:n_days){
    
    # simulate each donor
    for (donors in 1:n_donors){
      
      # Choose the screener (S)
      S_choice <- sample(1:n_s, 1, replace = TRUE)
      
      # Roll the dice on the interaction
      if (runif(1,0,1) < interaction_risk){
        S_vec[S_choice] <- TRUE
      }
      
      # Choose the phlebotomist (P)
      P_choice <- sample(1:n_p, 1, replace = TRUE)
      
      # Roll the dice on the interaction
      if (runif(1,0,1) < interaction_risk){
        P_vec[P_choice] <- TRUE
      }
      
    }
    # Store result for that day
    result_vec[days] <- (sum(S_vec) + sum(P_vec))/(length(S_vec) + length(P_vec))
  }
  
  invisible(result_vec)
}


# SIMULATE_DONOR_RISK ######################################
# Function to simulate the number of donors obtaining COVID from their visit to blood donation site
# Outputs a vector of the donation site acquired sites each day
simulate_donor_risk_case1 <- function(n_s,
                                      n_p,
                                      n_donors,
                                      n_days,
                                      interaction_risk,
                                      proportion_positive) {
  
  # Calculate donor interaction risk
  donor_interaction_risk <- interaction_risk/proportion_positive
  
  # Initiliaze the health status of healthcare workers
  S_vec <- rep(FALSE,times = n_s)
  P_vec <- rep(FALSE,times = n_p)
  
  # Initiliaze count of new donation center obtained cases
  new_case_count <- 0
  result_vec <- rep(0, times = n_days)
  
  # simulate across time
  for (days in 1:n_days){
    
    # simulate the donor's interactions with screeners and phlebotomists
    for (donors in 1:n_donors){
      
      # Initialize each donor as positive or negative based on the proportion_positive parameter
      donor_status_initial <- FALSE
      if (runif(1,0,1) < proportion_positive){
        donor_status_initial <- TRUE
      }
      donor_status <- donor_status_initial
      
      # Choose the screener (S)
      S_choice <- sample(1:n_s, 1, replace = TRUE)
      
      # Roll the dice on the interaction for hcw to obtain virus
      # Only roll it if the donor is positive
      if (donor_status == TRUE){
        if (runif(1,0,1) < donor_interaction_risk){
          S_vec[S_choice] <- TRUE
        }
      }
      
      # If the chosen hcw is positive and donor is negative, they have a chance to give it to the donor
      if (S_vec[S_choice] ==  TRUE && donor_status == FALSE){
        if (runif(1,0,1) < donor_interaction_risk){
          donor_status = TRUE
        }
      }
      
      # Repeat same process for interaction with phlebotomist
      P_choice <- sample(1:n_p, 1, replace = TRUE)
      
      # Roll the dice on the interaction for hcw to obtain virus
      # Only roll it if the donor is positive
      if (donor_status == TRUE){
        if (runif(1,0,1) < donor_interaction_risk){
          P_vec[P_choice] <- TRUE
        }
      }
      
      # If the chosen hcw is positive and donor is negative, they have a chance to give it to the donor
      if (P_vec[P_choice] ==  TRUE && donor_status == FALSE){
        if (runif(1,0,1) < donor_interaction_risk){
          donor_status = TRUE
        }
      }
      
      # Count increases by 1 if donor went from negative to positive
      if (donor_status_initial == FALSE && donor_status == TRUE){
        new_case_count <- new_case_count + 1
      }
      
    }
    
    # Store cumulative number of new cases
    result_vec[days] <- new_case_count
  }
  
  invisible(result_vec)
}


## Below for case 2 as described in R Markdown
# SIMULATE_HCW_RISK ######################################
# Function to simulate the number of hcw obtaining COVID from donors
# Outputs a vector of the proportion of hcw COVID+ after each day
simulate_hcw_risk_case2 <- function(n_s,
                                    n_p,
                                    n_donors,
                                    n_days,
                                    interaction_risk) {
  
  # Initiliaze the health status of healthcare workers
  S_vec <- rep(FALSE,times = n_s)
  P_vec <- rep(FALSE,times = n_p)
  
  # Initiliaze result vector to store poportion of COVID+ hcw at end of each day
  result_vec <- rep(0, times = n_days)
  
  # simulate across time
  for (days in 1:n_days){
    
    # simulate each donor
    for (donors in 1:n_donors){
      
      # Choose the screener (S) and phlebotomist (P) pair
      my_choice <- sample(1:n_s, 1, replace = TRUE)
      
      # Roll the dice on the interaction
      if (runif(1,0,1) < interaction_risk){
        S_vec[my_choice] <- TRUE
      }
      
      # Roll the dice on the interaction
      if (runif(1,0,1) < interaction_risk){
        P_vec[my_choice] <- TRUE
      }
      
    }
    # Store result for that day
    result_vec[days] <- (sum(S_vec) + sum(P_vec))/(length(S_vec) + length(P_vec))
  }
  
  invisible(result_vec)
}


# SIMULATE_DONOR_RISK ######################################
# Function to simulate the number of donors obtaining COVID from their visit to blood donation site
# Outputs a vector of the donation site acquired sites each day
simulate_donor_risk_case2 <- function(n_s,
                                      n_p,
                                      n_donors,
                                      n_days,
                                      interaction_risk,
                                      proportion_positive) {
  
  # Calculate donor interaction risk
  donor_interaction_risk <- interaction_risk/proportion_positive
  
  # Initiliaze the health status of healthcare workers
  S_vec <- rep(FALSE,times = n_s)
  P_vec <- rep(FALSE,times = n_p)
  
  # Initiliaze count of new donation center obtained cases
  new_case_count <- 0
  result_vec <- rep(0, times = n_days)
  
  # simulate across time
  for (days in 1:n_days){
    
    # simulate the donor's interactions with screeners and phlebotomists
    for (donors in 1:n_donors){
      
      # Initialize each donor as positive or negative based on the proportion_positive parameter
      donor_status_initial <- FALSE
      if (runif(1,0,1) < proportion_positive){
        donor_status_initial <- TRUE
      }
      donor_status <- donor_status_initial
      
      # Choose the screener (S) and phlebotomist (P) pair
      my_choice <- sample(1:n_s, 1, replace = TRUE)
      
      # Roll the dice on the interaction for hcw to obtain virus
      # Only roll it if the donor is positive
      if (donor_status == TRUE){
        if (runif(1,0,1) < donor_interaction_risk){
          S_vec[my_choice] <- TRUE
        }
      }
      
      # If the chosen hcw is positive and donor is negative, they have a chance to give it to the donor
      if (S_vec[my_choice] ==  TRUE && donor_status == FALSE){
        if (runif(1,0,1) < donor_interaction_risk){
          donor_status = TRUE
        }
      }
      
      # Roll the dice on the interaction for hcw to obtain virus
      # Only roll it if the donor is positive
      if (donor_status == TRUE){
        if (runif(1,0,1) < donor_interaction_risk){
          P_vec[my_choice] <- TRUE
        }
      }
      
      # If the chosen hcw is positive and donor is negative, they have a chance to give it to the donor
      if (P_vec[my_choice] ==  TRUE && donor_status == FALSE){
        if (runif(1,0,1) < donor_interaction_risk){
          donor_status = TRUE
        }
      }
      
      # Count increases by 1 if donor went from negative to positive
      if (donor_status_initial == FALSE && donor_status == TRUE){
        new_case_count <- new_case_count + 1
      }
      
    }
    
    # Store cumulative number of new cases
    result_vec[days] <- new_case_count
  }
  
  invisible(result_vec)
}

## Below for Case 3 as described in R Markdown
# SIMULATE_HCW_RISK ######################################
# Function to simulate the number of hcw obtaining COVID from donors
# Outputs a vector of the proportion of hcw COVID+ after each day
simulate_hcw_risk_case3 <- function(n_hcw,
                                    n_donors,
                                    n_days,
                                    interaction_risk) {
  
  # Initiliaze the health status of healthcare workers
  hc_vec <- rep(FALSE,times = n_hcw)
  
  # Initiliaze result vector to store poportion of COVID+ hcw at end of each day
  result_vec <- rep(0, times = n_days)
  
  # simulate across time
  for (days in 1:n_days){
    
    # simulate each donor
    for (donors in 1:n_donors){
      
      # Choose the screener (S)
      S_choice <- sample(1:n_hcw, 1, replace = TRUE)
      
      # Roll the dice on the interaction
      if (runif(1,0,1) < interaction_risk){
        hc_vec[S_choice] <- TRUE
      }
      
      # Choose the phlebotomist (P)
      P_choice <- sample(1:n_hcw, 1, replace = TRUE)
      
      # Roll the dice on the interaction
      if (runif(1,0,1) < interaction_risk){
        hc_vec[P_choice] <- TRUE
      }
      
    }
    # Store result for that day
    result_vec[days] <- sum(hc_vec)/length(hc_vec)
  }
  
  invisible(result_vec)
}


# SIMULATE_DONOR_RISK ######################################
# Function to simulate the number of donors obtaining COVID from their visit to blood donation site
# Outputs a vector of the donation site acquired sites each day
simulate_donor_risk_case3 <- function(n_hcw,
                                      n_donors,
                                      n_days,
                                      interaction_risk,
                                      proportion_positive) {
  
  # Calculate donor interaction risk
  donor_interaction_risk <- interaction_risk/proportion_positive
  
  # Initiliaze the health status of healthcare workers
  hc_vec <- rep(FALSE,times = n_hcw)
  
  # Initiliaze count of new donation center obtained cases
  new_case_count <- 0
  result_vec <- rep(0, times = n_days)
  
  # simulate across time
  for (days in 1:n_days){
    
    # simulate the donor's interactions with screeners and phlebotomists
    for (donors in 1:n_donors){
      
      # Initialize each donor as positive or negative based on the proportion_positive parameter
      donor_status_initial <- FALSE
      if (runif(1,0,1) < proportion_positive){
        donor_status_initial <- TRUE
      }
      donor_status <- donor_status_initial
      
      # Choose the screener (S)
      S_choice <- sample(1:n_hcw, 1, replace = TRUE)
      
      # Roll the dice on the interaction for hcw to obtain virus
      # Only roll it if the donor is positive
      if (donor_status == TRUE){
        if (runif(1,0,1) < donor_interaction_risk){
          hc_vec[S_choice] <- TRUE
        }
      }
      
      # If the chosen hcw is positive and donor is negative, they have a chance to give it to the donor
      if (hc_vec[S_choice] ==  TRUE && donor_status == FALSE){
        if (runif(1,0,1) < donor_interaction_risk){
          donor_status = TRUE
        }
      }
      
      # Repeat same process for interaction with phlebotomist
      P_choice <- sample(1:n_hcw, 1, replace = TRUE)
      
      # Roll the dice on the interaction for hcw to obtain virus
      # Only roll it if the donor is positive
      if (donor_status == TRUE){
        if (runif(1,0,1) < donor_interaction_risk){
          hc_vec[P_choice] <- TRUE
        }
      }
      
      # If the chosen hcw is positive and donor is negative, they have a chance to give it to the donor
      if (hc_vec[P_choice] ==  TRUE && donor_status == FALSE){
        if (runif(1,0,1) < donor_interaction_risk){
          donor_status = TRUE
        }
      }
      
      # Count increases by 1 if donor went from negative to positive
      if (donor_status_initial == FALSE && donor_status == TRUE){
        new_case_count <- new_case_count + 1
      }
      
    }
    
    # Store cumulative number of new cases
    result_vec[days] <- new_case_count
  }
  
  invisible(result_vec)
}

## Below for Case 4 as described in R Markdown
# SIMULATE_HCW_RISK ######################################
# Function to simulate the number of hcw obtaining COVID from donors
# Outputs a vector of the proportion of hcw COVID+ after each day
simulate_hcw_risk_case4 <- function(n_hcw,
                                    n_donors,
                                    n_days,
                                    interaction_risk) {
  
  # Initiliaze the health status of healthcare workers
  hc_vec <- rep(FALSE,times = n_hcw)
  
  # Initiliaze result vector to store poportion of COVID+ hcw at end of each day
  result_vec <- rep(0, times = n_days)
  
  # simulate across time
  for (days in 1:n_days){
    
    # simulate each donor
    for (donors in 1:n_donors){
      
      # Choose the hcw
      hcw_choice <- sample(1:n_hcw, 1, replace = TRUE)
      
      # Roll the dice on the interaction
      if (runif(1,0,1) < interaction_risk){
        hc_vec[hcw_choice] <- TRUE
      }
      
    }
    # Store result for that day
    result_vec[days] <- sum(hc_vec)/length(hc_vec)
  }
  
  invisible(result_vec)
}


# SIMULATE_DONOR_RISK ######################################
# Function to simulate the number of donors obtaining COVID from their visit to blood donation site
# Outputs a vector of the donation site acquired sites each day
simulate_donor_risk_case4 <- function(n_hcw,
                                      n_donors,
                                      n_days,
                                      interaction_risk,
                                      proportion_positive) {
  
  # Calculate donor interaction risk
  donor_interaction_risk <- interaction_risk/proportion_positive
  
  # Initiliaze the health status of healthcare workers
  hc_vec <- rep(FALSE,times = n_hcw)
  
  # Initiliaze count of new donation center obtained cases
  new_case_count <- 0
  result_vec <- rep(0, times = n_days)
  
  # simulate across time
  for (days in 1:n_days){
    
    # simulate the donor's interactions with screeners and phlebotomists
    for (donors in 1:n_donors){
      
      # Initialize each donor as positive or negative based on the proportion_positive parameter
      donor_status_initial <- FALSE
      if (runif(1,0,1) < proportion_positive){
        donor_status_initial <- TRUE
      }
      donor_status <- donor_status_initial
      
      # Choose the screener (S)
      hcw_choice <- sample(1:n_hcw, 1, replace = TRUE)
      
      # Roll the dice on the interaction for hcw to obtain virus
      # Only roll it if the donor is positive
      if (donor_status == TRUE){
        if (runif(1,0,1) < donor_interaction_risk){
          hc_vec[hcw_choice] <- TRUE
        }
      }
      
      # If the chosen hcw is positive and donor is negative, they have a chance to give it to the donor
      if (hc_vec[hcw_choice] ==  TRUE && donor_status == FALSE){
        if (runif(1,0,1) < donor_interaction_risk){
          donor_status = TRUE
        }
      }
      
      # Count increases by 1 if donor went from negative to positive
      if (donor_status_initial == FALSE && donor_status == TRUE){
        new_case_count <- new_case_count + 1
      }
      
    }
    
    # Store cumulative number of new cases
    result_vec[days] <- new_case_count
  }
  
  invisible(result_vec)
}


## Below for Case 5 as described in R Markdown
# SIMULATE_HCW_RISK ######################################
# Function to simulate the number of hcw obtaining COVID from donors
# Outputs a vector of the proportion of hcw COVID+ after each day
simulate_hcw_risk_case5 <- function(n_hcw,
                                    n_donors,
                                    n_days,
                                    interaction_risk,
                                    assist_rate) {
  
  # Initiliaze the health status of healthcare workers
  hc_vec <- rep(FALSE,times = n_hcw)
  
  # Initiliaze result vector to store poportion of COVID+ hcw at end of each day
  result_vec <- rep(0, times = n_days)
  
  # simulate across time
  for (days in 1:n_days){
    
    # simulate each donor
    for (donors in 1:n_donors){
      
      # Choose the hcw
      hcw_choice <- sample(1:n_hcw, 1, replace = TRUE)
      
      # Roll the dice on the interaction
      if (runif(1,0,1) < interaction_risk){
        hc_vec[hcw_choice] <- TRUE
      }
      
      # Possible Assist
      if (runif(1,0,1) < assist_rate){
        
        # Choose a new hcw
        assist_list <- 1:n_hcw
        assist_list <- assist_list[assist_list != hcw_choice]
        assist_choice <- sample(assist_list, 1, replace = TRUE)
        
        # Roll the dice on the interaction
        if (runif(1,0,1) < interaction_risk){
          hc_vec[assist_choice] <- TRUE
        }
      }
      
    }
    # Store result for that day
    result_vec[days] <- sum(hc_vec)/length(hc_vec)
  }
  
  invisible(result_vec)
}


# SIMULATE_DONOR_RISK ######################################
# Function to simulate the number of donors obtaining COVID from their visit to blood donation site
# Outputs a vector of the donation site acquired sites each day
simulate_donor_risk_case5 <- function(n_hcw,
                                      n_donors,
                                      n_days,
                                      interaction_risk,
                                      proportion_positive,
                                      assist_rate) {
  
  # Calculate donor interaction risk
  donor_interaction_risk <- interaction_risk/proportion_positive
  
  # Initiliaze the health status of healthcare workers
  hc_vec <- rep(FALSE,times = n_hcw)
  
  # Initiliaze count of new donation center obtained cases
  new_case_count <- 0
  result_vec <- rep(0, times = n_days)
  
  # simulate across time
  for (days in 1:n_days){
    
    # simulate the donor's interactions with screeners and phlebotomists
    for (donors in 1:n_donors){
      
      # Initialize each donor as positive or negative based on the proportion_positive parameter
      donor_status_initial <- FALSE
      if (runif(1,0,1) < proportion_positive){
        donor_status_initial <- TRUE
      }
      donor_status <- donor_status_initial
      
      # Choose the healthcare worker
      hcw_choice <- sample(1:n_hcw, 1, replace = TRUE)
      
      # Roll the dice on the interaction for hcw to obtain virus
      # Only roll it if the donor is positive
      if (donor_status == TRUE){
        if (runif(1,0,1) < donor_interaction_risk){
          hc_vec[hcw_choice] <- TRUE
        }
      }
      
      # If the chosen hcw is positive and donor is negative, they have a chance to give it to the donor
      if (hc_vec[hcw_choice] ==  TRUE && donor_status == FALSE){
        if (runif(1,0,1) < donor_interaction_risk){
          donor_status = TRUE
        }
      }
      
      # Possible Assist
      if (runif(1,0,1) < assist_rate){
        
        # Choose a new hcw
        assist_list <- 1:n_hcw
        assist_list <- assist_list[assist_list != hcw_choice]
        assist_choice <- sample(assist_list, 1, replace = TRUE)
        
        # Roll the dice on the interaction for assisting hcw to obtain virus
        # Only roll it if the donor is positive
        if (donor_status == TRUE){
          if (runif(1,0,1) < donor_interaction_risk){
            hc_vec[assist_choice] <- TRUE
          }
        }
        
        # If the assisting hcw is positive and donor is negative, they have a chance to give it to the donor
        if (hc_vec[assist_choice] ==  TRUE && donor_status == FALSE){
          if (runif(1,0,1) < donor_interaction_risk){
            donor_status = TRUE
          }
        }
        
      }
    
    # Count increases by 1 if donor went from negative to positive
    if (donor_status_initial == FALSE && donor_status == TRUE){
      new_case_count <- new_case_count + 1
    }
    
  }
  
  # Store cumulative number of new cases
  result_vec[days] <- new_case_count
}

invisible(result_vec)
}