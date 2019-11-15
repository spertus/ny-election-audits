#author: Jacob Spertus and Amanda Glazer
#date: 11/13/19

#functions to run New York audits and BRAVO RLA in a two candidate-election.


########### New York audit functions #############
# function to sample votes from new machines; is used in audit_results()
# inputs:
  # true_counts: a matrix of true vote counts with a column for each candidate and a row for each machine
  # reported_counts: a matrix of reported counts with a column for each candidate and row for each machine, must align with true_counts
  # machine_types: a vector of length nrow(true/reported_counts) that gives the machine type for each row of true/reported_counts
  # escalation_prop: what fraction of machines to sample (3%, 5%, 12%) in NY audit
  # ignore: an optional vector of indices of which machines should not be sampled. This allows for escalations to avoid sampling previously sampled machines.
# outputs:
  # handcount_samples_type: handcounted samples by machine type, a matrix with a row for each machine type and a column for each candidate
  # reported_samples_type: reported samples by machine type, a matrix with a row for each machine type and a column for each candidate
  # positive_discrepancies_type: positive_discrepancies_type, a matrix with a row for each machine type and a column for each candidate
  # negative_discrepancies_type: negative_discrepancies_type, a matrix with a row for each machine type and a column for each candidate
  # prop_machine_discrepancy: prop_machine_discrepancy, proportion of sampled machines that have _any_ discrepancies
  # sampled_machines: vector with indexes of machines that were sampled
sample_machines <- function(true_counts, reported_counts, machine_types, escalation_prop, ignore = NULL){
  unique_machine_types <- unique(machine_types)
  handcount_samples_type <- matrix(0, nrow = length(unique_machine_types), ncol = ncol(true_counts))
  reported_samples_type <- matrix(0, nrow = length(unique_machine_types), ncol = ncol(true_counts))
  positive_discrepancies_type <- matrix(0, nrow = length(unique_machine_types), ncol = ncol(true_counts))
  negative_discrepancies_type <- matrix(0, nrow = length(unique_machine_types), ncol = ncol(true_counts))
  prop_machine_discrepancy <- rep(0, length(unique_machine_types))
  #save and return sample index to avoid duplicating samples
  index_memory <- c()
  for(i in 1:length(unique_machine_types)){
    index <- which(machine_types == unique_machine_types[i])
    if(!is.null(ignore)){
      index <- index[-which(index %in% ignore)]
    }
    sample_size <- ceiling(escalation_prop * length(index))
    machines_sampled <- base::sample(index, size = sample_size, replace = FALSE)
    index_memory <- c(index_memory, machines_sampled)
    #specifiying drop = FALSE prevents R from turning a single row (if one machine is sampled) into a vector
    handcount_samples <- true_counts[machines_sampled,, drop = FALSE]
    reported_samples <- reported_counts[machines_sampled,, drop = FALSE]
    discrepancies <- handcount_samples - reported_samples
    # get number of machines with discrepancies
    num_machine_discrepancy <- nnzero(rowSums(abs(discrepancies)))
    # calculate proportion of sampled machines with discrepancies
    prop_machine_discrepancy[i] <- num_machine_discrepancy/sample_size
    #total handcounts within each machine type
    handcount_samples_type[i,] <- colSums(handcount_samples)
    reported_samples_type[i,] <- colSums(reported_samples)
    #pmax takes the maximum of it's left argument (the discrepancies matrix) and it's right argument, effectively rounding negatives up to 0 or vice versa 
    positive_discrepancies_type[i,] <- colSums(pmax(discrepancies, 0))
    negative_discrepancies_type[i,] <- colSums(pmin(discrepancies, 0))
  }
  list(handcount_samples_type = handcount_samples_type, 
       reported_samples_type = reported_samples_type,
       positive_discrepancies_type = positive_discrepancies_type, 
       negative_discrepancies_type = negative_discrepancies_type,
       prop_machine_discrepancy = prop_machine_discrepancy, 
       sampled_machines = index_memory)
}


#function to implement auditing algorithm given machine level returns from a single contest in a single county. function can only be used if the true counts as would be discovered by a manual tally are known for every machine, i.e. a simulated situation.
# input: 
  # true_counts: a matrix with a row for each machine and a column for each candidate. Reflects the true counts that would be revealed by a manual tabulation. These are sampled.
  # reported_counts: a matrix with a row for each machine and a column for each candidate. These are machine counts that contribute to the original reported outcome. These are compared to sampled true_counts.
  # machine_types: a vector coding the type of each machine. Can be numeric or characters. Should be the same length as the number of rows in true_counts and reported_counts
# output:
  # a string declaring whether the result is confirmed or whether to proceed to a full hand count.
audit_results <- function(true_counts, reported_counts, machine_types){
  unique_machine_types <- unique(machine_types)
  #containers for totals and discrepancies from sampled machines; there is a row for each machine, each column represents a different candidate
  #the law stipulates that the audit escalates if any combination of votes would cause a discrepancy of .1% of the original total. The worst case combinations of votes are the sums of all positive votes and all negative votes (such that discrepancies that go different ways don't cancel each other)
  # Sample 3% of machines
  initial_sample <- sample_machines(true_counts, reported_counts, machine_types, 0.03)
  
  initial_sample_index <- initial_sample$sampled_machines
  
  initial_positive_discrepancies <- colSums(initial_sample$positive_discrepancies_type)
  initial_negative_discrepancies <- colSums(initial_sample$negative_discrepancies_type)
  
  
  #compare total votes from handcounts to total reported votes (summed across machines)
  initial_handcount_totals <- colSums(initial_sample$handcount_samples_type)
  initial_reported_totals <- colSums(initial_sample$reported_samples_type)
  
  proportion_different_positive <- abs((initial_positive_discrepancies) / initial_handcount_totals)
  proportion_different_negative <- abs((initial_negative_discrepancies) / initial_handcount_totals)
  
  #escalate audit if discrepancies would shift total vote share by more than .001 for any candidate (question, proposal, etc) or 10% of machines have a discrepancy
  #note that it is possible that there can be NA entires in proportion_different_positive/negative due to a divide by zero error: there are machines with no votes for a particular candidate. 
  #we deal with this by ignoring such values when taking the maximum
  if(max(c(proportion_different_negative, proportion_different_positive), na.rm = T) > .001 | max(initial_sample$prop_machine_discrepancy) > 0.10){
    second_sample <- sample_machines(true_counts, reported_counts, machine_types, 0.05, ignore = initial_sample_index)
    
    second_sample_index <- second_sample$sampled_machines
    
    second_positive_discrepancies <- colSums(second_sample$positive_discrepancies_type)
    second_negative_discrepancies <- colSums(second_sample$negative_discrepancies_type)
    
    #compare total votes from handcounts to total reported votes (summed across machines)
    second_handcount_totals <- colSums(second_sample$handcount_samples_type)
    second_reported_totals <- colSums(second_sample$reported_samples_type)
    
    #combine the intial sample of 3% of machines with the additional 5% sample
    combined_handcount_totals <- initial_handcount_totals + second_handcount_totals
    combined_reported_totals <- initial_reported_totals + second_reported_totals
    
    combined_positive_discrepancies <- initial_positive_discrepancies + second_positive_discrepancies
    combined_negative_discrepancies <- initial_negative_discrepancies + second_negative_discrepancies
    
    proportion_different_positive <- abs((combined_positive_discrepancies) / combined_handcount_totals)
    proportion_different_negative <- abs((combined_negative_discrepancies) / combined_handcount_totals)
    
    if(max(c(proportion_different_positive, proportion_different_negative), na.rm = T) > .001 | max(second_sample$prop_machine_discrepancy) > 0.10){
      third_sample <- sample_machines(true_counts, reported_counts, machine_types, 0.12, ignore = c(initial_sample_index, second_sample_index))
      third_positive_discrepancies <- colSums(third_sample$positive_discrepancies_type)
      third_negative_discrepancies <- colSums(third_sample$negative_discrepancies_type)
      
      #compare total votes from handcounts to total reported votes (summed across machines)
      third_handcount_totals <- colSums(third_sample$handcount_samples_type)
      third_reported_totals <- colSums(third_sample$reported_samples_type)
      
      #combine the initial sample and escalation with a third escalation 
      combined_handcount_totals <- initial_handcount_totals + second_handcount_totals + third_handcount_totals
      combined_reported_totals <- initial_reported_totals + second_reported_totals + third_reported_totals
      
      combined_positive_discrepancies <- initial_positive_discrepancies + second_positive_discrepancies + third_positive_discrepancies
      combined_negative_discrepancies <- initial_negative_discrepancies + second_negative_discrepancies + third_negative_discrepancies
      
      proportion_different_positive <- abs((combined_positive_discrepancies) / combined_handcount_totals)
      proportion_different_negative <- abs((combined_negative_discrepancies) / combined_handcount_totals)
      
      if(max(c(proportion_different_positive, proportion_different_negative), na.rm = T) > .001 | max(third_sample$prop_machine_discrepancy) > 0.10){
        return("Proceed to full hand count")
      } else {
        return("Audit done; results confirmed")
      }
    } else {
      return("Audit done; results confirmed")
    }
    
  } else {
    return("Audit done; results confirmed")
  }
}



############## BRAVO audit #############
#bravo audit following Lindeman, Stark, and Yates (2012), available at: https://www.usenix.org/system/files/conference/evtwote12/evtwote12-final27.pdf
#currently only works for a two candidate election
#draws are with replacement
bravo_audit <- function(true_counts, reported_counts, machine_types, alpha = .05, M = sum(true_counts)){
    total_true_counts <- colSums(true_counts)
    total_reported_counts <- colSums(reported_counts)
    reported_winner <- which.max(total_reported_counts)
    #proportion of votes for winner
    s_w <- max(total_reported_counts) / sum(reported_counts)
    #total votes cast
    M <- sum(total_true_counts)
    
    #1 is a vote for the winner, 0 is a vote for the loser
    true_ballots <- c(rep(1, total_true_counts[reported_winner]), rep(0, total_true_counts[-reported_winner]))
    
    test_stat <- 1
    m <- 0
    
    continue <- TRUE
    while(continue){
      selection <- sample(1:length(true_ballots), size = 1)
      random_ballot <- true_ballots[selection]
      #multiply test statistic by s_w / .5 if ballot is for the winner, if for loser multiply by (1-s_w)/.5
      test_stat <- test_stat * (s_w / 0.5)^(random_ballot) * ((1 - s_w) / 0.5)^(1 - random_ballot)
      m <- m + 1
      
      if(test_stat > (1 / alpha)){
        continue <- FALSE
        print("Reported outcome is correct")
      } 
      if(m == M){
        continue <- FALSE
        print("All ballots have been counted; result overturned")
      }
    }
}

