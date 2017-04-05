  # The f_hybrid function combines the duplicate method with a statistical learning approach.
  # Hence, f_hybrid takes two matrices dup_freq_matrix and sl_matrix where
  # dup_freq_matrix: a matrix containing the frequency results from the duplicate method
  # sl_matrix: a matrix containing the probability results of a statistical learning method
  # For dup_freq_matrix one may use an frequency output of the f_duplicate function.
  # For sl_matrix one may use an output of the f_combined_svm function.

  f_hybrid <- function(dup_freq_matrix,sl_matrix)
  {
    comb_M <- matrix(0,nrow=nrow(sl_matrix),ncol=ncol(sl_matrix))
    for(i in 1:nrow(sl_matrix))
    {
      A <- sum(dup_freq_matrix[i,])
      if(A>0) B <- dup_freq_matrix[i,]/A
      if(A>0) comb_M[i,] <- (1/(A+1))*sl_matrix[i,] + (A/(A+1))*B
      if(A==0) comb_M[i,] <- sl_matrix[i,]
    }   
    return(comb_M)
  }