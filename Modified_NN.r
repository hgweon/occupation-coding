  # The codes below are for automated occupation coding in R.
  # For implementing the codes, the following conditions must be met.
  # Both training and test data are in either data.frame or matrix forms.
  # The first column of a data set must contain numerical occupation codes.
  
  # Modified NN
  f_modified_nn <- function(train_data,test_data)
  {
    X_tr <- as.matrix(train_data[,-1])
    X_ts <- as.matrix(test_data[,-1])
    nr <- nrow(X_tr)
    ns <- nrow(X_ts)
    similarity <- matrix(0,nrow=nrow(test_data), ncol=nrow(train_data))
    max_sim <- numeric(nrow(test_data))
    tr_label <- names(table(train_data[,1]))
    pred_freq <- matrix(0,nrow=nrow(test_data),ncol=length(tr_label))
    for(i in 1:ns)
    {
      ind <- which(X_ts[i,]==1)
      if(length(ind)>0 & sum(X_tr[,ind])>0) 
      {
        for(j in 1:nr)
        {
          which(X_ts[i,]==1)
          which(X_tr[j,]*X_ts[i,]>0)
          numer <- sum(X_tr[j,]*X_ts[i,])
          A <- sqrt(sum(X_tr[j,]))
          B <- sqrt(sum(X_ts[i,]))
          if(A>0 & B>0 ) similarity[i,j] <- numer/(A*B)
        }
        ind <- which(similarity[i,]==max(similarity[i,])) 
        result <- table(train_data[ind,1])
        max_sim[i] <- max(similarity[i,])
        for(j in 1:length(result))
        {
          ind <- which(names(result[j])==tr_label)
          pred_freq[i,ind] <- result[j]
        }
      }
    }
    pred_prob <- matrix(0,nrow=nrow(test_data),ncol=length(tr_label))  
    freq_sum <- apply(pred_freq,1,sum)
    for(i in 1:nrow(test_data))
    {
      if(freq_sum[i]>0) pred_prob[i,] <- pred_freq[i,]/freq_sum[i]
    }
    colnames(pred_freq) <- tr_label
    colnames(pred_prob) <- tr_label
    out <- list(frequency=pred_freq, probability=pred_prob, max_similarity=max_sim)
    return(out)
  }
  