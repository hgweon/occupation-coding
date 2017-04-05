
  # This function takes a probaility matrix and gives a prediction matrix.
  pred_out <- function(M)
  {
    out <- matrix(0,nrow=nrow(M),ncol=ncol(M))
    for(i in 1:nrow(M))
    {
      B <- which(max(M[i,])==M[i,])
      if(length(B)==1) A <- B
      if(length(B)>1) A <- sample(B,1,replace=FALSE)
      out[i,A] <- 1
    }
    return(out)
  }
  

  #  
  # Below is example codes for semi-automated coding
  tr_label <- names(table(train_data[,1]))
  pred_ts <- matrix(0,nrow=nrow(test_data),ncol=length(tr_label))
  for(i in 1:length(tr_label))
  {
    A <- which(tr_label[i]==test_data[,1])  
    y_new2 <- numeric(nrow(test_data))
    y_new2[A] <- 1
    pred_ts[,i] <- y_new2
  }
  
  # The duplicate method
  out <- f_duplicate(train_data,test_data)
  out_duplicate <- out$probability
  pred_duplicate <- pred_out(out_duplicate)
  mm <- apply(out_duplicate,1,max)
  order_duplicate <- order(mm, runif(length(mm)),decreasing=TRUE)

  # The combined SVM method
  out_combined <- f_combined_svm(train_data,test_data)
  pred_combined <- pred_out(out_combined)
  mm <- apply(out_combined,1,max)
  order_combined <- order(mm, runif(length(mm)),decreasing=TRUE)
  
  # The hybrid method using the duplicate and combined SVM results
  out_hybrid <- f_hybrid(out$frequency,out_combined)
  pred_hybrid <- pred_out(out_hybrid)
  mm <- apply(out_hybrid,1,max)
  order_hybrid <- order(mm, runif(length(mm)),decreasing=TRUE)
  
  # The modified nearest neighbor method
  out <- f_modified_nn(train_data2,test_data2)
  A <- out$frequency
  max_freq <- apply(A,1,sum)
  B <- out$probability
  pred_nn <- pred_out(B)
  max_prob <- apply(B,1,max)
  max_sim <- out$max_similarity
  nn_score <- max_prob*max_sim*(max_freq/(max_freq+0.1))
  order_nn <- order(nn_score, runif(length(nn_score)),decreasing=TRUE)

  # We will store the accuracy of each method
  # at prodcution rate from 0.01 to 1.
  ac_duplicate <- ac_combined <- ac_hybrid <- ac_nn <- numeric(100)
  jj <- (1:100)*0.01
  for(j in 1:100)
  {
    th <- jj[j]
    if(j>1) th1 <- jj[j-1]
    if(j==1) th1 <- 0
    n2 <- round(nrow(test_data)*th)
    n1 <- round(nrow(test_data)*th1)
    
    evl <- apply((pred_duplicate+pred_ts)==2,1,sum)
    ind1 <- order_duplicate[1:n2]
    ac_duplicate[j] <- sum(evl[ind1])/length(ind1)

    evl <- apply((pred_combined+pred_ts)==2,1,sum)
    ind1 <- order_combined[1:n2]
    ac_combined[j] <- sum(evl[ind1])/length(ind1)

    evl <- apply((pred_hybrid+pred_ts)==2,1,sum)
    ind1 <- order_hybrid[1:n2]
    ac_hybrid[j] <- sum(evl[ind1])/length(ind1)

    evl <- apply((pred_nn+pred_ts)==2,1,sum)
    ind1 <- order_nn[1:n2]
    ac_nn[j] <- sum(evl[ind1])/length(ind1)
  }
  