  # The codes below are for automated occupation coding in R.
  # For implementing the codes, the following conditions must be met.
  # Both training and test data are in either data.frame or matrix forms.
  # The first column of a data set must contain numerical occupation codes.
  
  #Duplicate method
  f_duplicate <- function(train_data,test_data)
  {
    tr_label <- names(table(train_data[,1]))
    pred_dup  <- matrix(0,nrow=nrow(test_data),ncol=length(tr_label))
    X_tr <- as.matrix(train_data[,-1])
    X_ts <- as.matrix(test_data[,-1])
    y_tr <- train_data[,1]
    nr <- nrow(X_tr)
    ns <- nrow(X_ts)
    for(i in 1:ns)
    {
      candid <- -2 
      ind <- which(X_ts[i,]==1)
      if(length(ind)>0 & sum(X_tr[,ind])>0) 
      {
        for(j in 1:nr)
        {
          if(sum(abs(X_tr[j,]-X_ts[i,]))==0) candid <- c(candid,y_tr[j]) ##
        }
        if(length(table(candid))>1)
        {
          for(j in 2:length(candid))
          {
            jj <- which(tr_label==candid[j])
            pred_dup[i,jj] <- pred_dup[i,jj]+1
          }
        }
      }
    }
    colnames(pred_dup) <- tr_label
    A <- apply(pred_dup,1,sum)
    pred_dup2 <- pred_dup
    for(i in 1:nrow(pred_dup))
    {
      if(A[i]>0) pred_dup2[i,] <- pred_dup2[i,]/A[i]      
      if(A[i]==0) pred_dup2[i,] <- 1/ncol(pred_dup2)      
    }
    out <- list(frequency=pred_dup, probability=pred_dup2)
    return(out)
  }

