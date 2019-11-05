rm(list=ls())


mkMatrix <- function(rows,cols,vec) {
  m <- matrix(0,nrow=rows,ncol=cols)
  if (length(vec) %% 3 > 0) stop("Incorrect vector length !!!")
  l = length(vec)/3
  for( i in 1:l) {
    #start of 3-tuple
    s = 3*(i-1)+1
    # row, col, number
    r = vec[s]
    c = vec[s+1]
    n = vec[s+2]
    m[r,c] <- n
  }
  return(m)
}


solver<-function(m, task, threshold, p) {
  rows = dim(m)[1]
  cols = dim(m)[2]
  l = length(task)/3
  samples = 1:l
  count = 0
  #print(samples)
  for(i in 1:threshold) {
    if (length(samples) == 0) break
    
    count = count + 1
    
    #tth task coin toss
    tidx = runif(1,1,length(samples))
    t = samples[tidx]
    
    #start of 3-tuple
    s = 3*(t-1)+1
    r = task[s]
    c = task[s+1]
    n = task[s+2]
    m[r,c] = 0
    
    # vert or horiz coin toss
    toss <- rbinom(1,1,p)
    if (toss == 1) {
      #horiz
      # task of size n has n possibilities
      ti = runif(1,min=1,max=n)
      #ith task begins at tb 
      tbs = max(1,c - (n - ti))
      tbe = min(cols,tbs+n-1)
      comp = numeric(length=n)
      slice = m[r,tbs:tbe]
      if (identical(comp, slice)) {
        # fill & remove task t
        m[r,tbs:tbe] = 1
        
        samples = samples[-tidx]
        #print(paste("donehoriz", count, "samples", length(samples)))
      } else {
        # put task back in sampler
        m[r,c] = n
      }
      
    } else {
      
      #vert
      # task of size n has n possibilities
      ti = runif(1,min=1,max=n)
      #ith task begins at tb 
      tbs = max(1,r - (n - ti))
      tbe = min(rows,tbs+n-1)
      comp = numeric(length=n)
      slice = m[tbs:tbe,c]
      if (identical(comp, slice)) {
        # fill & remove task t
        m[tbs:tbe,c] = 1
        
        samples = samples[-tidx]
        #print(paste("donevert", count,"samples", length(samples)))
      } else {
        # put task back in sampler
        m[r,c] = n
      }
      
    }
  }
  return(list("m"=m,"count"=count))
}




repsolver<-function(n, threshold,p) {
  task<-c(1,1,2,2,1,2,2,3,2,2,4,3,3,3,3,4,2,4,5,2,3)
  success = 0
  fail = 0
  successhist = c()
  for(i in 1:n) {
    m<- mkMatrix(5,4,task)
    ret<-solver(m,task,threshold,p)
    if (ret$count < threshold) {
      success = success + 1
      successhist = c(successhist, ret$count)
    }
  }
  fail = n - success
  print(paste(n,"Trials", success, "Success", fail, "Failures"))
  hist(successhist, 50)
  sumr = summary(successhist)
  print(sumr)
  print(paste("Mean", mean(successhist)))
  print(paste("Sigma", sd(successhist)))
  print(paste("Probability:", p))
  
  return(successhist)
}

res1<-repsolver(1000,100,0.5)
Sys.sleep(5)
res2<-repsolver(1000,100,0.3)
Sys.sleep(5)
res3<-repsolver(1000,100,0.8)
