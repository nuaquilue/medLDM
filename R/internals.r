############ Used in cohort.establish.r and post.fire.r ############
.count.spp <- function(x){
  return(c(sum(x==1), sum(x==2), sum(x==3), sum(x==4), sum(x==5), sum(x==6), sum(x==7),
           sum(x==8), sum(x==9), sum(x==10), sum(x==11), sum(x==12), sum(x==13)))
}

.select.cohort <- function(x){
  if(sum(x)==0)
    return(14)
  else
    return(sample(1:14, 1, replace=F, prob=x))
}


############ Used in afforestation.r ############
.select.spp <- function(x){
  if(sum(x)==0)
    return(NA)
  else
    return(sample(1:13, 1, replace=F, prob=x))
}

.count.spp.narm <- function(x){
  return(c(sum(x==1, na.rm=T), sum(x==2, na.rm=T), sum(x==3, na.rm=T), sum(x==4, na.rm=T), 
           sum(x==5, na.rm=T), sum(x==6, na.rm=T), sum(x==7, na.rm=T), sum(x==8, na.rm=T), 
           sum(x==9, na.rm=T), sum(x==10, na.rm=T), sum(x==11, na.rm=T), sum(x==12, na.rm=T), 
           sum(x==13, na.rm=T)))
}

