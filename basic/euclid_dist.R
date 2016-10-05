#typical call would be euclid_dist(M,r,"row")
euclid_dist <- function(M,ref,t)
{
  #this is for row wise  
  if (t == "row")
  {
    apply (M,1, function(x) dist(rbind(x,ref)) )
  }
  else
  {
    apply (M,2, function(x) dist(rbind(x,ref)) )
  }
  
}
