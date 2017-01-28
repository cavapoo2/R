stock_confusion_matrix <- function(x)
{
    library(ISLR) 
    #dots <- list(...)
   # print(dots)
    #cl = unlist(dots)
    train = (Weekly$Year < 2009) # filter
    dir = Weekly$Direction
    xdata = Weekly[x][,] # convert from data.frame to numeric / character
    df = data.frame(dir,xdata)
    df.2009 = df[!train,]
    glm.fit = glm(dir ~ xdata, data=df, family = binomial,subset = train)
    glm.probs = predict.glm(glm.fit,newdata = df.2009,type = "response")
    print(glm.probs)
    glm.pred = rep("Down",length(glm.probs))
    glm.pred[glm.probs > 0.5] = "Up"
    dir.2009 = Weekly$Direction[!train]
    conf_mat = table(glm.pred,dir.2009)
    return (conf_mat)
}

