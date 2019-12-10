library(jsonlite)

pgf <- function(j,m){
  if(j == m){
    return (m)
  }
  return (1 + (j/m-0.5)/(1-j/m))
}

result <- getURI("https://files.poyashi.me/json/meta.json")
df <- jsonlite::fromJSON(result)

answer <- c("Song title","Coefficient","Std. Error","t value","Pr(>|t|)")

for(i in 1:nrow(df)){
  each <- function(bpi){
    k <- df[i,1]
    s <- bpi
    z <- df[i,3]
    m <- df[i,2]
    S <- pgf(s,m) / pgf(k,m)
    Z <- pgf(z,m) / pgf(k,m)
    if(s < k){
      return (-log(S) / log(Z))
    }
    return (log(S) / log(Z))
  }
  prefix <- function(bpi){
    if(bpi < df[i,1]){
      return (-100)
    }
    return (100)
  }
  names <- c(100,90,80,70,60,50,40,30,20,10,0)
  dt <- c( each(df[i,4]),each(df[i,5]),each(df[i,6]),each(df[i,7]),each(df[i,8]),each(df[i,9]),each(df[i,10]),each(df[i,11]),each(df[i,12]),each(df[i,13]),each(df[i,1]) )
  prefix <- c( prefix(df[i,4]),prefix(df[i,5]),prefix(df[i,6]),prefix(df[i,7]),prefix(df[i,8]),prefix(df[i,9]),prefix(df[i,10]),prefix(df[i,11]),prefix(df[i,12]),prefix(df[i,13]),prefix(df[i,1]) )
  db <- data.frame( names,dt ) 
  
  ans <- nls( names ~ prefix * db$dt^b , start=list( b=1.175 ))
  answer <- rbind(answer,c(df[i,14],summary(ans)$coefficient ) )
  print(i)
 
}


print(answer)
write.table(answer,"result.csv",append=F,sep=",",row.names=F,col.names=F)
print("finish")


