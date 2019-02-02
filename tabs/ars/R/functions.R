##-- upper hull ----
ux <- function(log_fx, log_fxD, x, xj){
  u <- log_fx(xj) + (x-xj)*log_fxD(xj)
  return(u)
}  
##-- lower hull ----
lx <- function(log_fx, x, xj, xj1){
  l <- (log_fx(xj1)*(x-xj)+log_fx(xj)*(xj1-x))/(xj1-xj)
  return(l)
}
##-- upper hull intercepts ----
UxIntercept <- function(log_fx, log_fxD, xj, xj1){
  aux1 <- log_fx(xj)
  aux2 <- log_fx(xj1)
  aux3 <- log_fxD(xj)
  aux4 <- log_fxD(xj1)
  
  tol <- 10e-30
  auxNum <- aux3 - aux4
  
  if(auxNum < tol & auxNum > -tol){
    z <- (xj1+xj)/2
  } else{
    z <- (aux2-aux1-xj1*aux4+xj*aux3)/(aux3-aux4)
  }
  
  return(z)
}
##--  intervals weights ----
WeightsInterval <- function(log_fx, log_fxD, Points, Intercepts){  
  zs <- Intercepts 
  aux <- log_fxD(Points)
  
  W_Interval <- apply(X = data.frame(1:length(aux)), MARGIN = 1, 
                      FUN = function(k) 
                        if(aux[k] == 0){
                          (zs[k+1] - zs[k])*exp(ux(log_fx = log_fx, log_fxD = log_fxD, x = zs[k], xj = zs[k+1]))
                        } else{
                          aux1 <- (1/aux[k])
                          aux2 <- exp(ux(log_fx = log_fx, log_fxD = log_fxD, x = zs[k+1], xj = Points[k]))
                          aux3 <- exp(ux(log_fx = log_fx, log_fxD = log_fxD, x = zs[k], xj = Points[k]))
                          aux1*(aux2-aux3)
                        })
  
  return(W_Interval)
}
##-- sample of the envelope ----
SampleEnvelope <- function(log_fx, log_fxD, limInf, limSup, Points, Intercepts, W_Interval){
  zs <- Intercepts 
  
  #------------ sample interval
  Interval <- sample(x = 1:length(W_Interval), prob = W_Interval, size = 1)
  
  zaux <- zs[Interval:(Interval+1)]
  xaux <- Points[Interval]  
  
  #------------ sample of the envelope
  Unif <- runif(n = 1)
  tol <- 1e-30                      # derivate equal to zero  
  
  aux1 <- log_fxD(xaux)
  
  if(abs(aux1)<tol | is.na(aux1)){
    x <- runif(n = 1, min = zaux[1], max = zaux[2]) 
    
    Element <- max(0, which(x>Points))
    return(c(x, Element, Interval))
  } else{  
    aux2 <- exp(ux(log_fx = log_fx, log_fxD = log_fxD, x = zaux[1], xj = xaux))
    aux3 <- exp(ux(log_fx = log_fx, log_fxD = log_fxD, x = zaux[2], xj = xaux))
    aux4 <- exp(log_fx(xaux))
    
    x <- (1/aux1)*log(aux2/aux4 + Unif*((aux3-aux2)/aux4)) + xaux  
    
    Element <- max(0, which(x>Points))
    return(c(x, Element, Interval))
  }
}
##-- graphs ----
##-- + histogram ----
HistARS <- function(fx, sample, limInf, limSup, const){  
  #------------ colors
  colPoints <- "#0b5394"
  colLine <- "IndianRed"
  colHist <- "SteelBlue"
  
  if(all(is.na(sample))){
    plotHistARS <- ggplot(data = data.frame(x = c(limInf, limSup))) + 
      ylab("Density") + xlim(limInf, limSup) +
      stat_function(fun = function(x) fx(x)/const, col = colLine, size = size_line) + 
      gg_tema
  } else{
    sample <- sample[!is.na(sample)]
    plotHistARS <- ggplot(data = data.frame(x = sample)) + 
      geom_histogram(aes(x = x, y = ..density..),                     
                     fill = colHist, col = colHist, alpha = 0.4,
                     binwidth  = (limSup - limInf)/15,
                     boundary = limInf) +
      stat_function(fun = function(x) fx(x)/const, col = colLine, size = 1.5) +
      ylab("Density") + xlim(limInf, limSup) + 
      geom_point(data = data.frame(x = sample), aes(x = x, y = -Inf), 
                 size = 3, alpha = 0.9, col = colPoints) +
      gg_tema
  }
  return(plotHistARS)
}
##-- + log-function ----
LogFunGraph <- function(log_fx, log_fxD, limInf, limSup, Points, Intercepts){ 
  #------------ colors
  colPoints <- "#0b5394"
  colLine <- "IndianRed"
  colEnvSup <- "SteelBlue"
  colEnvInf <- "orange"
  
  #------------
  xs <- Points
  zs <- Intercepts
  
  #------------ Plot LogFunGraph    
  plotLogFunGraph <- ggplot(data = data.frame(x = c(limInf, limSup)), aes(x = x)) + 
    ylab("h(x)") + xlab("") + gg_tema +
    #---------- log function
    stat_function(fun = log_fx, size = 1.3, col = colLine) +
    #---------- upper hull    
    geom_segment(data = data.frame(x = zs[-length(zs)], 
                                   y = ux(log_fx = log_fx, log_fxD = log_fxD, x = zs[-length(zs)], xj = xs), 
                                   xend = zs[-1], 
                                   yend = ux(log_fx = log_fx, log_fxD = log_fxD, x = zs[-1], xj = xs)), 
                 aes(x = x, y = y, xend = xend, yend = yend), 
                 linetype = "dashed", col = colEnvSup, size = 1) +
    #---------- lower hull 
    geom_segment(data = data.frame(x = xs[-length(xs)], 
                                   y = log_fx(xs[-length(xs)]), 
                                   xend = xs[-1], 
                                   yend = log_fx(xs[-1])), 
                 aes(x = x, y = y, xend = xend, yend = yend), 
                 linetype = "dotted", col = colEnvInf, size = 1) +
    #---------- vertical line
    geom_segment(data = data.frame(x = xs, 
                                   y = log_fx(xs), 
                                   xend = xs, 
                                   yend = rep(-Inf, length(xs))), 
                 aes(x = x, y = y, xend = xend, yend = yend), 
                 linetype = "dotted", col = colPoints, size = 1) +    
    geom_point(data = data.frame(p = Points), aes(x = p, y = -Inf), 
               size = 3, alpha = 0.9, col = colPoints)
  
  return(plotLogFunGraph)  
}
##-- function ----
FunGraph <- function(fx, log_fx, log_fxD, limInf, limSup, Points, Intercepts){ 
  #------------ colors
  colPoints <- "#0b5394"
  colLine <- "IndianRed"
  colEnvSup <- "SteelBlue"
  colEnvInf <- "orange" 
  
  #------------ initial points
  xs <- Points
  zs <- Intercepts
  
  #------------ Plot FunGraph
  nSeq <- 10
  
  x1 <- as.vector(apply(data.frame(1:(length(zs)-1)), 1, 
                        function(i) seq(zs[i],zs[i+1], length.out = nSeq)))
  y1 <- as.vector(apply(data.frame(1:length(Points)), 1, 
                        function(i) exp(ux(log_fx = log_fx, log_fxD = log_fxD, x = x1[(nSeq*(i-1)+1):(nSeq*i)], xj = Points[i]))))
  
  x2 <- as.vector(apply(data.frame(1:(length(Points)-1)), 1, function(i) seq(Points[i], Points[i+1], length.out = nSeq)))
  y2 <- as.vector(apply(data.frame(1:(length(Points)-1)), 1, function(i) exp(lx(log_fx = log_fx, x = x2[(nSeq*(i-1)+1):(nSeq*i)], xj = Points[i], xj1 = Points[i+1]))))
  
  bSup <- data.frame(x = x1, y = y1)
  bInf <- data.frame(x = x2, y = y2)
  
  plotFunGraph <- ggplot(data = data.frame(x = c(limInf, limSup)), aes(x = x)) + 
    ylab("f(x)") + xlab("") + gg_tema +
    #---------- function
    stat_function(fun = fx, size = 1.3, col = colLine) +
    #---------- envelope   
    geom_line(data = bSup, aes(x = x, y = y), linetype="dashed", col = colEnvSup, size = 1) +
    #---------- squeezing 
    geom_line(data = bInf, aes(x = x, y = y), linetype="dotted", col = colEnvInf, size = 1) +
    #---------- vertical line
    geom_segment(data = data.frame(x = xs, 
                                   y = fx(xs), 
                                   xend = xs, 
                                   yend = rep(-Inf, length(xs))), 
                 aes(x = x, y = y, xend = xend, yend = yend), 
                 linetype = "dotted", col = colPoints, size = 1) +    
    geom_point(data = data.frame(p = Points), aes(x = p, y = -Inf), size = 3, alpha = 0.9, col = colPoints)
  
  return(plotFunGraph)
}
##-- acceptance rate ----
TSGraph <- function(AcceptRate, nIterations){
  #------------ colors
  colLine <- "IndianRed" 
  
  AcceptRate <- data.frame(AR = AcceptRate, Iteration = 1:length(AcceptRate))
  ggplot(AcceptRate) + geom_line(aes(x = Iteration, y = AR), 
                                 size = 1.5, alpha = 1, colour = colLine) +
    gg_tema +
    xlim(1, nIterations) + ylim(c(0, 1)) + ylab("Acceptance rate")
}
##-- sampler ----
##-- + ARS sampler ----
ARS <- function(fx, log_fx, log_fxD, limInf, limSup, Points, Intercepts, W_Interval){
  aux <- data.frame(xj = Points[-length(Points)], xj1 = Points[-1])
  zs <- Intercepts
  
  x <- SampleEnvelope(log_fx = log_fx, log_fxD = log_fxD, limInf = limInf, limSup = limSup, 
                      Points = Points, Intercepts = zs, W_Interval = W_Interval)
  
  Interval <- x[3]
  Element <- x[2]
  x <- x[1]
  
  if(Element < Interval){
    p1 <- zs[Interval]
    p2 <- Points[Interval]
  } else{
    p1 <- Points[Interval]
    p2 <- zs[Interval + 1]
  }
  
  # squeezing Test
  Unif <- runif(n = 1)  
  Squeezing <- exp(lx(log_fx = log_fx, x = x, xj = p1, xj1 = p2)-
                     ux(log_fx = log_fx, log_fxD = log_fxD, x = x, xj = p1))
  
  if(Unif <= Squeezing){
    Sample <- x
    YN <- 1
  } else{
    # rejection Test
    Rejection <- exp(log_fx(x = x)- ux(log_fx = log_fx, log_fxD = log_fxD, x = x, xj = p1))
    
    if(Unif <= Rejection){      
      Sample <- x
      YN <- 2
    }  else {YN <- 0}
  }
  return(c(x, YN, Element, Interval))
}