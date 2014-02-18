rm(list=ls())
x <- "NetLogo.csv"

netlogo <- function(x){
  x <- gsub(".csv","",x)
  file.name <- paste(x, ".csv", sep="")
  file <- scan(file.name, what="", n=1, skip=1, sep=",")
  date <- scan(file.name, what="", n=1, skip=2, sep=",")
  toplevel <- paste(file,date,sep=" ")
  toplevel <- gsub("\\/|\\:",".",toplevel)
  toplevel <- gsub(" ","_",toplevel)
  dir.create(toplevel)
  sapply(c("Globals", "Turtles", "Plots"), function(x) dir.create(paste(toplevel,"/", x ,sep="")))
  sapply(c("PositionPlot", "WinnersPlot","PolarizationPlot","IncumbentPercentagePlot"),
         function(x) dir.create(paste(toplevel,"/Plots/",x,sep="")))
  
  globals.begin <- 8
  glname <- scan(file.name, what="", n=-1, nlines=1, skip=globals.begin, sep=",")
  glvalues <- scan(file.name, what="", n=-1, nlines=1, skip=globals.begin+1, sep="," )
  glvalues <- gsub("\\]|\\[|^\"|\"$", "", glvalues)
  global <-  sapply(glvalues, strsplit, split=" ")
    
  is.not.numeric <- is.na(sapply(global, as.numeric))
  is.logical <- sapply(1:length(global), function(i) is.not.numeric[[i]]==TRUE & any(global[[i]] %in% c("TRUE","FALSE")))
  global <- sapply(1:length(global), function(i) if(is.not.numeric[i]==FALSE) as.numeric(global[[i]]) else global[[i]])
  global <- sapply(1:length(global), function(i) if(is.logical[i]==TRUE) as.logical(global[[i]]) else global[[i]])
  names(global) <- glname
  
  dump("global", file=paste(toplevel,"/Globals/Globals.R",sep=""))
  
  n.districts <- unlist(global["num-districts"])
  n.activists <- n.districts*unlist(global["num-activists-per-district"])
  n.voters <- n.districts*unlist(global["num-voters-per-district"])
  n.party <- unlist(global["n.parties"])
  n.candi <- n.districts*n.party
  total.obs <- n.districts+n.activists+n.voters+n.party+n.candi
  
  turtles.begin <- globals.begin+4
  
  col.names <- scan(file.name, what="", n=-1, nlines=1, skip=turtles.begin, sep=",")
  col.names <- subset(col.names, col.names!="")
  turtles <- scan(file.name, what="", n=-1, nlines=total.obs, skip=turtles.begin+1, sep=",")
  turtles <- gsub("\\]|\\[|\\{breed |\\{turtles |\\}|^\"|\"$", "", turtles)
  turtles <- matrix(turtles, total.obs, byrow=TRUE)
  turtles <- data.frame(turtles[,1:length(col.names)], stringsAsFactors=FALSE )
  colnames(turtles) <- col.names
  turtles[turtles[,]==""] <- NA
  
  districts <- subset(turtles, turtles$breed=="districts")

  is.na.all <- which(sapply(districts, function(x) sum(is.na(x))==n.districts))
  is.constant <- which(sapply(districts, function(x) length(unique(x))==1))
  drop <- c(is.na.all, is.constant)
  districts <- districts[,-drop]
  d.name <- colnames(districts)
  districts <- unlist(sapply(districts, strsplit, split=" "))
  districts <- matrix(districts, nrow=n.districts, byrow=FALSE)
  d.name[9:10] <- d.name[7]
  d.name[7:8] <- d.name[6]  
  colnames(districts) <- d.name
    
  write.csv(districts, paste(toplevel,"/Turtles/Districts.csv", sep=""), row.names=FALSE)
  
  voters <- subset(turtles, turtles$breed=="voters")
  
  is.na.all <- which(sapply(voters, function(x) sum(is.na(x))==n.voters))
  is.constant <- which(sapply(voters, function(x) length(unique(x))==1))
  drop <- c(is.na.all, is.constant)
  voters <- voters[,-drop]
  v.name <- colnames(voters)
  voters <- unlist(sapply(voters, strsplit, split=" "))
  voters <- matrix(voters, nrow=n.voters, byrow=FALSE)
  v.name[11:13] <- v.name[9]
  v.name[10] <- v.name[8]  
  v.name[8:9] <- v.name[7]
  colnames(voters) <- v.name
  
  write.csv(voters, paste(toplevel,"/Turtles/Voters.csv", sep=""), row.names=FALSE)
    
  activists <- subset(turtles, turtles$breed=="activists")
  
  is.na.all <- which(sapply(activists, function(x) sum(is.na(x))==n.activists))
  is.constant <- which(sapply(activists, function(x) length(unique(x))==1))
  drop <- c(is.na.all, is.constant)
  activists <- activists[,-drop]
  a.name <- colnames(activists)
  activists <- unlist(sapply(activists, strsplit, split=" "))
  activists <- matrix(activists, nrow=n.activists, byrow=FALSE)
  a.name[14] <- a.name[10]
  a.name[11:13] <- a.name[9]  
  a.name[9:10] <- a.name[8]
  colnames(activists) <- a.name
  
  write.csv(activists, paste(toplevel,"/Turtles/Activists.csv", sep=""), row.names=FALSE)  
  
  parties <- subset(turtles, turtles$breed=="parties")
  
  is.na.all <- which(sapply(parties, function(x) sum(is.na(x))==n.party))
  is.constant <- which(sapply(parties, function(x) length(unique(x))==1))
  drop <- c(is.na.all, is.constant)
  parties <- parties[,-drop]
  p.name <- colnames(parties)
  parties <- unlist(sapply(parties, strsplit, split=" "))
  parties <- matrix(parties, nrow=n.party, byrow=FALSE)
  p.name[223] <- p.name[12]
  p.name[222] <- p.name[11]  
  p.name[219:221] <- p.name[10]
  p.name[11:218] <- p.name[9]
  p.name[10] <- p.name[8]
  p.name[8:9] <- p.name[7]
  colnames(parties) <- p.name
  
  write.csv(parties, paste(toplevel,"/Turtles/Parties.csv", sep=""), row.names=FALSE)  
  
  
  candidates <- subset(turtles, turtles$breed=="cands")
  
  is.na.all <- which(sapply(candidates, function(x) sum(is.na(x))==n.candi))
  is.constant <- which(sapply(candidates, function(x) length(unique(x))==1))
  drop <- c(is.na.all, is.constant)
  candidates <- candidates[,-drop]
  c.name <- colnames(candidates)
  candidates <- unlist(sapply(candidates, strsplit, split=" "))
  candidates <- matrix(candidates, nrow=n.candi, byrow=FALSE)
  c.name[14:16] <- c.name[12]
  c.name[13] <- c.name[11]  
  c.name[12] <- c.name[10]
  c.name[11] <- c.name[9]
  c.name[10] <- c.name[8]
  c.name[8:9] <- c.name[7]
  colnames(candidates) <- c.name
  
  write.csv(candidates, paste(toplevel,"/Turtles/Candidates.csv", sep=""), row.names=FALSE) 
  
  patches.begin <- turtles.begin+total.obs+2
  patches.length <- (abs(unlist(global['min-pxcor']))+abs(unlist(global['max-pxcor']))+1)*(abs(unlist(global['min-pycor']))+abs(unlist(global['max-pycor']))+1)+13
  D1.info.begin <- patches.begin+patches.length 
  n.of.pen <- scan(file.name, what="", n=8, nlines=1, skip=D1.info.begin, sep=",")
  n.of.pen <- as.numeric(n.of.pen[8])
  n.of.x <- scan(file.name, what="", n=6, skip=D1.info.begin+3, sep=",")
  n.of.x <- ifelse(as.numeric(n.of.x[6])!=0,as.numeric(n.of.x[6])+1,0)
  position.begin <- D1.info.begin+n.of.pen+6
  
  col.names <- scan(file.name, what="", n=-1, nlines=1, skip=position.begin-2, sep=",")
  col.names <- gsub("^\"|\"$","",col.names)
  col.names <- subset(col.names, col.names != "")
  col.names[1] <- "RedIncumbents"
  col.names[2] <- "BlueIncumbents"
  plot.d1 <- scan(file.name, what="", n=-1, nlines=n.of.x, skip=position.begin, sep=",")
  plot.d1 <- subset(plot.d1, plot.d1!="")
  plot.d1 <- matrix(plot.d1, n.of.x, 4*n.of.pen, byrow=TRUE)
  plot.d1 <- plot.d1[,seq(2,(4*n.of.pen-2), by=4)]
  colnames(plot.d1) <- col.names
  period <- 0:(n.of.x-1)
  plot.d1 <- cbind(period, plot.d1)
  write.csv(plot.d1, paste(toplevel,"/Plots/PositionPlot/D1.csv", sep=""), row.names=FALSE) 

  interval <- n.of.x+3
  D2.info.begin <- position.begin+interval 
  n.of.pen <- scan(file.name, what="", n=8, skip=D2.info.begin, sep=",")
  n.of.pen <- as.numeric(n.of.pen[8])
  n.of.x <- scan(file.name, what="", n=6, skip=D2.info.begin+3, sep=",")
  n.of.x <- ifelse(as.numeric(n.of.x[6])!=0,as.numeric(n.of.x[6])+1,0)
  position.begin.2 <- D2.info.begin + n.of.pen+6
  
  plot.d2 <- scan(file.name, what="", n=-1, nlines=n.of.x, skip=position.begin.2, sep=",")
  plot.d2 <- subset(plot.d2, plot.d2!="")
  plot.d2 <- matrix(plot.d2, n.of.x, 4*n.of.pen, byrow=TRUE)
  plot.d2 <- plot.d2[,seq(2,(4*n.of.pen-2), by=4)]
  colnames(plot.d2) <- col.names
  period <- 0:(n.of.x-1)
  plot.d2 <- cbind(period, plot.d2)
  write.csv(plot.d2, paste(toplevel,"/Plots/PositionPlot/D2.csv", sep=""), row.names=FALSE) 
  
  interval <- n.of.x+3
  D3.info.begin <- position.begin.2+interval
  n.of.pen <- scan(file.name, what="", n=8, skip=D3.info.begin, sep=",")
  n.of.pen <- as.numeric(n.of.pen[8])
  n.of.x <- scan(file.name, what="", n=6, skip=D3.info.begin+3, sep=",")
  n.of.x <- ifelse(as.numeric(n.of.x[6])!=0,as.numeric(n.of.x[6])+1,0)
  position.begin.3 <- D3.info.begin + n.of.pen+6

  plot.d3 <- scan(file.name, what="", n=-1, nlines=n.of.x, skip=position.begin.3, sep=",")
  plot.d3 <- subset(plot.d3, plot.d3!="")
  plot.d3 <- matrix(plot.d3, n.of.x, 4*n.of.pen, byrow=TRUE)
  plot.d3 <- plot.d3[,seq(2,(4*n.of.pen-2), by=4)]
  colnames(plot.d3) <- col.names
  period <- 0:(n.of.x-1)
  plot.d3 <- cbind(period, plot.d3)
  write.csv(plot.d3, paste(toplevel,"/Plots/PositionPlot/D3.csv", sep=""), row.names=FALSE) 

  interval <- n.of.x+3
  D4.info.begin <- position.begin.3+interval
  n.of.pen <- scan(file.name, what="", n=8, skip=D4.info.begin, sep=",")
  n.of.pen <- as.numeric(n.of.pen[8])
  n.of.x <- scan(file.name, what="", n=6, skip=D4.info.begin+3, sep=",")
  n.of.x <- ifelse(as.numeric(n.of.x[6])!=0,as.numeric(n.of.x[6])+1,0)
  position.begin.4 <- D4.info.begin + n.of.pen+6
  
  interval <- n.of.x+3
  D5.info.begin <- position.begin.4+interval
  n.of.pen <- scan(file.name, what="", n=8, skip=D5.info.begin, sep=",")
  n.of.pen <- as.numeric(n.of.pen[8])
  n.of.x <- scan(file.name, what="", n=6, skip=D5.info.begin+3, sep=",")
  n.of.x <- ifelse(as.numeric(n.of.x[6])!=0,as.numeric(n.of.x[6])+1,0)
  position.begin.5 <- D5.info.begin + n.of.pen+6
  
  interval <- n.of.x+3
  D6.info.begin <- position.begin.5+interval
  n.of.pen <- scan(file.name, what="", n=8, skip=D6.info.begin, sep=",")
  n.of.pen <- as.numeric(n.of.pen[8])
  n.of.x <- scan(file.name, what="", n=6, skip=D6.info.begin+3, sep=",")
  n.of.x <- ifelse(as.numeric(n.of.x[6])!=0,as.numeric(n.of.x[6])+1,0)
  position.begin.6 <- D6.info.begin + n.of.pen+6
  
  interval <- n.of.x+3
  winner.info <- position.begin.6+interval
  n.of.pen <- scan(file.name, what="", n=8, skip=winner.info, sep=",")
  n.of.pen <- as.numeric(n.of.pen[8])
  n.of.x <- scan(file.name, what="", n=6, skip=winner.info+3, sep=",")
  n.of.x <- ifelse(as.numeric(n.of.x[6])!=0,as.numeric(n.of.x[6])+1,0)
  winner.begin <- winner.info+n.of.pen+6
  
  col.names <- scan(file.name, what="", n=-1, nlines=1, skip=winner.begin-2, sep=",")
  col.names <- subset(col.names, col.names!="")
  winner <- scan(file.name, what="", n=-1, nlines=n.of.x, skip=winner.begin, sep=",")
  winner <- subset(winner, winner!="")
  winner <- matrix(winner, n.of.x, 4*n.of.pen, byrow=TRUE)
  winner <- winner[,seq(2,(4*n.of.pen-2), by=4)]
  colnames(winner) <- col.names
  period <- 0:(n.of.x-1)
  winner <- cbind(period, winner)
  write.csv(winner, paste(toplevel,"/Plots/WinnersPlot/Winner.csv", sep=""), row.names=FALSE) 
  
  interval <- n.of.x+3
  polar.info <- winner.begin+interval
  n.of.pen <- scan(file.name, what="", n=8, skip=polar.info, sep=",")
  n.of.pen <- as.numeric(n.of.pen[8])
  n.of.x <- scan(file.name, what="", n=6, skip=polar.info+3, sep=",")
  n.of.x <- ifelse(as.numeric(n.of.x[6])!=0,as.numeric(n.of.x[6])+1,0)
  polar.begin <- polar.info+n.of.pen+6
  
  col.names <- scan(file.name, what="", n=-1, nlines=1, skip=polar.begin-2, sep=",")
  col.names <- subset(col.names, col.names!="")
  polar <- scan(file.name, what="", n=-1, nlines=n.of.x, skip=polar.begin, sep=",")
  polar <- subset(polar, polar!="")
  polar <- matrix(polar, n.of.x, 4*n.of.pen, byrow=TRUE)
  polar <- polar[,seq(2,(4*n.of.pen-2), by=4)]
  colnames(polar) <- col.names
  period <- 0:(n.of.x-1)
  polar <- cbind(period, polar)
  write.csv(polar, paste(toplevel,"/Plots/PolarizationPlot/Polarization.csv", sep=""), row.names=FALSE) 
  
  interval <- n.of.x+3
  Incum.info <- polar.begin+interval
  n.of.pen <- scan(file.name, what="", n=8, skip=Incum.info, sep=",")
  n.of.pen <- as.numeric(n.of.pen[8])
  n.of.x <- scan(file.name, what="", n=6, skip=Incum.info+3, sep=",")
  n.of.x <- ifelse(as.numeric(n.of.x[6])!=0,as.numeric(n.of.x[6])+1,0)
  incum.begin <- Incum.info+n.of.pen+6
  
  percent <- scan(file.name, what="", n=-1, nlines=n.of.x, skip=incum.begin, sep=",")
  percent <- subset(percent, percent!="")
  percent <- matrix(percent, n.of.x, 4*n.of.pen, byrow=TRUE)
  percent <- percent[,seq(2,(4*n.of.pen-2), by=4)]
  period <- 0:(n.of.x-1)
  percent <- cbind(period, percent)
  write.csv(percent, paste(toplevel,"/Plots/IncumbentPercentagePlot/IncumbentWins.csv", sep=""), row.names=FALSE) 
}