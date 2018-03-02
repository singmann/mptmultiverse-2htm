rm(list=ls())

dmat <- matrix(scan("c://work/broder_rating_bias/exp1.txt"),ncol=16,byrow=TRUE)

MPT <- function(Q, gen=FALSE){
	
	Do <- Q[1]
	Dn <- Q[2]
	
	g <- Q[3]
	
	dorm1 <- Q[4]
	dorm2 <- Q[5]
	dorm3 <- Q[6]

	dnrm1 <- Q[7]
	dnrm2 <- Q[8]
	dnrm3 <- dorm3#Q[7]	

	gorm1 <- Q[9]
	gorm2 <- Q[10]
	gorm3 <- Q[11]

	gnrm1 <- Q[12]
	gnrm2 <- gorm2#Q[13]
	gnrm3 <- gorm3#Q[11]	

	
	ro <- c(dorm1,(1-dorm1)*dorm2,(1-dorm1)*(1-dorm2)*dorm3,(1-dorm1)*(1-dorm2)*(1-dorm3))
	rn <- c(dnrm1,(1-dnrm1)*dnrm2,(1-dnrm1)*(1-dnrm2)*dnrm3,(1-dnrm1)*(1-dnrm2)*(1-dnrm3))	
	go <- c(gorm1,(1-gorm1)*gorm2,(1-gorm1)*(1-gorm2)*gorm3,(1-gorm1)*(1-gorm2)*(1-gorm3))
	gn <- c(gnrm1,(1-gnrm1)*gnrm2,(1-gnrm1)*(1-gnrm2)*gnrm3,(1-gnrm1)*(1-gnrm2)*(1-gnrm3))	
	
	#ro <- rev(ro)
	#rn <- rev(rn)
	#go <- rev(go)
	#gn <- rev(gn)	
	
	e <- c()

	e[1] <- (1-Do)*(1-g)*gn[1]
	e[2] <- (1-Do)*(1-g)*gn[2]
	e[3] <- (1-Do)*(1-g)*gn[3]
	e[4] <- (1-Do)*(1-g)*gn[4]
	e[5] <- Do*ro[4] + (1-Do)*g*go[4]
	e[6] <- Do*ro[3] + (1-Do)*g*go[3]
	e[7] <- Do*ro[2] + (1-Do)*g*go[2]
	e[8] <- Do*ro[1] + (1-Do)*g*go[1]

	e[9] <-  Dn*rn[1] + (1-Dn)*(1-g)*gn[1] 
	e[10] <- Dn*rn[2] + (1-Dn)*(1-g)*gn[2]
	e[11] <- Dn*rn[3] + (1-Dn)*(1-g)*gn[3]
	e[12] <- Dn*rn[4] + (1-Dn)*(1-g)*gn[4]
	e[13] <- (1-Dn)*g*go[4]
	e[14] <- (1-Dn)*g*go[3]
	e[15] <- (1-Dn)*g*go[2]
	e[16] <- (1-Dn)*g*go[1]


	if(gen==TRUE) return(e)
	if(sum(is.na(e))>0) {print("blah");QQ <<- Q;print(Q);print(e);return(Inf)}
	if(sum(is.nan(e))>0) {print("blah");print(e);return(Inf)}
	
	N <- c(rep(sum(d[1:8]),8), rep(sum(d[9:16]),8))

	e <- N*e
	Gsq <- 2*sum(d[d!=0]*(log(d[d!=0])-log(e[d!=0])))
	return(Gsq)
}


SDT <- function(Q, gen=FALSE){

   e=rep(NA,16)

    mu1 <- Q[1] 
    ss1 <- Q[2] 

   
    cr1 <- cumsum(Q[3:9])

	
	   
    # old items
    e[1] <- pnorm((cr1[1]-mu1)/ss1)        
    e[2] <- pnorm((cr1[2]-mu1)/ss1) - pnorm((cr1[1]-mu1)/ss1) 
    e[3] <- pnorm((cr1[3]-mu1)/ss1) - pnorm((cr1[2]-mu1)/ss1)
    e[4] <- pnorm((cr1[4]-mu1)/ss1) - pnorm((cr1[3]-mu1)/ss1)
    e[5] <- pnorm((cr1[5]-mu1)/ss1) - pnorm((cr1[4]-mu1)/ss1) 
	e[6] <- pnorm((cr1[6]-mu1)/ss1) - pnorm((cr1[5]-mu1)/ss1) 
	e[7] <- pnorm((cr1[7]-mu1)/ss1) - pnorm((cr1[6]-mu1)/ss1) 
    e[8] <- 1-pnorm((cr1[7]-mu1)/ss1)    

    #new items
    e[9] <- pnorm(cr1[1])
    e[10] <- pnorm(cr1[2]) - pnorm(cr1[1])
    e[11] <- pnorm(cr1[3]) - pnorm(cr1[2])
    e[12] <- pnorm(cr1[4]) - pnorm(cr1[3])
    e[13] <- pnorm(cr1[5]) - pnorm(cr1[4])
	e[14] <- pnorm(cr1[6]) - pnorm(cr1[5])
	e[15] <- pnorm(cr1[7]) - pnorm(cr1[6])
    e[16] <- 1-pnorm(cr1[7])

	

    if(gen==TRUE) return(e)
    #print(c(sum(e[1:8]),sum(e[9:16]),sum(e[17:24]), sum(e[25:32])))
	N <- c(rep(sum(d[1:8]),8), rep(sum(d[9:16]),8))
	e <- N*e
	Gsq <- sum(2* d[d!=0]*(log(d[d!=0])-log(e[d!=0])))
    return(Gsq)
}





fitruns <- 10
resultsMPT1 <- matrix(NA,nrow(dmat),13)
resultsSDT1 <- matrix(NA,nrow(dmat),10)



for (i in 1:nrow(dmat)) {

	d <- dmat [i,]
	
	temp <- Inf; temppar <- c()
			for (j in 1:fitruns){
			start <- c(runif(12))
			 GtestMPT <- nlminb(start,MPT, lower=c(rep(0,12)), upper=c(rep(1,12))) 
			if (GtestMPT$objective < temp) {temp <- GtestMPT$objective; temppar <- GtestMPT$par} 
		}
		
	resultsMPT1[i,] <- c(temp, temppar)

	
		temp <- Inf; temppar <- c()	
		for (j in 1:fitruns){
			start <- c(runif(1,0,2),runif(1,0.3,2),runif(1,-2,1),runif(6,0,0.4))
			GtestSDT <- nlminb(start,SDT, lower=c(0,0.1,-Inf,0,0,0,0,0,0), upper=rep(Inf,9)) 
			if (GtestSDT$objective < temp) {temp <- GtestSDT$objective; temppar <- GtestSDT$par}
		}
	resultsSDT1[i,] <- c(temp, temppar)
	
	print(i)
}


sum(resultsMPT1[,1])
sum(resultsSDT1[,1])


sum(resultsMPT1[,1] > 5.99)/length(resultsMPT1[,1]) #number of significant misfits to data
sum(resultsSDT1[,1] > 11.07)/length(resultsSDT1[,1]) #number of significant misfits to data











D1 <- 1 - (1-resultsMPT1[1:33,2])*(1-resultsMPT1[1:33,3])   # overall detection scores
D2 <- 1 - (1-resultsMPT1[34:68,2])*(1-resultsMPT1[34:68,3]) # overal detection scores

t.test(D1,D2,var.equal=TRUE) # tests of detection scores


t.test(resultsMPT1[1:33,4],resultsMPT1[34:68,4],var.equal=TRUE) #test for guessing biases

t.test(resultsMPT1[1:33,5],resultsMPT1[34:68,5],var.equal=TRUE,alternative="less")  # test for extreme rm in Do



t.test(resultsMPT1[1:33,8],resultsMPT1[34:68,8],var.equal=TRUE,alternative="less")  # test for extreme rm in Dn



	
t.test(resultsMPT1[1:33,9],resultsMPT1[34:68,9],var.equal=TRUE,alternative="less")   # test for extreme rm in Go
t.test(resultsMPT1[1:33,12],resultsMPT1[34:68,12],var.equal=TRUE,alternative="less")  # test for extreme rm in Gn


cor.test(resultsMPT1[,1],resultsSDT1[,1])






#########################
#### Exps 2 and 3    ####
#########################


dmat1 <- matrix(scan("c://work/broder_rating_bias/inDataTable.txt"),ncol=32,byrow=TRUE)
dmat2 <- matrix(scan("c://work/broder_rating_bias/indDataExp3.txt"),ncol=32,byrow=TRUE)

dmat <- rbind(dmat1,dmat2)






SDT <- function(Q,rest=0){

   e=rep(NA,32)

    mu1 <- Q[1] 
    ss1 <- Q[2] 
	mu2 <- Q[3]
	ss2 <- Q[4]
   
    cr1 <- cumsum(Q[5:11])
	cr2 <- cumsum(Q[12:18])
	
	if(rest==1){mu2 <- mu1; ss2<-ss1}
	
	
   
    # old items
    e[1] <- pnorm((cr1[1]-mu1)/ss1)        
    e[2] <- pnorm((cr1[2]-mu1)/ss1) - pnorm((cr1[1]-mu1)/ss1) 
    e[3] <- pnorm((cr1[3]-mu1)/ss1) - pnorm((cr1[2]-mu1)/ss1)
    e[4] <- pnorm((cr1[4]-mu1)/ss1) - pnorm((cr1[3]-mu1)/ss1)
    e[5] <- pnorm((cr1[5]-mu1)/ss1) - pnorm((cr1[4]-mu1)/ss1) 
	e[6] <- pnorm((cr1[6]-mu1)/ss1) - pnorm((cr1[5]-mu1)/ss1) 
	e[7] <- pnorm((cr1[7]-mu1)/ss1) - pnorm((cr1[6]-mu1)/ss1) 
    e[8] <- 1-pnorm((cr1[7]-mu1)/ss1)    

    #new items
    e[9] <- pnorm(cr1[1])
    e[10] <- pnorm(cr1[2]) - pnorm(cr1[1])
    e[11] <- pnorm(cr1[3]) - pnorm(cr1[2])
    e[12] <- pnorm(cr1[4]) - pnorm(cr1[3])
    e[13] <- pnorm(cr1[5]) - pnorm(cr1[4])
	e[14] <- pnorm(cr1[6]) - pnorm(cr1[5])
	e[15] <- pnorm(cr1[7]) - pnorm(cr1[6])
    e[16] <- 1-pnorm(cr1[7])
	
	# old items
    e[17] <- pnorm((cr2[1]-mu2)/ss2)        
    e[18] <- pnorm((cr2[2]-mu2)/ss2) - pnorm((cr2[1]-mu2)/ss2) 
    e[19] <- pnorm((cr2[3]-mu2)/ss2) - pnorm((cr2[2]-mu2)/ss2)
    e[20] <- pnorm((cr2[4]-mu2)/ss2) - pnorm((cr2[3]-mu2)/ss2)
    e[21] <- pnorm((cr2[5]-mu2)/ss2) - pnorm((cr2[4]-mu2)/ss2) 
	e[22] <- pnorm((cr2[6]-mu2)/ss2) - pnorm((cr2[5]-mu2)/ss2) 
	e[23] <- pnorm((cr2[7]-mu2)/ss2) - pnorm((cr2[6]-mu2)/ss2) 
    e[24] <- 1-pnorm((cr2[7]-mu2)/ss2)    

    #new items
    e[25] <- pnorm(cr2[1])
    e[26] <- pnorm(cr2[2]) - pnorm(cr2[1])
    e[27] <- pnorm(cr2[3]) - pnorm(cr2[2])
    e[28] <- pnorm(cr2[4]) - pnorm(cr2[3])
    e[29] <- pnorm(cr2[5]) - pnorm(cr2[4])
	e[30] <- pnorm(cr2[6]) - pnorm(cr2[5])
	e[31] <- pnorm(cr2[7]) - pnorm(cr2[6])
    e[32] <- 1-pnorm(cr2[7])
 
    
    #print(c(sum(e[1:8]),sum(e[9:16]),sum(e[17:24]), sum(e[25:32])))
	N <- c(rep(sum(d[1:8]),8), rep(sum(d[9:16]),8), rep(sum(d[17:24]),8),rep(sum(d[25:32]),8))
	e <- N*e
	Gsq <- sum(2* d[d!=0]*(log(d[d!=0])-log(e[d!=0])))
    return(Gsq)
}




MPT <- function(Q, gen=FALSE, rest=0){
	
	Do <- Q[1]
	Dn <- Q[2]
	
	g <- Q[3]
	
	dorm1 <- Q[4]
	dorm2 <- Q[5]
	dorm3 <- Q[6]

	dnrm1 <- Q[7]
	dnrm2 <- Q[8]
	dnrm3 <- dorm3#Q[7]	

	gorm1 <- Q[9]
	gorm2 <- Q[10]
	gorm3 <- Q[11]

	gnrm1 <- Q[12]
	gnrm2 <- gorm2#Q[13]
	gnrm3 <- gorm3#Q[11]	


	DDo <- Q[13]
	DDn <- Q[14]
	
	
	gg <- Q[15]
	
	ddorm1 <- Q[16]
	ddorm2 <- Q[17]
	ddorm3 <- Q[18]

	ddnrm1 <- Q[19]
	ddnrm2 <- Q[20]
	ddnrm3 <- ddorm3#Q[7]	

	ggorm1 <- Q[21]
	ggorm2 <- Q[22]
	ggorm3 <- Q[23]

	ggnrm1 <- Q[24]
	ggnrm2 <- ggorm2#Q[13]
	ggnrm3 <- ggorm3#Q[11]	

	if(rest==1){DDo <- Do; DDn <- Dn}
	
	
	ro <- c(dorm1,(1-dorm1)*dorm2,(1-dorm1)*(1-dorm2)*dorm3,(1-dorm1)*(1-dorm2)*(1-dorm3))
	rn <- c(dnrm1,(1-dnrm1)*dnrm2,(1-dnrm1)*(1-dnrm2)*dnrm3,(1-dnrm1)*(1-dnrm2)*(1-dnrm3))	
	go <- c(gorm1,(1-gorm1)*gorm2,(1-gorm1)*(1-gorm2)*gorm3,(1-gorm1)*(1-gorm2)*(1-gorm3))
	gn <- c(gnrm1,(1-gnrm1)*gnrm2,(1-gnrm1)*(1-gnrm2)*gnrm3,(1-gnrm1)*(1-gnrm2)*(1-gnrm3))	

	
	rro <- c(ddorm1,(1-ddorm1)*ddorm2,(1-ddorm1)*(1-ddorm2)*ddorm3,(1-ddorm1)*(1-ddorm2)*(1-ddorm3))
	rrn <- c(ddnrm1,(1-ddnrm1)*ddnrm2,(1-ddnrm1)*(1-ddnrm2)*ddnrm3,(1-ddnrm1)*(1-ddnrm2)*(1-ddnrm3))	
	ggo <- c(ggorm1,(1-ggorm1)*ggorm2,(1-ggorm1)*(1-ggorm2)*ggorm3,(1-ggorm1)*(1-ggorm2)*(1-ggorm3))
	ggn <- c(ggnrm1,(1-ggnrm1)*ggnrm2,(1-ggnrm1)*(1-ggnrm2)*ggnrm3,(1-ggnrm1)*(1-ggnrm2)*(1-ggnrm3))	



	
	#ro <- rev(ro)
	#rn <- rev(rn)
	#go <- rev(go)
	#gn <- rev(gn)	
	
	e <- c()

	e[1] <- (1-Do)*(1-g)*gn[1]
	e[2] <- (1-Do)*(1-g)*gn[2]
	e[3] <- (1-Do)*(1-g)*gn[3]
	e[4] <- (1-Do)*(1-g)*gn[4]
	e[5] <- Do*ro[4] + (1-Do)*g*go[4]
	e[6] <- Do*ro[3] + (1-Do)*g*go[3]
	e[7] <- Do*ro[2] + (1-Do)*g*go[2]
	e[8] <- Do*ro[1] + (1-Do)*g*go[1]

	e[9] <-  Dn*rn[1] + (1-Dn)*(1-g)*gn[1] 
	e[10] <- Dn*rn[2] + (1-Dn)*(1-g)*gn[2]
	e[11] <- Dn*rn[3] + (1-Dn)*(1-g)*gn[3]
	e[12] <- Dn*rn[4] + (1-Dn)*(1-g)*gn[4]
	e[13] <- (1-Dn)*g*go[4]
	e[14] <- (1-Dn)*g*go[3]
	e[15] <- (1-Dn)*g*go[2]
	e[16] <- (1-Dn)*g*go[1]

	
	e[17] <- (1-DDo)*(1-gg)*ggn[1]
	e[18] <- (1-DDo)*(1-gg)*ggn[2]
	e[19] <- (1-DDo)*(1-gg)*ggn[3]
	e[20] <- (1-DDo)*(1-gg)*ggn[4]
	e[21] <- DDo*rro[4] + (1-DDo)*gg*ggo[4]
	e[22] <- DDo*rro[3] + (1-DDo)*gg*ggo[3]
	e[23] <- DDo*rro[2] + (1-DDo)*gg*ggo[2]
	e[24] <- DDo*rro[1] + (1-DDo)*gg*ggo[1]

	e[25] <- DDn*rrn[1] + (1-DDn)*(1-gg)*ggn[1] 
	e[26] <- DDn*rrn[2] + (1-DDn)*(1-gg)*ggn[2]
	e[27] <- DDn*rrn[3] + (1-DDn)*(1-gg)*ggn[3]
	e[28] <- DDn*rrn[4] + (1-DDn)*(1-gg)*ggn[4]
	e[29] <- (1-DDn)*gg*ggo[4]
	e[30] <- (1-DDn)*gg*ggo[3]
	e[31] <- (1-DDn)*gg*ggo[2]
	e[32] <- (1-DDn)*gg*ggo[1]

	N <- c(rep(sum(d[1:8]),8), rep(sum(d[9:16]),8), rep(sum(d[17:24]),8),rep(sum(d[25:32]),8))
	e <- N*e
	Gsq <- sum(2* d[d!=0]*(log(d[d!=0])-log(e[d!=0])))
    return(Gsq)
}









fitruns <- 20
resultsMPT1 <- matrix(NA,nrow(dmat),25)
resultsSDT1 <- matrix(NA,nrow(dmat),19)

resultsMPT2 <- matrix(NA,nrow(dmat),25)
resultsSDT2 <- matrix(NA,nrow(dmat),19)

for (i in 1:nrow(dmat)) {

	d <- dmat [i,]
	
	temp <- Inf; temppar <- c()
			for (j in 1:fitruns){
			start <- c(runif(24))
			 GtestMPT <- nlminb(start,MPT, lower=c(rep(0,24)), upper=c(rep(1,24))) 
			if (GtestMPT$objective < temp) {temp <- GtestMPT$objective; temppar <- GtestMPT$par}
		}
		
	resultsMPT1[i,] <- c(temp, temppar)

	
		temp <- Inf; temppar <- c()	
		for (j in 1:fitruns){
			start <- c(runif(1,0,2),runif(1,0.3,2),runif(1,0,2),runif(1,0.3,2),runif(1,-2,1),runif(6,0,0.4),runif(1,-2,1),runif(6,0,0.4))
			GtestSDT <- nlminb(start,SDT, lower=c(0,0.1,0,0.1,-Inf,0,0,0,0,0,0,-Inf,0,0,0,0,0,0), upper=rep(Inf,18)) 
			if (GtestSDT$objective < temp) {temp <- GtestSDT$objective; temppar <- GtestSDT$par}
		}
	resultsSDT1[i,] <- c(temp, temppar)
	
	
		temp <- Inf; temppar <- c()
			for (j in 1:fitruns){
			start <- c(runif(24))
			 GtestMPT <- nlminb(start,MPT, lower=c(rep(0,24)), upper=c(rep(1,24)),rest=1) 
			if (GtestMPT$objective < temp) {temp <- GtestMPT$objective; temppar <- GtestMPT$par}
		}
		
	resultsMPT2[i,] <- c(temp, temppar)

	
		temp <- Inf; temppar <- c()	
		for (j in 1:fitruns){
			start <- c(runif(1,0,2),runif(1,0.3,2),runif(1,0,2),runif(1,0.3,2),runif(1,-2,1),runif(6,0,0.4),runif(1,-2,1),runif(6,0,0.4))
			GtestSDT <- nlminb(start,SDT, lower=c(0,0.1,0,0.1,-Inf,0,0,0,0,0,0,-Inf,0,0,0,0,0,0), upper=rep(Inf,18),rest=1) 
			if (GtestSDT$objective < temp) {temp <- GtestSDT$objective; temppar <- GtestSDT$par}
		}
	resultsSDT2[i,] <- c(temp, temppar)
	
	
	
	print(i)
}


##################
## EXPERIMENT 2 ##
##################
####################################################
# goodness of fit and memory-parameter restriction #
####################################################


# Exp2 - strong scale

#2HTM

median(resultsMPT1[1:25,1])
sum(resultsMPT1[1:25,1] > 9.49)

sum(resultsMPT1[1:25,1])
sum(resultsMPT2[1:25,1])
sum(resultsMPT2[1:25,1] - resultsMPT1[1:25,1])

sum(resultsMPT2[1:25,1] - resultsMPT1[1:25,1] < 5.99)
sum(resultsMPT2[1:25,1] - resultsMPT1[1:25,1] < 5.99)/25


# SDT
sum(resultsSDT1[1:25,1])
sum(resultsSDT2[1:25,1])
sum(resultsSDT1[1:25,1] > 18.31) 
sum(resultsSDT2[1:25,1] > 21.03)
sum(resultsSDT2[1:25,1] - resultsSDT1[1:25,1])

sum(resultsSDT2[1:25,1] - resultsSDT1[1:25,1] < 5.99)
sum(resultsSDT2[1:25,1] - resultsSDT1[1:25,1] < 5.99)/25

median(resultsMPT1[1:25,4])
median(resultsMPT1[1:25,16])



# Exp2  - weak scale

median(resultsMPT1[26:51,1])
sum(resultsMPT1[26:51,1])
sum(resultsMPT2[26:51,1])
sum(resultsMPT1[26:51,1] > 9.49)
sum(resultsMPT2[26:51,1] < 12.59)
sum(resultsMPT2[26:51,1] - resultsMPT1[26:51,1])

sum(resultsMPT2[26:51,1] - resultsMPT1[26:51,1] > 5.99)
sum(resultsMPT2[26:51,1] - resultsMPT1[26:51,1] < 5.99)/26


sum(resultsSDT1[26:51,1])
sum(resultsSDT2[26:51,1])
sum(resultsSDT1[26:51,1] > 18.31)
sum(resultsSDT2[26:51,1] > 21.03)
sum(resultsSDT2[26:51,1] - resultsSDT1[26:51,1] > 5.99)
sum(resultsSDT2[26:51,1] - resultsSDT1[26:51,1] < 5.99)/26


#################################################
# parameter estimates across scale manipulation #
#################################################

 # parameter estimates #

#round(c(apply(resultsMPT1[1:25,2:5],2,median) ,median((1-resultsMPT1[1:25,5])*resultsMPT1[1:25,6]),median((1-resultsMPT1[1:25,5])*(1-resultsMPT1[1:25,6])*resultsMPT1[1:25,7])),2)

#c(median(resultsMPT1[1:25,8]),median((1-resultsMPT1[1:25,8])*resultsMPT1[1:25,6]),median((1-resultsMPT1[1:25,8])*(1-resultsMPT1[1:25,6])*resultsMPT1[1:25,7]))


#median((1-resultsMPT1[1:25,5])*(1-resultsMPT1[1:25,6])*(1-resultsMPT1[1:25,7]))

#apply(resultsMPT1[26:51,2:5],2,median) 
 
#apply(resultsMPT2[1:25,2:5],2,median) 
#apply(resultsMPT2[26:51,2:5],2,median) 
 
 
 # tests on parameter estimates #

D1 <- 1 - (1-resultsMPT2[1:25,2])*(1-resultsMPT2[1:25,3])   # overall detection scores
D2 <- 1 - (1-resultsMPT2[26:51,2])*(1-resultsMPT2[26:51,3]) # overal detection scores

t.test(D1,D2,var.equal=TRUE) # test of overall detection score


t.test(resultsMPT2[1:25,2],resultsMPT2[26:51,2],var.equal=TRUE) #test for Do
t.test(resultsMPT2[1:25,3],resultsMPT2[26:51,3],var.equal=TRUE) #test for Dn



t.test(resultsMPT2[1:25,4],resultsMPT2[26:51,4],var.equal=TRUE) #test for guessing biases
t.test(resultsMPT2[1:25,16],resultsMPT2[26:51,16],var.equal=TRUE) #test for guessing biases

t.test(resultsMPT2[1:25,5],resultsMPT2[26:51,5],var.equal=TRUE,alternative="less")  # test for extreme rm in Do
t.test(resultsMPT2[1:25,8],resultsMPT2[26:51,8],var.equal=TRUE,alternative="less")  # test for extreme rm in Dn

t.test(resultsMPT2[1:25,17],resultsMPT2[26:51,17],var.equal=TRUE,alternative="less")  # test for extreme rm in Do
t.test(resultsMPT2[1:25,20],resultsMPT2[26:51,20],var.equal=TRUE,alternative="less")  # test for extreme rm in Dn





t.test(resultsMPT2[1:25,10],resultsMPT2[26:51,10],var.equal=TRUE,alternative="less")  # test for extreme rm in go
t.test(resultsMPT2[1:25,13],resultsMPT2[26:51,13],var.equal=TRUE,alternative="less")  # test for extreme rm in gn

t.test(resultsMPT2[1:25,22],resultsMPT2[26:51,22],var.equal=TRUE,alternative="less")  # test for extreme rm in go
t.test(resultsMPT2[1:25,25],resultsMPT2[26:51,25],var.equal=TRUE,alternative="less")  # test for extreme rm in gn
	
	
	
	
	



#240*25
#240*26

# exp2-strong-scale the compromise delta x^2 is 73.78
# exp2-strong-scale the compromise delta x^2 is 76.77



##################
## EXPERIMENT 3 ##
##################
####################################################
# goodness of fit and memory-parameter restriction #
####################################################


# Exp3 - strong scale

#2HTM
sum(resultsMPT1[52:75,1])
sum(resultsMPT2[52:75,1])
sum(resultsMPT2[52:75,1] - resultsMPT1[52:75,1])

sum(resultsMPT2[52:75,1] - resultsMPT1[52:75,1] < 5.99)
sum(resultsMPT2[52:75,1] - resultsMPT1[52:75,1] < 5.99)/24






# SDT
sum(resultsSDT1[52:75,1])
sum(resultsSDT2[52:75,1])
sum(resultsSDT2[52:75,1] - resultsSDT1[52:75,1])

sum(resultsSDT2[52:75,1] - resultsSDT1[52:75,1] < 5.99)
sum(resultsSDT2[52:75,1] - resultsSDT1[52:75,1] < 5.99)/24



# Exp3  - weak scale
sum(resultsMPT1[76:97,1])
sum(resultsMPT2[76:97,1])
sum(resultsMPT2[76:97,1] - resultsMPT1[76:97,1])

sum(resultsMPT2[76:97,1] - resultsMPT1[76:97,1] < 5.99)
sum(resultsMPT2[76:97,1] - resultsMPT1[76:97,1] < 5.99)/22


sum(resultsSDT1[76:97,1])
sum(resultsSDT2[76:97,1])
sum(resultsSDT2[76:97,1] - resultsSDT1[76:97,1])
sum(resultsSDT2[76:97,1] - resultsSDT1[76:97,1] < 5.99)/22

#320*24
#320*22


#################################################
# parameter estimates across scale manipulation #
#################################################

 # parameter estimates #

#round(c(apply(resultsMPT1[52:75,2:5],2,median) ,median((1-resultsMPT1[52:75,5])*resultsMPT1[52:75,6]),median((1-resultsMPT1[52:75,5])*(1-resultsMPT1[52:75,6])*resultsMPT1[52:75,7])),2)

#c(median(resultsMPT1[52:75,8]),median((1-resultsMPT1[52:75,8])*resultsMPT1[52:75,6]),median((1-resultsMPT1[52:75,8])*(1-resultsMPT1[52:75,6])*resultsMPT1[52:75,7]))


#median((1-resultsMPT1[52:75,5])*(1-resultsMPT1[52:75,6])*(1-resultsMPT1[52:75,7]))

#apply(resultsMPT1[76:97,2:5],2,median) 
 
#apply(resultsMPT2[52:75,2:5],2,median) 
#apply(resultsMPT2[76:97,2:5],2,median) 
 
 
 # tests on parameter estimates #

D1 <- 1 - (1-resultsMPT2[52:75,2])*(1-resultsMPT2[52:75,3])   # overall detection scores
D2 <- 1 - (1-resultsMPT2[76:97,2])*(1-resultsMPT2[76:97,3]) # overal detection scores

t.test(D1,D2,var.equal=TRUE) # test of overall detection score


t.test(resultsMPT2[52:75,2],resultsMPT2[76:97,2],var.equal=TRUE) #test for Do
t.test(resultsMPT2[52:75,3],resultsMPT2[76:97,3],var.equal=TRUE) #test for Dn



t.test(resultsMPT2[52:75,4],resultsMPT2[76:97,4],var.equal=TRUE) #test for guessing biases
t.test(resultsMPT2[52:75,16],resultsMPT2[76:97,16],var.equal=TRUE) #test for guessing biases

t.test(resultsMPT2[52:75,5],resultsMPT2[76:97,5],var.equal=TRUE,alternative="less")  # test for extreme rm in Do
t.test(resultsMPT2[52:75,8],resultsMPT2[76:97,8],var.equal=TRUE,alternative="less")  # test for extreme rm in Dn

t.test(resultsMPT2[52:75,17],resultsMPT2[76:97,17],var.equal=TRUE,alternative="less")  # test for extreme rm in Do
t.test(resultsMPT2[52:75,20],resultsMPT2[76:97,20],var.equal=TRUE,alternative="less")  # test for extreme rm in Dn





t.test(resultsMPT2[52:75,10],resultsMPT2[76:97,10],var.equal=TRUE,alternative="less")  # test for extreme rm in go
t.test(resultsMPT2[52:75,13],resultsMPT2[76:97,13],var.equal=TRUE,alternative="less")  # test for extreme rm in gn

t.test(resultsMPT2[52:75,22],resultsMPT2[76:97,22],var.equal=TRUE,alternative="less")  # test for extreme rm in go
t.test(resultsMPT2[52:75,25],resultsMPT2[76:97,25],var.equal=TRUE,alternative="less")  # test for extreme rm in gn
	
	
