digit.distr <-
function(dat,mod="ben",upbound=ceiling(max(dat)),dig=1,col=c("#FFFFAA","#AAFFAA"),colbl=c("#FFFFAA","#AAFFFF"),colbebl=c("#FFFFAA","#AAFFAA","#AAFFFF"),main="Distribution of digits",legend=TRUE,leg=c("Observed","Benford"),legbebl=c("Observed","Benford","Blondeau"),legbl=c("Observed","Blondeau")){

	prep<-function(dat){dat=as.data.frame(dat); rownb=dim(dat)[1]; colnb=dim(dat)[2];
	for (j in 1:colnb) if (is.numeric(dat[,j])==FALSE) dat[,j]=as.numeric(as.character(dat[,j])); 
	for (i in 1:rownb) for (j in 1:colnb) if (is.na(dat[i,j])==TRUE) dat[i,j]=0;
	return(dat);}

dat=prep(dat); ond=obs.numb.dig(dat,dig); size=sum(ond); 
if (mod=="ben" & dig==1) {val=c(ond[1],ond[2],ond[3],ond[4],ond[5],ond[6],ond[7],ond[8],ond[9],Benf.val(1)*size,Benf.val(2)*size,Benf.val(3)*size,Benf.val(4)*size,Benf.val(5)*size,Benf.val(6)*size,Benf.val(7)*size,Benf.val(8)*size,Benf.val(9)*size); type = c("1","2","3","4","5","6","7","8","9"); val = matrix(val,ncol=9, byrow=T); colnames(val) = type; dev.new();
			barplot(val, xlab="First digit",ylab="Frequency", beside=T, col=col, ylim=c(0,max(val)*1.05), main=main);box();
			if (legend==TRUE) legend("topright", inset=.05,leg, fill=col, horiz=TRUE, cex=0.8,bg="gray85")}
else if (mod=="ben") {val=c(ond[1],ond[2],ond[3],ond[4],ond[5],ond[6],ond[7],ond[8],ond[9],ond[10],Benf.val(0,dig)*size,Benf.val(1,dig)*size,Benf.val(2,dig)*size,Benf.val(3,dig)*size,Benf.val(4,dig)*size,Benf.val(5,dig)*size,Benf.val(6,dig)*size,Benf.val(7,dig)*size,Benf.val(8,dig)*size,Benf.val(9,dig)*size); type = c("0","1","2","3","4","5","6","7","8","9"); val = matrix(val,ncol=10, byrow=T); colnames(val) = type; dev.new();
			barplot(val, xlab=c("Digit ",dig),ylab="Frequency", beside=T, col=col, ylim=c(0,max(val)*1.05), main=main);box();
			if (legend==TRUE) legend("topright", inset=.05,leg, fill=col, horiz=TRUE, cex=0.8,bg="gray85")}
else if (upbound<10**(dig-1)) return("upbound does not have enough digits")
else if (mod=="ben&blo") {if (dig==1) {val=c(ond[1],ond[2],ond[3],ond[4],ond[5],ond[6],ond[7],ond[8],ond[9],Benf.val(1)*size,Benf.val(2)*size,Benf.val(3)*size,Benf.val(4)*size,Benf.val(5)*size,Benf.val(6)*size,Benf.val(7)*size,Benf.val(8)*size,Benf.val(9)*size,Blon.val(upbound,1)*size,Blon.val(upbound,2)*size,Blon.val(upbound,3)*size,Blon.val(upbound,4)*size,Blon.val(upbound,5)*size,Blon.val(upbound,6)*size,Blon.val(upbound,7)*size,Blon.val(upbound,8)*size,Blon.val(upbound,9)*size); type = c("1","2","3","4","5","6","7","8","9"); val = matrix(val,ncol=9, byrow=T); colnames(val) = type; dev.new();
				barplot(val, xlab="First digit",ylab="Frequency", beside=T, col=colbebl, ylim=c(0,max(val)*1.05), main=main);box();
				if (legend==TRUE) legend("topright", inset=.05,legbebl, fill=colbebl, horiz=TRUE, cex=0.8,bg="gray85")} 
			else {val=c(ond[1],ond[2],ond[3],ond[4],ond[5],ond[6],ond[7],ond[8],ond[9],ond[10],Benf.val(0,dig)*size,Benf.val(1,dig)*size,Benf.val(2,dig)*size,Benf.val(3,dig)*size,Benf.val(4,dig)*size,Benf.val(5,dig)*size,Benf.val(6,dig)*size,Benf.val(7,dig)*size,Benf.val(8,dig)*size,Benf.val(9,dig)*size,Blon.val(upbound,0,dig)*size,Blon.val(upbound,1,dig)*size,Blon.val(upbound,2,dig)*size,Blon.val(upbound,3,dig)*size,Blon.val(upbound,4,dig)*size,Blon.val(upbound,5,dig)*size,Blon.val(upbound,6,dig)*size,Blon.val(upbound,7,dig)*size,Blon.val(upbound,8,dig)*size,Blon.val(upbound,9,dig)*size); type = c("0","1","2","3","4","5","6","7","8","9"); val = matrix(val,ncol=10, byrow=T); colnames(val) = type; dev.new();
				barplot(val, xlab=c("Digit ",dig),ylab="Frequency", beside=T, col=colbebl, ylim=c(0,max(val)*1.05), main=main);box();
				if (legend==TRUE) legend("topright", inset=.05,legbebl, fill=colbebl, horiz=TRUE, cex=0.8,bg="gray85")}}
else {if (dig==1) {val=c(ond[1],ond[2],ond[3],ond[4],ond[5],ond[6],ond[7],ond[8],ond[9],Blon.val(upbound,1)*size,Blon.val(upbound,2)*size,Blon.val(upbound,3)*size,Blon.val(upbound,4)*size,Blon.val(upbound,5)*size,Blon.val(upbound,6)*size,Blon.val(upbound,7)*size,Blon.val(upbound,8)*size,Blon.val(upbound,9)*size); type = c("1","2","3","4","5","6","7","8","9"); val = matrix(val,ncol=9, byrow=T); colnames(val) = type; dev.new();
		barplot(val, xlab="First digit",ylab="Frequency", beside=T, col=colbl, ylim=c(0,max(val)*1.05), main=main);box();
		if (legend==TRUE) legend("topright", inset=.05,legbl, fill=colbl, horiz=TRUE, cex=0.8,bg="gray85")} 
else {val=c(ond[1],ond[2],ond[3],ond[4],ond[5],ond[6],ond[7],ond[8],ond[9],ond[10],Blon.val(upbound,0,dig)*size,Blon.val(upbound,1,dig)*size,Blon.val(upbound,2,dig)*size,Blon.val(upbound,3,dig)*size,Blon.val(upbound,4,dig)*size,Blon.val(upbound,5,dig)*size,Blon.val(upbound,6,dig)*size,Blon.val(upbound,7,dig)*size,Blon.val(upbound,8,dig)*size,Blon.val(upbound,9,dig)*size); type = c("0","1","2","3","4","5","6","7","8","9"); val = matrix(val,ncol=10, byrow=T); colnames(val) = type; dev.new();
barplot(val, xlab=c("Digit ",dig),ylab="Frequency", beside=T, col=colbl, ylim=c(0,max(val)*1.05), main=main);box();
if (legend==TRUE) legend("topright", inset=.05,legbl, fill=colbl, horiz=TRUE, cex=0.8,bg="gray85")}}}
