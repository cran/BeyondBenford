dat.distr <-
function(dat,xlab="data",ylab="Frequency",main="Distribution of data",theor=TRUE,nclass=50,col="lightblue",conv=0,upbound=ceiling(max(dat)),dig=1,colt="red",ylim=NULL,border="blue",nchi=0,legend=TRUE,bg.leg="gray85"){

	prep<-function(dat){dat=as.data.frame(dat); rownb=dim(dat)[1]; colnb=dim(dat)[2];
	for (j in 1:colnb) if (is.numeric(dat[,j])==FALSE) dat[,j]=as.numeric(as.character(dat[,j])); 
	for (i in 1:rownb) for (j in 1:colnb) if (is.na(dat[i,j])==TRUE) dat[i,j]=0;
	return(dat);}

dat=prep(dat); rownb=dim(dat)[1]; colnb=dim(dat)[2]; num.elig.val=0;
if (conv==1) {min=max(abs(dat)); for (i in 1:rownb) for (j in 1:colnb) if (dat[i,j]!=0) if (abs(dat[i,j])<min) min=abs(dat[i,j]);
	k=0; while(min*10**k<1) k=k+1; dat=10**k*dat;};
for (i in 1:rownb) for (j in 1:colnb) if (dat[i,j]>=10**(dig-1)) num.elig.val=num.elig.val+1; if (num.elig.val==0) return("No eligible value");
data=rep(0,num.elig.val); l=1; for (i in 1:rownb) for (j in 1:colnb) if (dat[i,j]>=10**(dig-1)) {data[l]=dat[i,j]; l=l+1;}
width=(max(data)-min(data))/nclass; if (width==0) return("Be careful, all selected data are equal"); dev.new(); 
if (theor==TRUE) {hist(data,breaks=(c(0:nclass)*width+min(data)),main=main,xlab=xlab,xlim=c(10**(dig-1),max(upbound,max(dat))),ylim=ylim,ylab=ylab,col=col,border=border);
		if (upbound>=10**(dig-1)) lines(c(rep(0,10**(dig-1)),theor.distr.val(upbound=upbound,dig)*num.elig.val*width),col=colt,type="p",pch=20,cex=0.2) 
		else print("Upbound does not have enough digits"); 
		box();
		if (legend==TRUE) legend("topright", legend=c("Ideal theoretical distribution"),col=colt,inset=.05, lty=1:2, cex=0.8,bg=bg.leg)}
else {hist(data,breaks=(c(0:nclass)*width+min(data)),main=main,xlab=xlab,ylab=ylab,col=col,border=border);box();}
if (nchi!=0) {max=max(data,upbound); freq.obs=rep(0,nchi); 
	for (k in 1:num.elig.val) if (data[k]==max) freq.obs[nchi]=freq.obs[nchi]+1 
				else freq.obs[floor(((data[k]-10**(dig-1))/(max-10**(dig-1)))*nchi)+1]=freq.obs[floor(((data[k]-10**(dig-1))/(max-10**(dig-1)))*nchi)+1]+1;
	Blon.class=rep(0,nchi); count=0; k=10**(dig-1); tdv=theor.distr.val(upbound=upbound,dig=dig); 
	while (count<(nchi-1)) {	
		while (k<(10**(dig-1)+(count+1)*(max-10**(dig-1))/nchi) & k<=upbound) {Blon.class[count+1]=tdv[k-10**(dig-1)+1]*num.elig.val+Blon.class[count+1]; k=k+1;}; count=count+1;}
	if (k<=upbound) for (l in k:upbound) Blon.class[nchi]=tdv[l-10**(dig-1)+1]*num.elig.val+Blon.class[nchi]; 
	print(c("Class freq.: ",freq.obs)); print(c("Theor. freq.:",Blon.class));
	if(Blon.class[nchi]<5) return("Chi2 can not be applied: at least one insufficient theoretical frequency") 
	else {chi=0; for (i in 1:nchi) {chi=chi+(freq.obs[i]-Blon.class[i])**2/Blon.class[i]}; 			
	return(data.frame(chi2=c("Chi2 value is:",nchi*chi),pval=c("The p-value is:",1-pchisq(nchi*chi,nchi-1))))}}}
