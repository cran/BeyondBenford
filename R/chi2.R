chi2 <-
function(dat,mod="ben",upbound=ceiling(max(dat)),dig=1,pval=0){

	prep<-function(dat){dat=as.data.frame(dat); rownb=dim(dat)[1]; colnb=dim(dat)[2];
	for (j in 1:colnb) if (is.numeric(dat[,j])==FALSE) dat[,j]=as.numeric(as.character(dat[,j])); 
	for (i in 1:rownb) for (j in 1:colnb) if (is.na(dat[i,j])==TRUE) dat[i,j]=0;
	return(dat);}

dat=prep(dat); size=sum(obs.numb.dig(dat,dig));
if (mod=="ben") {if(size*Benf.val(9,dig)<5) {return("Chi2 can not be applied: at least one insufficient theoretical frequency")} else {
		if (dig==1) {chi=0; for (fig in 1:9) {chi=chi+((obs.numb.dig(dat)/size)[fig]-Benf.val(fig))**2/Benf.val(fig)}; 			if (pval!=0) return(data.frame(chi2=c("Chi2 value is:",size*chi),pval=c("The p-value is:",1-pchisq(size*chi,8))))
		else return(data.frame(chi2=c("Chi2 value is:"),stat=c(size*chi)));}
		else {chi=0; for (fig in 1:10) {chi=chi+((obs.numb.dig(dat,dig)/size)[fig]-Benf.val(fig-1,dig))**2/Benf.val(fig-1,dig)};
		if (pval!=0) return(data.frame(chi2=c("Chi2 value is:",size*chi),pval=c("The p-value is:",1-pchisq(size*chi,9))))
		else return(data.frame(chi2=c("Chi2 value is:"),stat=c(size*chi))); }}}
	else {if (upbound<10**(dig-1)) print("Upbound does not have enough digits")
		else{if(size*Blon.val(upbound,9,dig)<5) {return("Chi2 can not be applied: at least one insufficient theoretical frequency")} else {
		if (dig==1) {chi=0; for (fig in 1:9) {chi=chi+((obs.numb.dig(dat,dig)/size)[fig]-Blon.val(upbound,fig,dig))**2/Blon.val(upbound,fig,dig)}; if (pval!=0) return(data.frame(chi2=c("Chi2 value is:",size*chi),pval=c("The p-value is:",1-pchisq(size*chi,8))))
		else return(data.frame(chi2=c("Chi2 value is:"),stat=c(size*chi)));}
		else {chi=0;for (fig in 1:10) {chi=chi+((obs.numb.dig(dat,dig)/size)[fig]-Blon.val(upbound,fig-1,dig))**2/Blon.val(upbound,fig-1,dig)}; if (pval!=0) return(data.frame(chi2=c("Chi2 value is:",size*chi),pval=c("The p-value is:",1-pchisq(size*chi,9))))
		else return(data.frame(chi2=c("Chi2 value is:"),stat=c(size*chi)));}}}}}
