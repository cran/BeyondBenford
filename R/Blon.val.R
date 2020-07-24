Blon.val <-
function(lwbound=10**(dig-1),upbound,fig,dig=1,sd=0){
if (lwbound>upbound) return("lwbound must be less than upbound")
if (upbound<10**(dig-1)) return("upbound does not have enough digits")
if (lwbound<(10**(dig-1))) return("lwbound does not have enough digits")
count=0; prop_sum=0;
for (i in(lwbound:upbound)) {if (as.numeric(substr(i,dig,dig))==fig) {count=count+1;
								       prop_sum=prop_sum+count/(i-lwbound+1);}
                             else prop_sum=prop_sum+count/(i-lwbound+1);}


	if (sd==0) return(prop_sum/(upbound-lwbound+1))
        else return(data.frame(Blon.pro=c("The probability is:",prop_sum/(upbound-lwbound+1)),stan.dev=c("The standard deviation is:",Blon.val.sd(lwbound,upbound,fig,dig))))
    }
