Blon.val.sd <-
function(lwbound=10**(dig-1),upbound,fig,dig=1){
if (lwbound>upbound) return("lwbound must be less than upbound")
if (upbound<10**(dig-1)) return("upbound does not have enough digits")
if (lwbound<(10**(dig-1))) return("lwbound does not have enough digits")
count=0; var_sum=0;
     for (i in(lwbound:upbound)) {if (as.numeric(substr(i,dig,dig))==fig) {count=count+1;
								       var_sum=var_sum+count*(i-lwbound+1-count)/(i-lwbound+1)**2;}
                                  else var_sum=var_sum+count*(i-lwbound+1-count)/(i-lwbound+1)**2;
                                 }
    return(var_sum**0.5/(upbound-lwbound+1))
}
