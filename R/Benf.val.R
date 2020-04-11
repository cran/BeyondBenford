Benf.val <-
function(fig,dig=1){if(dig==1) {if (fig==0) return("First digit is all but 0") else return(log(1+1/fig,10))} 
			else {k=0; for (i in 10**(dig-2):(10**(dig-1)-1)) k=k+log(1+1/(10*i+fig),10); return(k);}}
