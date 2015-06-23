`ADM3` <- function(file, outfile, t=6, np=3, mr=0.3, autoCut=F, score=F, scale=0) {
	if(scale<0 | scale>1) { stop("scale must be between 0 and 1"); }
	rep=NULL; rr=NULL; data=read.table(file); u=unique(data[,1]);
	if(length(data[1,])<5 & !score) { data=cbind(data,1); }
	if(length(data[1,])<5 & score) { stop("require score in column 5"); }
	if(length(data[1,])==5 & !score) { data[,5]=1; }
	data[,5]=scale+(data[,5]*(1-scale));
	for(y in 1:length(u)) {
		print(y); d=data[data[,1]==u[y],]; re=NULL;		  
		  res <- .C("_F",
			"d" =  as.double( r <- .C("_P"
							,"dat" = as.double(d[,4])
							,"prob" = as.double(d[,5])
		 					,"threshold" = as.double(t)
		 					,"size" = as.integer(length(d[,4]))
		 					,"PACKAGE" = "ADM3")$dat
		 					)
			,"st"= as.double(rep(0,length(d[,1])))
			, "so"= as.double(rep(0,length(d[,1])))
			,"threshold" = as.double(t)
			,"size" = as.integer(length(r))
			,"PACKAGE" = "ADM3"
			); 
			ind1 = res$st[res$st!=0]; ind2 = res$so[res$so!=0];
			if(length(ind1)>0 & length(ind1) == length(ind2)) {
				for(x in 1:length(ind1)) {
					if(ind1[x]>0 & ind2[x]>0 & ind1[x]<=length(d[,1]) & ind2[x]<=length(d[,1]) & ind1[x]<=ind2[x]) {
						ind=mapBreak(d, ind1[x], ind2[x], mr);
						re = rbind(re, cbind(d[ind[1],1],  d[ind[1],2],  d[ind[2],3], mean(d[ind[1]:ind[2],4]), median(r[ind[1]:ind[2]]), (ind[2]-ind[1])+1, ind[1], ind[2]))
					}
				};	
				if(autoCut) {
					rep = rbind(rep, re[abs(re[, 4]) > getCut(dLRs(d[,4]), mr) & abs(re[,5])>t & re[,6] > np, ]);
           		} else {
           			rep = rbind(rep, re[abs(re[,4])>0.3 & abs(re[,5])>t & re[,6]>3,]);
           		}
			}; rr=c(rr,res$d)
	}
	l=list(); l$data=cbind(data,rr); l$report=rep;
	write.table(l$data, file=paste(substr(outfile, 1, nchar(outfile)-4), "_data.txt", sep=""), sep="\t", row.names=F, col.names=F, quote=F)
	write.table(l$report, file=paste(substr(outfile, 1, nchar(outfile)-4), "_report.txt", sep=""), sep="\t", row.names=F, col.names=F, quote=F)
	return(l)
}
	
