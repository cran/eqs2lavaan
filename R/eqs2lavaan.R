eqs2lavaan <-
function(eqs,data=NULL,compact=TRUE)
{
	library(lavaan)
	library(stringr)
	options(warn=-1)
	#eqs		<- file.choose()
	if(length(data)==1)
	{
		data	<- c(data,data)
	}
	#eqs	<- "/Users/craig_m_k/Documents/hw5 uncorrelated 4 factor.out"
	#eqs	<- "/Users/craig_m_k/Documents/cfa4faccorr.out"
	#eqs	<- "/Users/craig_m_k/Documents/cfacorrelatedfullrun.out"
	#eqs	<- "/Users/craig_m_k/Documents/hw5 corr1 factor.out"
	#eqs	<- "/Users/craig_m_k/Documents/hw7 latent variable model.out"
	#eqs	<- "/Users/craig_m_k/Documents/n25sample.out"
	#eqs	<- "/Users/craig_m_k/Documents/URI/PSY 612/HW7/lvmsaturated.out"
	cd		<- FALSE
	comm	<- NULL
	if(str_detect(eqs,ignore.case(".out")))
	{
		info	<- out2lavaan(eqs)
		covi	<- info[[1]]
		desc	<- info[[2]]
		cd		<- info
		#data	<- FALSE
	}
	eqs		<- readLines(eqs, n=-1L)
	loc		<- grep("/",eqs)
	# LABELS
	l		<- which(loc==grep("/LAB", eqs))
	V		<- NULL
	W		<- NULL
	if(length(l)>0)
	{
		i		<- (loc[l]+1):(loc[l+1]-1)
		str		<- str_split(eqs[i], ";")
		for(j in 1:length(str))
		{
			s	<- str_trim(str[[j]])
			for(k in 1:length(s))
			{
				eq	<- as.numeric(str_locate(s[k], "=")[,2])
				if(is.na(eq)==FALSE)
				{
					x 	<- str_trim(str_sub(s[k],1,eq-1))
					y	<- str_locate(x,"V")[1,1]
					x	<- str_trim(str_sub(x,y))
					V	<- c(V,x)
					W 	<- c(W,str_trim(str_sub(s[k],eq+1)))
				}
			}
		}
	}
	# Equations
	l		<- which(loc==grep("/EQU", eqs))
	if(length(l)>0)
	{
		eq		<- NULL
		fs		<- NULL
		vs		<- NULL		
		repl	<- (loc[l]+1):(loc[l+1]-1)
		x		<- str_trim(eqs[repl])
		x		<- which(eqs[repl]=="")
		if(length(x)>0)
		{
			repl	<- repl[-x]
		}
		for(lp in 1:length(repl))
		{
			xx		<- str_trim(str_replace(eqs[repl[lp]],"D",""))[[1]]
			xx		<- str_trim(str_sub(xx,str_locate(xx," ")[,1]))
			xx		<- str_replace_all(xx,"E","")
			#xx		<- str_replace(x,";","")
			xx		<- str_replace_all(xx,"[*]","")
			xx		<- str_trim(str_split(xx,";")[[1]])
			xx		<- xx[which(nchar(xx)>0)]
			for(ii in 1:length(xx))
			{
				str		<- unlist(str_split(xx[ii],"="))
				lhs		<- unlist(str_split(str[2],"[+]"))
				lhs		<- str_trim(lhs[which(is.na(as.vector(lhs,"numeric")))])
				for(i in 1:length(lhs))
				{
					x	<- str_locate(lhs[i],"F")[,2]
					if(!is.na(x))
					{
						pre	<- NA
						if(x>1 && !is.na(str_locate(eqs[repl[lp]],"V")[,2]))
						{
							pre		<- str_sub(lhs[i],1,x-1)
							lhs[i]	<- str_sub(lhs[i],x)
						}
						if(x>1 && is.na(str_locate(eqs[repl[lp]],"V")[,2]))
						{
							pre		<- str_sub(lhs[i],1,x-1)
							lhs[i]	<- str_c(pre,str_sub(lhs[i],x),sep="*")
							pre		<- NA
						}
					}
				}
				lhs		<- str_trim(str_c(lhs,collapse=" + "))
				rhs		<- unlist(str_trim(str[1]))
				rhs		<- rhs[which(is.na(as.vector(rhs,"numeric")))]
				if(length(cd)>1 & length(grep(" ",rhs))>0)
				{
					rhs		<- str_split(rhs," ")
					rhs		<- rhs[which(nchar(rhs)>0)][-1]
				}
				if(!is.na(pre))
				{
					rhs	<- str_trim(str_c(pre,rhs,sep="*"))
				}
				if(is.na(pre))
				{
					rhs		<- str_trim(str_c(rhs,collapse="+"))
				}
				if(!is.na(str_locate(eqs[repl[lp]],"V")[,2]))
				{
					x	<- str_locate(rhs,"V")[,2]
					x	<- word(str_sub(rhs,x),1)
					if(length(V)==0)
					{
						y	<- x
					}
					if(length(V)>0)
					{
						y	<- W[which(V==x)]
					}
					rhs	<- str_replace(rhs,x,y)
					e	<- str_c(str_trim(lhs),str_trim(rhs),sep=" =~ ")
				}
				if(is.na(str_locate(eqs[repl[lp]],"V")[,2]))
				{
					e		<- str_c(str_trim(rhs),str_trim(lhs),sep="  ~ ")
				}
				fs		<- c(fs,lhs)
				vs		<- c(vs,rhs)
				#eq		<- c(eq,list(e))
				eq		<- c(eq,list(noquote(e)))
			}
		}
	}
	eqfull		<- eq
	if(compact==TRUE)
	{
		yy		<- NULL
		y		<- unique(fs)
		if(length(grep(" ~ ",eq))>0)
		{
			y		<- unique(fs[-grep(" ~ ",eq)])
			yy		<- eq[grep(" ~ ",eq)]
		}
		eq		<- NULL
		eq2		<- NULL
		zz		<- NULL
		for(i in 1:length(y))
		{
			man		<- which(fs==y[i])
			xx		<- str_c(vs[man],collapse=" + ")
			eq2		<- c(eq2,str_c(y[i],xx,sep=" =~ "))
			ast		<- grep("[*]",vs[man])
			if(str_sub(vs[ast],1,1)=="1" && length(ast)==1)
			{
				z		<- str_locate(vs[ast],"[*]")[2] + 1
				z		<- c(str_sub(vs[man[ast]],z),vs[man[-ast]])
				z		<- c(str_trim(z))
				zz		<- c(zz,z)
				z		<- str_c(z,collapse=" + ")
			}
			else{
				z		<- c(str_trim(vs[man]))
				zz		<- c(zz,z)
				z		<- str_c(z,collapse=" + ")
				aff		<- FALSE
				comm	<- NULL
			}
			eq		<- c(eq,str_c(y[i],z,sep=" =~ "))
		}
		eq		<- c(eq,yy)
		comm	<- c(comm,"auto.fix.first=TRUE")
	}
	# Variances
	l		<- which(loc==grep("/VAR", eqs))
	if(length(l)>0)
	{
		vari	<- NULL	
		repl	<- (loc[l]+1):(loc[l+1]-1)
		#x		<- str_trim(eqs[repl])
		x		<- which(eqs[repl]=="")
		if(length(x)>0)
		{
			repl	<- repl[-x]
		}
		if(length(which(length(grep(ignore.case("\f"),eqs[repl]))>0))!=0)
		{
			repl	<- repl[-grep(ignore.case("\f"),eqs[repl])]
		}
		if(length(which(length(grep(ignore.case("PAGE:"),eqs[repl]))>0))!=0)
		{
			repl	<- repl[-grep(ignore.case("PAGE:"),eqs[repl])]
		}
		if(length(which(length(grep(ignore.case("TITLE:"),eqs[repl]))>0))!=0)
		{		
			repl	<- repl[-grep(ignore.case("TITLE:"),eqs[repl])]
		}
		if(length(repl)>1 && repl[1]>repl[2])
		{
			repl	<- NULL
		}
		if(length(repl)>0)
		{
			for(lp in 1:length(repl))
			{
				xx		<- str_trim(eqs[repl[lp]])[[1]]
				xx		<- str_trim(str_sub(xx,str_locate(xx," ")[,1]))
				xx		<- str_replace_all(xx,"E","V")
				xx		<- str_replace_all(xx,"D","F")
				#xx		<- str_replace_all(xx,"E","")
				#xx		<- str_replace(x,";","")
				xx		<- str_replace_all(xx,"[*]","")
				xx		<- str_replace_all(xx,"=","")
				xx		<- str_trim(str_split(xx,";")[[1]])
				xx		<- xx[which(nchar(xx)>0)]
				#str		<- str_trim(str_split(eqs[repl[lp]],"=")[[1]])

				#str		<- str_trim(str_replace_all(str,";",""))
				#str		<- str_trim(str_replace_all(str,"[*]",""))
				#str		<- str_split(str," ")[[1]]
				#if(length(cd)>1)
				#{
				#	str		<- xx[which(nchar(xx)>0)][-1]
				#}
				#else{
				#	str		<- xx[which(nchar(xx)>0)]
				#}
				x		<- which(!is.na(as.numeric(xx)))
				if(length(x)>0)
				{
					xx		<- str_c(xx[1],xx[2],sep=" = ")
					vari	<- c(vari,list(xx))
				}
				if(length(x)==0)
				{
					for(ii in 1:length(xx))
					{
						x		<- which(V==xx[ii])
						if(length(x)>0)
						{
							xx[ii]		<- W[x]
						}
						if(length(grep(ignore.case("to"),xx[ii]))>0)
						{
							xx[ii]		<- str_trim(str_split(xx[ii],"to")[[1]])
							trp		<- NULL
							for(t in str_sub(xx[1],2):str_sub(xx[2],2))
							{
								trp		<- c(trp,str_c(str_sub(xx[1],1,1),t,sep=""))
							}
							xx[ii]		<- trp
						}
						vari	<- c(vari,list(str_c(xx[ii],xx[ii],sep=" ~~ ")))
					}	
				}
			}
		}
	}
	if(length(vari)==length(y)+length(vs))
	{
		comm	<- c(comm,"auto.var=TRUE","auto.cov.y=TRUE")
	}
	#if(length(vari)!=length(y)+length(vs))
	#{
#		eq		<- c(eq,vari)
#	}	
	# Covariances
	l		<- which(loc==grep("/COV", eqs))
	if(length(l)>0)
	{
		cova	<- NULL
		repl	<- (loc[l]+1):(loc[l+1]-1)
		x		<- str_trim(eqs[repl])
		x		<- which(eqs[repl]=="")
		if(length(x)>0)
		{
			repl	<- repl[-x]
		}
		if(length(which(length(grep(ignore.case("\f"),eqs[repl]))>0))!=0)
		{
			repl	<- repl[-grep("\f",eqs[repl])]
		}
		if(length(which(length(grep(ignore.case("PAGE:"),eqs[repl]))>0))!=0)
		{
			repl	<- repl[-grep(ignore.case("PAGE:"),eqs[repl])]
		}
		if(length(which(length(grep(ignore.case("TITLE:"),eqs[repl]))>0))!=0)
		{		
			repl	<- repl[-grep(ignore.case("TITLE:"),eqs[repl])]
		}
		if(length(repl)>1 & repl[2]-repl[1]<=0)
		{
			repl	<- NULL
		}
		if(length(repl)>0)
		{
			for(lp in 1:length(repl))
			{
				xx		<- str_trim(eqs[repl[lp]])
				xx		<- str_trim(str_sub(xx,str_locate(xx," "))[[1]])
				xx		<- str_trim(str_split(xx,";")[[1]])
				xx		<- xx[which(nchar(xx)>0)]
				xx		<- str_replace(xx,"[*]","")
				xx		<- str_trim(str_replace(xx,"=",""))
				
				#xx		<- str_trim(str_split(xx,"=")[[1]])
				#str		<- str_trim(str_split(str," ")[[1]])
				#if(length(which(nchar(xx)==0))>0)
				#{
			#		str		<- str[-which(nchar(str)==0)]
		#		}
		#		if(length(cd)>1)
		#		{
		#			str		<- str[-1]
		#		}
				for(ii in 1:length(xx))
				{
					str		<- str_trim(str_split(xx[ii],",")[[1]])
					if(length(grep("to",str))==0)
					{
						#str	<- str_replace(str[1],"E","V")
						str		<- str_replace(str,"D","F")
						x		<- str_locate(str,"F")[,2]
						f1		<- word(str_sub(str[1],x[1]),1)
						f1		<- str_trim(str_replace(f1,","," "))
						f1		<- str_trim(str_replace(f1,";"," "))
						f2		<- word(str_sub(str[2],x[2]),1)
						f2		<- str_trim(str_replace(f2,","," "))
						f2		<- str_trim(str_replace(f2,";"," "))
						cova	<- c(cova,list(str_c(f1,f2,sep=" ~~ ")))
					}
					if(length(grep("to",str))>0)
					{
						str		<- str_trim(str_split(str,"to")[[1]])
						trp		<- NULL
						for(t in str_sub(str[1],2):str_sub(str[2],2))
						{
							trp		<- c(trp,str_c(str_sub(str[1],1,1),t,sep=""))
						}
						str		<- trp
						str		<- str_replace(str,"D","F")
						for(t in 1:(length(str)-1))
						{
							for(u in (t+1):length(str))
							{
								cova	<- c(cova,list(str_c(str[t],str[u],sep=" ~~ ")))
							}
						}
					}
				}
			}
		}
	}
	if(length(cova)==0)
	{
		comm	<- c(comm,"orthogonal=TRUE")
	}
	lav			<- NULL
	# Compile
	code		<- str_c(c("e2l.mod <- '","# EQUATIONS #",unlist(eq),""),collapse=" \r ")
	if(length(grep("auto.var",comm))==0)
	{
		xx		<- str_c(c("# VARIANCES #",unlist(vari),""),collapse=" \r ")
		code	<- str_c(code,xx)
	}
	if(length(cova)>0)
	{
		xx		<- str_c(c("# COVARIANCES #",unlist(cova),""),collapse=" \r ")
		code	<- str_c(code,xx)
	}	    
	code		<- str_c(code,"'")                             
	write(noquote(code),file="EQSlavaan.txt", append=FALSE)
	code		<- str_c(c(unlist(eqfull),unlist(vari),
	                       unlist(cova)),collapse=" ; ")
	if(length(cd)>1 & length(data)<=1)
	{
		loc			<- agrep("CASES=",eqs)[1]
		x			<- str_locate(eqs[loc],ignore.case("CASES"))[,2]
		loc			<- str_sub(eqs[loc],x+1)
		loc			<- str_replace(loc,"=","")
		loc			<- as.numeric(str_trim(str_replace(loc,";","")))
		info		<- c(info,list(loc))
		cvm		<- str_c(as.vector(info[[1]],"numeric"),collapse=", ")
		cvm		<- str_c("matrix(c(",cvm,"), ncol=",dim(info[[1]])[2],")")
		v		<- str_c("'",colnames(info[[1]]),collapse="', ")
		v		<- str_c("c(",v,"')")
		cvm		<- str_c("","# Covariance matrix from EQS file.",
		                 "cvm <- ",cvm,"","colnames(cvm) <- ",v,
		                 "rownames(cvm) <- colnames(cvm)","",sep=" \r ")
		M		<- str_c(as.vector(info[[2]][,1],"numeric"),collapse=", ")
		M		<- str_c("M <- c(",M,")","")
		M		<- str_c("# Means of observed variables.",M,"",sep=" \r ")
		n		<- as.numeric(info[[3]])
		n		<- str_c("n <- ",n)
		n		<- str_c("# Number of observations.",n,"","",sep=" \r ")
		comm	<- str_c(comm,collapse=", ")
		comm	<- str_c("e2l.out <- lavaan(e2l.mod, sample.cov=cvm, sample.mean=M, sample.nobs=n, mimic='EQS', ",comm,")",sep="")
		appn		<- str_c("",cvm,M,n,comm,collapse=" \r ")
		write(noquote(appn),file="EQSlavaan.txt", append=TRUE)
		if(length(grep("auto.fix.first",comm))==0)
		{
			lav			<- lavaan(model=code,
								  sample.cov=covi,
								  sample.mean=desc[,1],
								  sample.nobs=loc,
								  mimic="EQS")
		}
		if(length(grep("auto.fix.first",comm))>0)
		{
			lav			<- lavaan(model=code,
								  sample.cov=covi,
								  sample.mean=desc[,1],
								  sample.nobs=loc,
								  auto.fix.first=TRUE,
								  mimic="EQS")
		}
	}
	#data			<- file.choose()
	if(length(data)>1)
	{
		if(is.vector(data[1],"character"))
		{
			data			<- read.csv(data[1], header=T)
			lav				<- lavaan(code,data=data,mimic="EQS")
		}
		if(!is.vector(data[1],"character"))
		{
			lav		<- lavaan(code,data=data,mimic="EQS")	
		}
	}
	if(length(lav)==0)
	{
		lav		<- code
	}
	i		<- intersect(grep("CHI-SQUARE",eqs),grep("DEGREES",eqs))
	if(length(i)>0)
	{
		chsq	<- as.numeric(str_trim(str_split(eqs[i[2]]," ")[[1]]))
		chsq	<- chsq[which(!is.na(chsq))]
		if(abs(chsq[1]-lav@Fit@test[[1]]$stat)>0.2)
		{
			lav		<- noquote("The results for this translation seem to not be measuring something correctly.  Please report the error and send a copy of the .out file to craigmk@my.uri.edu so this error can be corrected.")
			write(lav,file="EQSlavaan.txt", append=FALSE)
		}
	}
	if(length(i)==0)
	{
		lav		<- noquote("EQS file does not have a Chi-Square and likely has a matrix that is not positive definite.")
		write(lav,file="EQSlavaan.txt", append=FALSE)
	}
	return(lav)
}
