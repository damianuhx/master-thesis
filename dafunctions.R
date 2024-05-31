library("psych")
library("Hmisc")
library("multilevel")
library("ggplot2")
library('car')
library('plyr')
library('xtable')
library('swst')

makecat <- function (input, mins, maxs, vals)
{
	buffer<-c(1:length(input))
	buffer <- NA
	
	for (i in 1:length(input))
	{
		for (j in 1:length(mins))
		{
			if (input[i]>=mins[j] && input<=maxs[j])
			{
				buffer[i]<-vals[j]
			}
		}
	}
	return(buffer)
}

desctable<-function(name, var, alphavar, alpha=TRUE)
{
	buffer <- data.frame(scale=c(name))
	print(name)
	
	buffer$mean<-mean(na.omit(var))
	buffer$median<-median(na.omit(var))
	buffer$std<-sd(na.omit(var))
	buffer$min<-min(na.omit(var))
	buffer$max<-max(na.omit(var))
	buffer$sk<-skew(na.omit(var))
	buffer$kurt<-kurtosi(na.omit(var))
	if (alpha==TRUE){buffer$alpha<-cronbach(na.omit(alphavar))[1]}
	else {}
	
	print(buffer)
	return (buffer)
}

cortable <- function(hdata, vdata, method="spearman", sig=0)
{
	buffer <- data.frame(scale=names(vdata))


	for (i in 1:length(hdata[1,]))
	{
		#buffer <- data.frame(buffer, paste(hnames[i]) = NA)
		for (j in 1:length(vdata[1,]))
		{
			buffer[j,i+1]<-cor(hdata[,i], vdata[,j], use='pairwise.complete.obs', method = "spearman")
			names(buffer)[i+1]=names(hdata)[i]
		}
	}

	return (buffer)
}

 

meanscales <- function(data, sum=FALSE)
{
	buffer<- array(NA, dim=(c (length(data[,1]), length(data[1,]) )))
	
	
	for (j in 1:length(data[1,]))
	{
		for (i in 1:length(data[,1]))
		{
			value <- 0
			number<- 0
			for (k in 1: length(data[i,j]))
			{
				if (is.na(data[i,j][k])){}
				else
				{
				value <-value +(data[i,j][k])
				number<-number+1
				#print(data[i,j][k])
				}
			}
			if (number>0)
			{
				if (sum==FALSE)
				{
					buffer[i,j]<-(value/number)
				}
				else
				{
					buffer[i,j]<-value
				}
			}
		}
	}
	
	buffer<-as.data.frame(buffer)
	names(buffer)<-(names(data))
	return(buffer)
}


scale <- function(rawdata, namespos, namesneg, minus, plus)
{
	totallength<-length(namespos)+length(namesneg)

	buffer <- array(NA, dim=(c (length(rawdata[,1]), totallength )))
	
	for (i in 1:length(buffer[,1]))
	{
		for (k in 1:length(rawdata[1,]))
		{
			if (length(namespos)>0)
			{
				for (j in 1:length(namespos))
				{
					if (names(rawdata)[k]==namespos[j])
					{
						buffer[i,j]<-rawdata[i,k]-minus
						break;
					}
				}
			}
			if (length(namesneg)>0)
			{
				for (j in 1:length(namesneg))
				{
					if (names(rawdata)[k]==namesneg[j])
					{
						buffer[i,j+length(namespos)]<- -1*(rawdata[i,k]-minus)+plus	
						break;
					}
				}
			}
		}
	}
	return (buffer)
}

recode <- function(data, oldvalues, newvalues)
{
	if (length(oldvalues)==length(newvalues))
	{
		for (i in 1: length(oldvalues))
		{
			for (j in 1:length(data))
			{
					if (is.na(data[j])){}
					else
					{
						if (data[j]==oldvalues[i])
						{
							data[j]=newvalues[i]
						}
					}
				
			}
		}
	}
	else {print("#error: Recode cammand's oldvalues is not the same length as newvalues")}
	return(data)
}



makebool <- function(column, min0, max0, min1, max1)
{
	for (i in 1:length(column))
	{
		if (is.na(column[i])){}
		else
		{
			if (column[i]>=min0 && column[i]<=max0)
			{
				column[i]<-0
			}
			else if (column[i]>=min1 && column[i]<=max1)
			{
				column[i]<-1
			}
			else 
			{
				column[i]<-NA
			}
		}
	}	
	return (column)
}


nominal <- function(variable, values)
{
	buffer <- array(NA, dim=(length(variable)), length(values))
	for (i in 1:length(variable))
	{
		for (j in 1:length(values))
		{
			if (variable[i]==values[j])
			{
				buffer[i,j]==1
			}
			else 
			{
				buffer[i,j]==0
			}
		}
	}
}


 make2d <- function(matrix)
 {
 
 	buffer <- data.frame(first=c(1:length(matrix[,1,1])))

 	for (i in 1:length(matrix[1,,1]))
 	{
		for (j in 1:length(matrix[1,1,]))
 		{
 			buffer[paste("I", i, "V", j,sep="")]<-matrix[,i,j]
 		}
 	}
 return (buffer)
 }
 
 

selectmask <- function(input, row, value)
{
	for (i in 1:length(input[,1]))
	{
		if (input[i, row]==value)
		{
		input<-input[-i,]
		}
	}
return(input)
}


select <- function(input, mask)
{
j<-0
delbuffer<-0
len<-length(input[1,])

	for (i in 1:len)
	{
		if (mask[i-j]==0)
		{
			input<-input[,-(i-delbuffer)]
			delbuffer<-delbuffer+1
		}
		if (i-j>=length(mask))
		{
			j<-i
		}
	}
return(input)
}



#make frequencies per group table
groupfreqs<-function(input, group, min, max, scale)
{
	buffer<-data.frame(V1=c(1: (((max-min)/scale)+1) ))
	for (i in min(group):max(group))
	{
		buffer[,i]<-0
	}
	minj<-min/scale
	maxj<-max/scale
	for (j in c( minj : maxj ))
	{
	
		for (k in 1:length(input))
		{
			if (input[k]>=j*scale && input[k]<(j+1)*scale)
			{
				buffer[j-minj+1, group[k]]<-buffer[j-minj+1, group[k]]+1
			}
		}
	}
	return(buffer)
}

#draw freq per group
drawfreqs <- function (table, type, min, max, scale, colors, name, legendnames, file)
{
	
	png(filename=file, height=480, width=640, bg="white")

for (i in 1:length(table[1,]))
   	{
   		table[i]<-100*table[i]/sum(table[i])
   	}
   	
   	
   	
  if(type == 1)
  {
  	barplot(t(as.matrix(table)), main=name, ylab= "%",ylim=c(0,80), beside=TRUE, col=colors)

  }
  
  else{
	plot(table[[1]]/length(table[1]), lwd=0, type="l", ylim=c(0,80), xlim=c(1,max/scale))
   	for (i in 1:length(table[1,]))
   	{
   		lines(table[[i]], col=colors[i], lwd=3)	
   	}
  }

  
   legend("topright", legendnames, cex=1, col=colors, pch=21:23, lty=1:3);

   
	dev.off()
	
	return(0)
}