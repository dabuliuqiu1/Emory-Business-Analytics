---
title: "HW2"
output: pdf_document

---
```{r,warning=FALSE}
library(readxl)
library(readr)
library(dplyr)
library(svMisc)
library(data.table)
library(igraph)
library(lubridate)
library(corrplot)
library(pscl)
library(stats)
library(cluster)
library(factoextra)
library(survival)

event = fread('/Users/huangzm/Desktop/Social Network/HW2/Funding_events_7.14 1.csv')
event2 = read_excel("/Users/huangzm/Desktop/Social Network/HW2/Funding_events_7.14_page2.xlsx")
setDT(event2)
setDT(event)
colnames(event2) = colnames(event)
```

```{r,warning=FALSE}
# *****************************************************************
# I found out that some dates in the FIRST CSV are in 
# m-d-yy format while some are yyyy-m-d. 
# so I first unified the date column to yyyy-mm-dd in first CSV
# THIS WILL MAKE MY RESULTS A BIT DIFFERENT BUT I BELIEVE THEY ARE CORRECT
# *****************************************************************
event[1:5]$`Deal Date`
```
```{r,warning=FALSE}
# unified the date column to yyyy-mm-dd in first CSV

# raw_links1=fread("/Users/huangzm/Desktop/Social Network/HW2/raw1.csv")

if (exists("events_VC")==FALSE){ # if I've run it before don't run this again
for (i in 1:nrow(event)){
  term_1 = strsplit(toString(event[i,"Deal Date"]),"/")[[1]][1]
  term_2 = strsplit(toString(event[i,"Deal Date"]),"/")[[1]][2]
  term_3 = strsplit(toString(event[i,"Deal Date"]),"/")[[1]][3]
  if (as.numeric(term_1)<100){ # if m/d/yy
    if (as.numeric(term_3)>50){ # if y < 2000
      event[i,"Deal Date"] = paste(paste("19",term_3,sep=""),
                                   term_1,term_2,sep="/")}
    else {event[i,"Deal Date"] = paste(paste("20",term_3,sep=""),
                                       term_1,term_2,sep="/")}
  }
  #print(i)
}
event$`Deal Date` = as.Date(event$`Deal Date`, format = "%Y/%m/%d")
event2$`Deal Date` = as.Date(event2$`Deal Date`, format = "%Y/%m/%d")

# combine the two event files
events = data.table(rbind(event,event2))

# filter the events with more than one VC
events_VC = dplyr::filter(events, grepl(', ', Investors))
}
```

```{r,warning=FALSE}

if (exists("raw_links1")==FALSE){
raw_links1 = data.table(VC1="",VC2="",`Deal Date`=as.Date("2000-01-01"),
                        `DealSize (USD mn)`=1)
for (t in 1:nrow(events_VC)){
  investors = as.list(strsplit(toString(events_VC[t,"Investors"]),", ")[[1]])
  for (i in 1:(length(investors)-1)){
    for(j in (i+1):length(investors)){
      raw_links1 = rbind(raw_links1, 
                         data.table(VC1 = toString(investors[i]),
                                    VC2 = toString(investors[j]),
                                    events_VC[t,"Deal Date"],
                                    events_VC[t,"DealSize (USD mn)"]),
                         fill=TRUE)}}}
  
colnames(raw_links1) = c("VC1","VC2","Date","Size")
raw_links1 = raw_links1[2:nrow(raw_links1)]

# fill NA size with the mean
raw_links1[is.na(raw_links1$Size)]$Size = mean(raw_links1[Size>0]$Size)

# drop invalid ties
invalid_name = c("Inc.","Inc","INC",
                 "Ltd.","Ltd","LTD",
                 "LLC.","LLC","L.L.C","llc",
                 "L.P.","LP.","LP")
for (name in invalid_name){raw_links1 = raw_links1[VC1!=name & VC2!=name]}}

write.csv(raw_links1, file="/Users/huangzm/Desktop/Social Network/HW2/raw1.csv")
```

```{r,warning=FALSE}

# a function to clean the decayed ties with given date
trim_links = function(links,date_now){
  
  # get all the renewal windows by Dislocation Subtraction
  links = links[order(VC1,VC2,Date),]
  t1 = links[1:(nrow(links)-1),]
  t2 = links[2:nrow(links),]
  t1$diff = as.numeric(t2$Date - t1$Date)
  t1$VC11 = t2$VC1
  t1$VC22 = t2$VC2
  
  # if no ties occurs more than 3 times then keep all the ties
  if (nrow(unique(t1[VC1==VC11 & VC2==VC22]))<=3){
    uniq_trim = links
    uniq_trim$diff = date_now - uniq_trim$Date}
  # else trim the decay
  if (nrow(unique(t1[VC1==VC11 & VC2==VC22]))>3){
    windows = t1[VC1==VC11 & VC2==VC22]$diff
    
    # use the 90% threshold to trim
    renewal_standard = sort(windows,decreasing=F)[ceiling(0.9*length(windows))]
    links$diff = date_now-links$Date
    
    # drop duplicate in trimmed links
    links_trim = links[diff<=renewal_standard]
    links_trim = links_trim[order(VC1,VC2,-Date),]
    uniq_trim = unique(links_trim, by = c("VC1","VC2"))
    }
  return(uniq_trim)
}

uniq_trim = trim_links(raw_links1,max(raw_links1$Date)+1)
```

```{r,warning=FALSE}

# QUESTION 1 --------------------------------------------------------

VCnet = graph.data.frame(uniq_trim[,1:2],directed = F)
sort(closeness(VCnet),decreasing=T)[1]
# Intel Capital is at the center
```

```{r,warning=FALSE}

# QUESTION 2 --------------------------------------------------------

# generate month list and coreness list
months = seq(1,interval(min(uniq_trim$Date), max(uniq_trim$Date)) 
             %/% months(1)+1)

cores = c()
for (i in year(min(raw_links1$Date)):year(max(raw_links1$Date))){
  if (i==year(min(raw_links1$Date)))
    {month_list=seq(month(min(raw_links1$Date)),12)}
  else if (i==year(max(raw_links1$Date)))
    {month_list=seq(1,month(max(raw_links1$Date)))}
  else
  {month_list=seq(1,12)}
  for (j in month_list){
    max_day = days_in_month(paste(toString(i),toString(j),"1",sep="-"))
    max_date = paste(toString(i),toString(j),toString(max_day),sep="-")
    tmp = raw_links1[Date<=as.Date(max_date)]
    tmp_trim = trim_links(tmp,as.Date(max_date)+1)
    net = graph.data.frame(tmp_trim[,1:2],directed = F)
    cores = c(cores,mean(coreness(net)))
    #print(c(i,j))
  }
}
plot(seq(1,length(cores)),cores,xlab="months",type="l")
```

```{r,warning=FALSE}

# QUESTION 3.A --------------------------------------------------------
mons = seq(as.Date("1985-06-30"),as.Date("2014-06-30"),by="years")

for (m in 1:length(mons)){
  cor=c(0)
  tmp = raw_links1[Date<=mons[m]]
  tmp_trim = trim_links(tmp,mons[m])
  net = graph.data.frame(tmp_trim[,1:2],directed = F)
  C = sort(eigen_centrality(net)$vector)
  for (p in 1:(length(V(net))-1)){
    Cp = c(rep(0, length(V(net))-p), rep(1, p))
    cor = c(cor,cor(C,Cp))}
  par(pty="s",mfrow=c(1,1))
  plot(seq(1,length(cor)),cor,xlab="p",ylab="Concentration Score",
       type = "b",main = toString(mons[m]))
  Sys.sleep(1)
}
```
```{r,warning=FALSE}

# QUESTION 3.B --------------------------------------------------------
mons = seq(as.Date("1985-06-30"),as.Date("2014-06-30"),by="years")
for (m in seq(1,length(mons),3)){
  cor=c(0)
  tmp = raw_links1[Date<=mons[m]]
  tmp_trim = trim_links(tmp,mons[m])
  net = graph.data.frame(tmp_trim[,1:2],directed = F)
  plot(cmdscale(dist(as_adjacency_matrix(net)),2),xlab="",ylab="",
       main=paste("2-Dimensional Graph",toString(mons[m])))
  Sys.sleep(1)
}
# due to my laptop's power, it automatically stop calculating after 2003, but 
# there is already clear pattern of core-periphery developed through the years
```


```{r,warning=FALSE}

# QUESTION 4 --------------------------------------------------------

m = as.Date("1996-06-30")
tmp = raw_links1[Date<=m]
tmp_trim = trim_links(tmp,m)
net = graph.data.frame(tmp_trim[,1:2],directed = F)

sil = c()
for (k in 1:(length(V(net))-1)){
  pam = pam(dist(as_adjacency_matrix(net)),k=k)
  sil = c(sil,pam$silinfo$avg.width)
}

plot(seq(1,length(sil)),sil)
# which(sil==max(sil))
# the optimal k = 9

opt_pam = pam(dist(as_adjacency_matrix(net)),k=9)
plot(silhouette(opt_pam,dist(as_adjacency_matrix(net))))
fviz_silhouette(opt_pam, label=F)
```

```{r,warning=FALSE}

# BONUS  ------------------------------------------------------------

# 1. SUCCESSFUL INVESTMENT  ----------------------------------------------------

success = fread('/Users/huangzm/Desktop/Social Network/HW2/Venture_capital_firm_outcomes.csv')

test_success = function(year){
  
  # generate network for the given year
  trim_0 = raw_links1[Date<as.Date(paste(toString(year),"1","1",sep="-"))]
  trim_1 = trim_links(trim_0,as.Date(paste(toString(year),"1","1",sep="-")))
  
  # weight = deal size*100 / time difference between last update and now
  trim_1[,weight := Size*100/as.numeric(trim_1$diff, units="days")]
  trim_1[trim_1$weight<=0] = 0.0000001
  net1 = graph.data.frame(trim_1[,c(1,2,6)],directed = F)
  success_inv = data.table(success[year==year],key="firm_name")
  
  # get centralities
  degree = data.table(degree = degree(net1,normalized = T)
                      [c(success_inv$firm_name)])
  closeness = data.table(closeness = closeness(net1,mode = "all")
                         [c(success_inv$firm_name)])
  betweeness = data.table(betweeness = betweenness(net1,normalized=T)
                          [c(success_inv$firm_name)])
  PageRank = data.table(PageRank = page_rank(net1)$vector
                        [c(success_inv$firm_name)])
  Eigen = data.table(Eigen = eigen_centrality(net1)$vector
                     [c(success_inv$firm_name)])
  
  data_success = cbind(success = success_inv$successful_investments,degree,closeness,betweeness,PageRank,Eigen)
  
  # cor(data_success,use = "complete.obs")
  # corrplot(cor(data_success,use = "complete.obs"))
  
  # from the corrplot, generally closeness & eigen have almost no correlation with success in most of the years, and if I include them all, there will be serious multi-collineartiy, so I ignored these two in the model construction
  
  # zero-inflated model is more appropriate here because there are excess zeros but they are not structural 
  zl = zeroinfl(success ~degree+PageRank+betweeness, data_success)
  coef = summary(zl)$coefficients
  
  # get the variables with >95% significance and their coefficients
  coef$count = coef$count[which(coef$count[, 4] < 0.05),]
  coef$zero = coef$zero[which(coef$zero[, 4] < 0.05),]
  print("significant variables in count model and zero model:")
  return(coef)
}
test_success(1990)
```

```{r,warning=FALSE}
test_success(1993)
```

```{r,warning=FALSE}
test_success(1995)
```

```{r,warning=FALSE}
test_success(2000)
```

```{r,warning=FALSE}
test_success(2005)
```

```{r,warning=FALSE}
test_success(2010)
```

```{r,warning=FALSE}
test_success(2014)
```

# the result gives: 
# in the early years, centrality have some negative effect on the # of success. This is probably because in the early stages the number of observation is too small. After 2000, centrality tend to have obvious positive impact on investment success: the more central a VC is, the less likely it will have zero successful investment, the more likely it have more successful investment than those who are not central. 

```{r,warning=FALSE}

# 2. SURVIVAL ANALYSIS  ----------------------------------------------------

# get all centrality
degree = degree(VCnet,normalized = T)[c(success$firm_name)]
closeness = closeness(VCnet,mode = "all")[c(success$firm_name)]
betweeness = betweenness(VCnet,normalized=T)[c(success$firm_name)]
PageRank = page_rank(VCnet)$vector[c(success$firm_name)]
Eigen = eigen_centrality(VCnet)$vector[c(success$firm_name)]

y = Surv(success$year,success$out_of_business)
x = data.table(degree,closeness,betweeness,PageRank,Eigen)
form = formula(y ~ degree+closeness+betweeness+PageRank+Eigen)
cox_bmt = coxph(form,data = y)
summary(cox_bmt)
# the result shows that all the centrality is significant and the more 
# central a VC is, the more likely it can survive
```






