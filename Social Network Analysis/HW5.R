library(data.table)
library(igraph)
library(foreach)
library(plm)

inv_de=fread('/Users/huangzm/Desktop/Social Network/HW5/investors_and_deals.csv')
inv=fread('/Users/huangzm/Desktop/Social Network/HW5/investor_details.csv')
deal = fread('/Users/huangzm/Desktop/Social Network/HW5/deal_details.csv')
comp = fread('/Users/huangzm/Desktop/Social Network/HW5/company_details.csv')

# PREPROCESSING ----------------------------------------------------------------

# filter 1990 onward
deal[,yy:=substring(Deal_Date,8,10)]
deal[,year:=ifelse(yy>50,paste("19",yy,sep=""),paste("20",yy,sep=""))]
deal[,year:=as.numeric(year)]
deal = deal[year>=1990]

# filter venture capital
inv = inv[Investor_Type=="Venture Capital"]
inv_deal = merge(inv_de,inv[,1],by.x="Investor_Id",by.y="InvestorId")
inv_deal = inv_deal[,1:6]

# get company_id and year
inv_deal = merge(inv_deal,deal[,c("DealId","year","CompanyId")],
                 by.x="Deal_Id",by.y="DealId",all.x=T)
inv_deal = inv_deal[!is.na(year)]

# generate links
inv_deal_full = merge(inv_deal,inv_deal[,c(1,2,4)],by="Deal_Id",allow.cartesian=T)
inv_deal_full = inv_deal_full[order(Investor_Id.x,Investor_Id.y,year)]
inv_deal_full = inv_deal_full[Investor_Id.x!=Investor_Id.y]

# drop old ties (5 years not renewed)
inv_deal_full[, lead.year:= shift(year,type='lead'),by=list(Investor_Id.x,Investor_Id.y)]
inv_deal_full[is.na(lead.year)]$lead.year = 0
inv_deal_full = inv_deal_full[lead.year-year<=5]

# calculate status
inv_deal_full[,total_deal:=.N,by=list(Investor_Id.x,Investor_Id.y,year)]
inv_deal_full[,total_lead:=sum(Lead_Investor.x),by=list(Investor_Id.x,Investor_Id.y,year)]
inv_deal_full[,status_entry:=cumsum(total_lead)/cumsum(total_deal),by=list(Investor_Id.x,Investor_Id.y)]

status = unique(inv_deal_full[,c('Investor_Id.x','Lead_Investor','Investor_Id.y',
                                 'year','year_total','year_lead')],                               by=c('Investor_Id.x','Investor_Id.y'))

iii = inv_deal_full[Lead_Investor.x>0,
                    c('Investor_Id.x','Investor_Id.y','year','status_entry')]

View(merge(iii,superiority_edge,by.x=c("Investor_Id.x",'Investor_Id.y','year'),
      by.y =  c("from","to",'year'),all.y=T))

unique(iii[order(Investor_Id.x,Investor_Id.y,-year)],
       by=c("Investor_Id.x",'Investor_Id.y','year'))


get_year_status = function(y){
  i = iii[year<2018]
  i = unique(i[order(Investor_Id.x,Investor_Id.y,-year)],
             by=c("Investor_Id.x",'Investor_Id.y'))
  i = i[,c(1,2,4)]
  names(i) = c("from","to","weight")
  list = unique(i$from)
  net = graph.data.frame(i,directed = F)
  status = power_centrality(net, exponent = .75)[c(list)]
  status_table = data.table(Investor_Id = list, year = y, status = status)
  return(status_table)}

# get_year_status(1997)

yearly_status = foreach(y = 1990:2018, .combine = rbind)%do%{get_year_status(y)}

names(comp)[1] = "CompanyId"
full_data = merge(inv_deal_full,comp,by="CompanyId")
full_data = full_data[order(Investor_Id.x,year)]

# Q1 (A) -----------------------------------------------------------------------

# get hhi
hhi_data = full_data[,c("Investor_Id.x","year","Primary_Industry_Code")]
hhi_data[,industry_cnt:=.N,by=list(Investor_Id.x,year,Primary_Industry_Code)]
hhi_uniq = unique(hhi_data, by=c("Investor_Id.x","year","Primary_Industry_Code"))

hhi_uniq[,year_total:=sum(industry_cnt),by=list(Investor_Id.x,year)]
hhi_uniq[,share:=industry_cnt/year_total*100]
library(hhi)
hhi_uniq[,hhi:=hhi(data.frame(share)),by=list(Investor_Id.x,year)]
hhi_year = unique(hhi_uniq,by=c("Investor_Id.x","year"))[,c(1,2,7)]

# add hhi to full data
full_data = merge(full_data,hhi_year,by=c("Investor_Id.x","year"))

# add first round control to full data
full_data[,first_round:=ifelse(year==min(year),1,0),by=CompanyId]

# add IT control to full data
full_data[,IT:=ifelse(Primary_Industry_Sector=="Information Technology",1,0)]

# add early stage control to full data
deal_type = unique(deal,by=c("DealId","Deal_Type_1"))[,c("DealId","Deal_Type_1")]
full_data = merge(full_data,deal_type,by.x="Deal_Id",by.y="DealId")
full_data[,early_stage:=ifelse(Deal_Type_1=="Early Stage VC"|Deal_Type_1=="Angel (individual)"|Deal_Type_1=="Seed Round"|Deal_Type_1=="Accelerator/Incubator",1,0)]

# add firm age control to full data
full_data[,age:=year-min(year),by=Investor_Id.x]


yearly_data = unique(full_data,by=c("Investor_Id.x","year"))
names(yearly_data)[2]="Investor_Id"
yearly_data = merge(yearly_data,yearly_status,by=c("Investor_Id","year"))

yearly_data[,lag_status:=shift(status,type='lag'),by=Investor_Id]
yearly_data[,lag_IT:=shift(IT,type='lag'),by=Investor_Id]
yearly_data[,lag_first_round:=shift(first_round,type='lag'),by=Investor_Id]
yearly_data[,lag_early_stage:=shift(early_stage,type='lag'),by=Investor_Id]

summary(plm(hhi~lag_status+I(lag_status^2)+age+lag_early_stage+lag_IT+year+
    lag_first_round,data=yearly_data,model = "within", effect = "individual"))

# the prior status has negative effect on the concentration level, 
# the higher the prior status, the more diversified


# Q1 (B) -----------------------------------------------------------------------

inv_ind = unique(full_data[,c("Investor_Id.x","year","Primary_Industry_Code")])

yearly_dist = function(y){
  k = inv_ind[year==y]
  k = k[order(Investor_Id.x),]
  spread_keys = dcast(k, Investor_Id.x  ~ Primary_Industry_Code)
  spread_keys = spread_keys[,-1]
  
  spread_keys[!is.na(spread_keys)] = 1
  spread_keys[is.na(spread_keys)] = 0
  
  spread_keys = as.matrix(spread_keys)
  spread_keys = matrix(as.numeric(spread_keys),ncol = ncol(spread_keys))
  
  dist = proxy::dist(spread_keys, method = "Jaccard",by_rows=F)
  dist_df = melt(as.matrix(dist), varnames = c("ind1", "ind2"))
  
  indid =  data.table(id = seq(1,length(unique(k$Primary_Industry_Code))),
                      industry = unique(k$Primary_Industry_Code))
  dist_df = merge(dist_df,indid,by.x="ind1",by.y="id")
  dist_df = merge(dist_df,indid,by.x="ind2",by.y="id")
  dist_df$year = y
  dist_df = dist_df[,c(3,4,5,6)]
  names(dist_df)[1] = "distance"
  return(dist_df)
}

yearly_dist(2000)
full_yearly_dist = foreach(y = 1990:2018, .combine = rbind)%do%{yearly_dist(y)}

inv_ind_dist = merge(inv_ind,inv_ind,by=c("Investor_Id.x","year"),allow.cartesian=T)
inv_ind_dist = inv_ind_dist[Primary_Industry_Code.x>Primary_Industry_Code.y]
inv_ind_dist = merge(inv_ind_dist,full_yearly_dist,
                     by.x=c("Primary_Industry_Code.x","Primary_Industry_Code.y","year"),
                     by.y=c("industry.x","industry.y","year"))

n = length(unique(inv_ind$Primary_Industry_Code))
names(inv_ind_dist)[4] = "Investor_Id"
inv_ind_dist[,yearly_dist:=1-(1/(1+sum(distance)/(.N))),by=list(Investor_Id,year)]

uniq_dist = unique(inv_ind_dist[,c(3,4,6)],by=c("Investor_Id","year"))

# add yearly dist to full data
yearly_data = merge(yearly_data,uniq_dist,by=c("Investor_Id","year"),all.x=T)
yearly_data[is.na(yearly_dist)]$yearly_dist = 0

q1b_data = yearly_data[,c('Investor_Id','year','yearly_dist','lag_status','age','lag_early_stage','lag_IT','lag_first_round')]
q1b_data = q1b_data[complete.cases(q1b_data)]

q1b_data[,mean_pred:=mean(lag_status,lag_status^2,age,lag_early_stage,lag_IT,
          lag_first_round),by=list(Investor_Id,year)]

q1b_data[,tmp:=1]
q1b_data[,Mundlak:=cumsum(mean_pred)/cumsum(tmp),by=list(Investor_Id)]

summary(glm(yearly_dist~lag_status+I(lag_status^2)+age+lag_early_stage+lag_IT+as.factor(year)+lag_first_round+Mundlak,data=q1b_data,family = quasibinomial(link = "logit")))
      