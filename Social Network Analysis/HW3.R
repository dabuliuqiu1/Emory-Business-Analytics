library(data.table)
library(PearsonDS)
library(foreach)
library(ggplot2)
library(plm)
library(pglm)

rain=fread('/Users/huangzm/Desktop/Social Network/HW3/monthly_rainfall.csv')
election=fread('/Users/huangzm/Desktop/Social Network/HW3/election_results.csv')
nei=fread('/Users/huangzm/Desktop/Social Network/HW3/border_information.csv')

# clean data ------------------------------------------------------------------
rain$year = floor(rain$time)
rain[, rain:=sum(rainfall), by=c("district","year")]
rain = unique(rain, by = c('district','rain'))
rain[, count:=.N, by=district]
rain[rain==0]$rain = 0.0000001
rain = rain[count>1] # delete districts with 1 occurance

# calculate SPI
Mean = mean(rain$rain)
Var = var(rain$rain)
Scale = Var/Mean
Shape = Mean^2/Var
rain[, Pear:=qpearsonIII(pnorm(rain,mean=mean(rain,na.rm=T),sd=sd(rain,na.rm=T)),shape=Shape,location=0,scale=Scale)]
rain[, PearMean:=mean(Pear,na.rm=T), by=district]
rain[, PearSD:=sd(Pear,na.rm=T), by=district]
rain[, SPI:=(Pear-PearMean)/PearSD]
rain_spi = rain[,c("district","year","rain","SPI")]


# Q1.A -------------------------------------------------------------------------

# create period list
period = data.table(year = unique(election$year), period = seq(1,length(unique(election$year))))
period$interval = 0
for (i in 1:nrow(period)){
  period[i,3] = period[i,1] - max(1946,as.numeric(period[i-1,1]),na.rm=T)
}

full_period = data.table(year=seq(1946,1999), period=-1)
for (i in nrow(full_period):1){
  after = min(period[year>=full_period[i,year]]$year)
  before = max(max(period[year<=full_period[i,year]]$year),0)
  full_period[year<=after & year>=before]$period = period[year==after]$period
}

# label election data with period
dist_names = unique(election$district)

election_data = foreach(i = 1:length(dist_names), .combine = rbind)%do%{
  if (nrow(election[district==dist_names[i]])>0){
  m = merge(election[district==dist_names[i]],period,by=c("year"),all=T)
  m$district = dist_names[i]
  m}}

dist_names = unique(rain_spi$district)

# label spi data with period
spi_data = foreach(i = 1:length(dist_names), .combine = rbind)%do%{
  if (nrow(rain_spi[district==dist_names[i]])>0){
    m = merge(rain_spi[district==dist_names[i]],full_period,by=c("year"),all=T)
    m$district = dist_names[i]
    m}}

spi_data = spi_data[year<=1999]

# get number of founded for each district 
election_data = election_data[order(district,year,party_name),]
uniq = unique(election_data,by=c("district","party_name"))
num_found = uniq[,.N, by=c("district","year","period")]
names(num_found)[4] = "num_founded"

# get average spi district 
period_spi = spi_data[,mean(SPI), by=c("district","period")]
names(period_spi)[3] = "spi"

# generate scatter plot
found_spi = merge(num_found,period_spi,by=c("district","period"))

plot(found_spi$spi, found_spi$num_founded, main="Scatterplot",
     xlab="SPI", ylab="Number of parties founded", pch=19)


# Q1.B -------------------------------------------------------------------------

# get all neighbor data
nei1 = nei
nei2 = nei[,c("district","focal_district")]
colnames(nei2) = c("focal_district","district")
nei_total = rbind(nei1,nei2)
nei_total = nei_total[order(focal_district,district)]
nei_total = unique(nei_total,by=c("district","focal_district"))
colnames(nei_total) = c("district","neighbor")

# get all neighbor spi for each district
nei_spi = merge(nei_total,period_spi,by="district",all=T,allow.cartesian=TRUE)
nei_spi = nei_spi[order(district),]
nei_spi = nei_spi[!is.na(period)]

nei_spi_full = (merge(x=period_spi, y=nei_spi, by.x=c("district","period"), by.y=c("neighbor","period")))
colnames(nei_spi_full) = c("district","period","spi","neighbor","nei_spi")

nei_spi_full[,nei_avg_spi:=mean(nei_spi,na.rm=T),by=c("district","period")]
nei_spi_data = unique(nei_spi_full,by=c("district","period"))

# modeling
n = nrow(nei_spi_data)
nei_spi_data$lag_focal = c(0,nei_spi_data[1:(n-1)]$spi)
nei_spi_data$lag_nei = c(0,nei_spi_data[1:(n-1)]$nei_avg_spi)

plm_spi = plm(spi~lag_focal+lag_nei,nei_spi_data[2:n,],
              effect = "twoways", model = "within", index = "district")
summary(plm_spi)

# they are both significant, and the period SPI is positively related to the SPI from the previous period



# Q1.C -------------------------------------------------------------------------

spi_data$extreme = 0
spi_data[SPI <= -1 | SPI >= 1]$extreme = 1

# get period extreme count
spi_data[,period_ext:=sum(extreme),by=c("district","period")]
period_ext = unique(spi_data[,c(2,5,7)],by=c("district","period"))

# get all neighbor extreme for each district
nei_ext = merge(nei_total,period_ext,by="district",all=T,allow.cartesian=TRUE)
nei_ext = nei_ext[order(district),]
nei_ext = nei_ext[!is.na(period)]

nei_ext_full = (merge(x=period_ext, y=nei_ext, by.x=c("district","period"), by.y=c("neighbor","period")))
colnames(nei_ext_full) = c("district","period","ext","neighbor","nei_ext")

nei_ext_full[,nei_avg_ext:=mean(nei_ext,na.rm=T),by=c("district","period")]
nei_ext_data = unique(nei_ext_full,by=c("district","period"))

n = nrow(nei_ext_data)
nei_ext_data$lag_focal_ext = c(0,nei_ext_data[1:(n-1)]$ext)
nei_ext_data$lag_nei_ext = c(0,nei_ext_data[1:(n-1)]$nei_avg_ext)

summary(pglm(ext~lag_focal_ext+lag_nei_ext,nei_ext_data[2:n,],
                effect = "twoways", model = "within", 
                index = "district", family = "poisson"))

# they are both non-significant, meaning that number of extreme weather is independent across periods



# Q2.A -----------------------------------------------------------------------

# get extreme and founded data
eee = merge(spi_data,uniq,by=c("district","year"),all.x=T)
eee[,year_founded:=ifelse(is.na(name),0,1)]
eee[,period_founded:=sum(year_founded,na.rm=T),by=list(district,period.x)]
eee = eee[order(district,period.x,-year)]

uniq_eee = unique(eee,by=c("district","period.x"))
uniq_eee = uniq_eee[,c("district","period.x","year","period_ext","period_founded")]
names(uniq_eee)[2] = "period"
uniq_eee = cbind(uniq_eee,interval = rep(period$interval,length.out = 4648))

summary(pglm(period_founded ~ period_ext+interval, 
             data = uniq_eee, effect = "twoways", 
             model = "within", index = "district", family = "poisson"))

found_party = function(party_type){
  eee = merge(spi_data,uniq[party_issue==party_type],by=c("district","year"),all.x=T)
  eee[,year_founded:=ifelse(is.na(name),0,1)]
  eee[,period_founded:=sum(year_founded,na.rm=T),by=list(district,period.x)]
  eee = eee[order(district,period.x,-year)]
  uniq_eee = unique(eee,by=c("district","period.x"))
  uniq_eee = uniq_eee[,c("district","period.x","year","period_ext","period_founded")]
  names(uniq_eee)[2] = "period"
  uniq_eee = cbind(uniq_eee,interval = rep(period$interval,length.out = 4648))
  return(summary(pglm(period_founded ~ period_ext+interval, 
                      data = uniq_eee, effect = "twoways", 
                      model = "within", index = "district", family = "poisson")))
}

# unique(election$party_issue)
found_party("liberal")
found_party("far left")
found_party("economic_farming")
found_party("ethnic regional")
found_party("far right")
found_party("economic_nonfarming")
found_party("religious")

# in general, period extreme weather causes more party to be founded in the next election year
# liberal party and economic-nonfarming is more likely to be founded after the extreme weather



# Q2.B -----------------------------------------------------------------------

# get extreme and founded data of neighbor

nnn = merge(nei_total,spi_data,by="district",all.y=T,allow.cartesian=TRUE)

nnn_full = merge(x=nnn, y=spi_data[,c("district", "year", "extreme")], 
      by.x=c("neighbor","year"), by.y=c("district","year"))

nnn_full = nnn_full[order(district,year),c("district", "neighbor", "year", "extreme.y")]
nnn_avg = nnn_full[,list(nei_avg = mean(extreme.y)),by=list(district,year)]

nnn_with_nei = merge(eee,nnn_avg,by=c("district", "year"),all.x=T)

nnn_with_nei[,period_nei_avg:=mean(nei_avg,na.rm=T),by=list(district,period.x)]

uniq_nnn = unique(nnn_with_nei,by=c("district","period.x"))
uniq_nnn = uniq_nnn[,c("district","period.x","year","period_ext","period_founded","period_nei_avg")]
names(uniq_nnn)[2] = "period"
uniq_nnn = cbind(uniq_nnn,interval = rep(period$interval,length.out = 4648))
uniq_nnn$lag_nei = c(0,uniq_nnn[2:nrow(uniq_nnn)]$period_nei_avg)

summary(pglm(period_founded ~ period_ext+interval+lag_nei, 
             data = uniq_nnn[2:nrow(uniq_nnn),], effect = "twoways", 
             model = "within", index = "district", family = "poisson"))

# neighbor district extreme weather in 2 decades ago negatively affect the focal district's number of party founded, while focal extreme weather positively affect. It means the disaster in neighbor district may distract the focal political force



# Q3 -------------------------------------------------------------------------

library(hhi)
election_data[,total_vote:=sum(vote_count),by=list(district,period)]
election_data[,vote_share:=vote_count/total_vote*100]
election_data[is.na(vote_share)]$vote_share = 0
election_data[,hhi:=hhi(data.frame(vote_share)),by=list(district,period)]

hhi_data = merge(nnn_with_nei,election_data,by=c('district','year'),
                 all.x = T,allow.cartesian=TRUE)
hhi_data = hhi_data[order(district,period.x,-year)]
uniq_hhi = unique(hhi_data,by=c("district","period.x"))
uniq_hhi = cbind(uniq_hhi,interval = rep(period$interval,length.out = 4648))
uniq_hhi$lag_nei = c(0,uniq_hhi[2:nrow(uniq_hhi)]$period_nei_avg)

summary(pglm(hhi ~ period_ext+lag_nei+interval, 
             data = uniq_hhi[2:nrow(uniq_hhi),], effect = "twoways", 
             model = "within", index = "district", family = "poisson"))

# extreme weather reduce the concentration (hhi)



# Q4 -------------------------------------------------------------------------
elect_trim = uniq[,c("district","year","party_name","period")]
elect_trim$party_name = toupper(elect_trim$party_name)

previous_nei = c()
for (i in (1:nrow(elect_trim))){
  dist = elect_trim[i,district]
  y = elect_trim[i,year]
  neis = nei_total[district==dist]$neighbor
  for (n in neis){
    s=0
    if (elect_trim[i,party_name] %in% elect_trim[district==n&year<y]$party_name){
      s=s+1}}
  previous_nei = c(previous_nei,s)
}
# sum(previous_nei)

elect_trim = cbind(elect_trim,previous_nei)
# colnames(hhi_data)
hhi_trim = hhi_data[,c("district","year","period","period_ext",
                       "period_nei_avg")]
full_previous_nei = merge(hhi_trim,elect_trim,by=c("district","year"),
                          allow.cartesian=TRUE)

previous_data = unique(full_previous_nei,by=c("district","year"))

uniq_hhi = merge(previous_data,period,by=c("year"),all.x = T)
uniq_hhi = uniq_hhi[order(district,period,year),]

uniq_hhi$lag_nei = c(0,uniq_hhi[2:nrow(uniq_hhi)]$period_nei_avg)

# for party that diffuse from the neighbor to the focal district
summary(pglm(previous_nei ~ period_ext+lag_nei+interval, 
             data = uniq_hhi[2:nrow(uniq_hhi),], effect = "twoways", 
             model = "within", index = "district", family = "poisson"))

# neighbor's extreme weather is likely to cause the the difussion

uniq_hhi$non_previous_nei = -uniq_hhi$previous_nei+1

# for party that are newly founded in the distirct
summary(pglm(non_previous_nei ~ period_ext+lag_nei+interval, 
             data = uniq_hhi[2:nrow(uniq_hhi),], effect = "twoways", 
             model = "within", index = "district", family = "poisson"))
# there is no significant relationship

