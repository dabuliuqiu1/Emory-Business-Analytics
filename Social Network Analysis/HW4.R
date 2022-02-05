library(data.table)
library(igraph)
library(foreach)
library(zoo)
library(ggplot2)
library(tidyr)
library(reshape2)
library(proxy)
library(stats)
library(MASS)
keyword=fread('/Users/huangzm/Desktop/Social Network/HW4/film_keywords.csv')
maker=fread('/Users/huangzm/Desktop/Social Network/HW4/producers_and_films.csv')

# PREPARATRION: get all links --------------------------------------------------

# filter out US film maker
maker = maker[country=="us"]
maker[,count:= .N, by=pindex]

# get company list
mmm = maker[count>1]
maker_list = unique(mmm$pcindex)

# generate links
maker_full_links = merge(maker,maker[,list(pindex,pcindex,prod_company)],
                         by='pindex',allow.cartesian=TRUE)
maker_full_links = maker_full_links[order(pindex,pcindex.x,pcindex.y),]
maker_full_links = maker_full_links[pcindex.x < pcindex.y]

# calculate generalist
get_year_core = function(y){
  links = maker_full_links[year<y]
  maker_net = graph.data.frame(links[,list(pcindex.x,pcindex.y)],directed = F)
  coreness = data.table(pcindex = c(maker_list),
                        eigen = eigen_centrality(maker_net)$vector[c(maker_list)])
  # quar = quantile(coreness$eigen,0.75,na.rm=T)
  coreness$year = y
  coreness = coreness[,c(1,3,2)]
  return(coreness)
}

ten_core = foreach(y = 1985:2019, .combine = rbind)%do%{get_year_core(y)}
ten_core = na.omit(ten_core)
ten_core = ten_core[order(pcindex,year)]

ten_core[,ten_mean:= rollapply(eigen, width=10, mean, 
                               align='right', partial=T, fill=0), by=pcindex]

ten_core[, quant:= quantile(ten_mean,0.75), by = year]
ten_core[, generalist:= ifelse(ten_mean>=quant,1,0)]


maker_type = merge(maker,ten_core[,c("pcindex","year","generalist")],
          by=c("pcindex","year"),all.x=T)

# there is a lot of NA because 1000+ maker appear only once in the table,so they don't have coreness of previous years. These makers are marked specialist
# maker[,.N,by=pcindex]

maker_type[is.na(maker_type$generalist),"generalist"] = 0
maker_type = maker_type[order(year,pindex)]
maker_type[,center_num:=sum(generalist), by = pindex]

# label collaboration type
maker_type$PSolo = ifelse(maker_type$count==1 & maker_type$generalist==0, 1, 0)
maker_type$CSolo = ifelse(maker_type$count==1 & maker_type$generalist==1, 1, 0)
maker_type$PCo = ifelse(maker_type$count>1 & maker_type$center_num==0, 1, 0)
maker_type$CCo = ifelse(maker_type$count>1 & maker_type$count==maker_type$center_num, 1, 0)
maker_type$HCo = ifelse(maker_type$count>1 & maker_type$center_num>0 &
                          maker_type$count>maker_type$center_num, 1, 0)
# maker_type[,PSolo+CSolo+PCo+CCo+HCo] verify MECE


# PREPARATRION: get new keywords -----------------------------------------------

film_year = unique(maker_type[,c("pindex","year")])
key = merge(keyword,film_year)
key = key[order(keyword)]

ear_list = unique(key[,list(keyword,year)], by=c("keyword","year"))
ear_list = ear_list[order(keyword,year)]
ear_list[,first_appear:=min(year),by=keyword]
ear_list[,new:=ifelse(year<first_appear+3,1,0)]

key = merge(key,ear_list[,c(1,2,4)],by=c("keyword","year"),all.x = T)


# PREPARATRION: get new combination --------------------------------------------
#comb = fread("/Users/huangzm/Desktop/Social Network/HW4/comb.csv")
if (exists("comb")==F){
  comb = merge(keyword,keyword,by="pindex",allow.cartesian = T)
  comb = comb[order(pindex,keyword.x,keyword.y)]
  comb = comb[keyword.x<keyword.y]
  comb = merge(comb,film_year)
  
  ear_comb = unique(comb[,list(keyword.x,keyword.y,year)], 
                    by=c("keyword.x","keyword.y","year"))
  ear_comb = ear_comb[order(keyword.x,keyword.y,year)]
  ear_comb[,first_appear:=min(year),by=list(keyword.x,keyword.y)]
  ear_comb[,new:=ifelse(year<first_appear+3,1,0)]
  
  comb = merge(comb,ear_comb[,c(1,2,3,5)],
               by=c("keyword.x","keyword.y","year"),all.x = T)
  
  comb[, num_new_comb:=sum(new), by = pindex]
  
  write.csv(comb, file="/Users/huangzm/Desktop/Social Network/HW4/comb.csv")}

# Q1 (A) NEW KEYWORDS ----------------------------------------------------------

# get average keywords per film 
maker_key = merge(maker_type, key[,c(1,3,4)], by="pindex", allow.cartesian = T)
maker_key[, num_new_key:=sum(new), by = pindex]

new_key_type = unique(maker_key[,c("pindex","year","PSolo","CSolo",
                    "PCo","CCo","HCo","num_new_key")], by="pindex")
new_key_type = new_key_type[order(year)]

# exclude data later than 2017 because keyword data lacks records after 2017
# exclude data at 1985 because no network has formed prior to 1985, 
# and all the keywords are new that year, so analyzing 1985 data us meaningless
new_key_type = new_key_type[year < 2017 & year > 1985]

PSolo = new_key_type[PSolo==1][,mean(num_new_key), by=year]
CSolo = new_key_type[CSolo==1][,mean(num_new_key), by=year]
PCo = new_key_type[PCo==1][,mean(num_new_key), by=year]
CCo = new_key_type[CCo==1][,mean(num_new_key), by=year]
HCo = new_key_type[HCo==1][,mean(num_new_key), by=year]

years=seq(1986,2016)
new_key_data = data.table(years,PSolo[,2],CSolo[,2],PCo[,2],CCo[,2],HCo[,2])
colnames(new_key_data) = c("year","Peripheral solo productions",
                           "Central solo productions","Peripheral co-productions",
                           "Central co-productions","Hybrid co-productions")

aggNew = new_key_data %>% 
  pivot_longer(cols = `Peripheral solo productions`:`Hybrid co-productions`, 
    names_to = "type",values_to = "number")

ggplot(aggNew, aes(x = year, y = number, group = type, color = type)) +
  geom_line(size = 0.5) + ylab("Average Number of Keywords") + 
  scale_x_continuous(breaks = seq(1986,2016,10))
  
  
# Q1 (A) NEW COMBINATION -------------------------------------------------------

comb_avg = unique(comb,by="pindex")

maker_comb = merge(maker_type, comb_avg[,c(4,6)], by="pindex", allow.cartesian = T)
maker_comb = maker_comb[year < 2017 & year > 1985]
maker_comb = maker_comb[order(year)]

PSolo1 = maker_comb[PSolo==1][,mean(num_new_comb), by=year]
CSolo1 = maker_comb[CSolo==1][,mean(num_new_comb), by=year]
PCo1 = maker_comb[PCo==1][,mean(num_new_comb), by=year]
CCo1 = maker_comb[CCo==1][,mean(num_new_comb), by=year]
HCo1 = maker_comb[HCo==1][,mean(num_new_comb), by=year]

new_comb_data = data.table(years,PSolo1[,2],CSolo1[,2],PCo1[,2],CCo1[,2],HCo1[,2])
colnames(new_comb_data) = c("year","Peripheral solo productions",
                           "Central solo productions","Peripheral co-productions",
                           "Central co-productions","Hybrid co-productions")

aggNew_comb = new_comb_data %>% 
  pivot_longer(cols = `Peripheral solo productions`:`Hybrid co-productions`, 
               names_to = "type",values_to = "number")

ggplot(aggNew_comb, aes(x = year, y = number, group = type, color = type)) +
  geom_line(size = 0.5) + ylab("Average Number of Keywords") + 
  scale_x_continuous(breaks = seq(1986,2016,10))


# Q1 (B)  ----------------------------------------------------------------------
sub = fread("/Users/huangzm/Desktop/Social Network/HW4/production_subsidiaries.csv")
box = fread("/Users/huangzm/Desktop/Social Network/HW4/box_office_revenues.csv")

# label subsidiary
maker_key1 = merge(maker_key,sub,by="pcindex",all.x=T)
maker_key1[,sub:=ifelse(year>=first_year & year<=last_year,1,0)]
maker_key1[is.na(maker_key1$sub),"sub"] = 0

# add box office
maker_key1 = merge(maker_key1,box[,c(1,4)],by="pindex",all.x=T)
maker_key1[,pcbox:=sum(total_box),by=pcindex]

# add operation years
maker_key1[,operation_year:=year-min(year),by=pcindex]
maker_key1[operation_year>0]

key_year = merge(keyword,film_year)
film_maker = unique(maker[,c('pindex','pcindex')],by=c("pindex"))
key_year = merge(key_year,film_maker,all.x=T)

# get cmdscale of jaccard for the given year
yearly_scale = function(y){
  k = key_year[year<=y & year>y-3]
  k = k[order(pcindex),]
  # spread the keywords of each pcindex into columns
  spread_keys = dcast(k, pcindex  ~ keyword) 
  spread_keys = spread_keys[,-1]
  # transform to matrix
  spread_keys = as.matrix(spread_keys)
  spread_keys = matrix(as.numeric(spread_keys),ncol = ncol(spread_keys))
  # calculate 2-dimension mapping of jaccard
  scale = cmdscale(proxy::dist(spread_keys, method = "Jaccard",by_rows=T),2)
  maker_scale =  data.table(pcindex = unique(k$pcindex),scale)
  maker_scale$year = y
  return(maker_scale)
}

#full_yearly_scale = fread("/Users/huangzm/Desktop/Social Network/HW4/full_yearly_scale.csv")
#names(full_yearly_scale)[1] = "id"

if (exists("full_yearly_scale")==F){
  full_yearly_scale = foreach(y = 1985:2019, .combine = rbind)%do%{yearly_scale(y)}
  write.csv(full_yearly_scale, file="/Users/huangzm/Desktop/Social Network/HW4/full_yearly_scale.csv")}

maker_key1 = merge(maker_key1,full_yearly_scale,by=c("pcindex","year"),all.x=T)

# get film level data for further calculation
maker_key1_film = unique(maker_key1,by=c("pcindex","pindex"))
maker_key1_film = maker_key1_film[order(year,pcindex),]

# get yearly new keys for each producer
maker_key1_film[,pc_new_key:=sum(num_new_key),by=list(pcindex,year)]

# get yearly new combinations for each producer
maker_key1_film = merge(maker_key1_film, comb_avg[,c(4,6)], 
                        by="pindex", allow.cartesian = T)
maker_key1_film[,pc_new_comb:=sum(num_new_comb),by=list(pcindex,year)]

# get yearly number of film a producer makes each
maker_key1_film[,pc_new_film:=.N,by=list(pcindex,year)]

# get pc level data for regression
maker_key1_pc = unique(maker_key1_film,by=c("pcindex","year"))
maker_key1_pc = maker_key1_pc[year < 2017 & year > 1985]

# REGRESSION FOR NEW KEY -------------------------------------------------------

summary(glm.nb(pc_new_key ~CCo+PCo+HCo+V1+V2+pcbox+operation_year+sub+
                 factor(year),maker_key1_pc,offset(pc_new_film)))

# all CCO, PCO, and HCO are significant
# Center co-production and hybrid co-production is more likely to make more new keywords than peripheral co-production

# REGRESSION FOR NEW COMBINATION -----------------------------------------------
summary(glm.nb(pc_new_comb ~CCo+PCo+HCo+V1+V2+pcbox+operation_year+sub+
                 factor(year),maker_key1_pc,offset(pc_new_film)))

# all CCO, PCO, and HCO are significant
# Center co-production and hybrid co-production is more likely to make more new combinations than peripheral co-production



# Q2 ---------------------------------------------------------------------------

yearly_dist = function(y){
  k = key_year[year<=y & year>y-3]
  k = k[order(pcindex),]
  spread_keys = dcast(k, pcindex  ~ keyword)
  spread_keys = spread_keys[,-1]
  spread_keys = as.matrix(spread_keys)
  spread_keys = matrix(as.numeric(spread_keys),ncol = ncol(spread_keys))
  dist = proxy::dist(spread_keys, method = "Jaccard",by_rows=T)
  dist_df = melt(as.matrix(dist), varnames = c("pc1", "pc2"))
  pcid =  data.table(id = seq(1,length(unique(k$pcindex))),
                            pcindex = unique(k$pcindex))
  dist_df = merge(dist_df,pcid,by.x="pc1",by.y="id")
  dist_df = merge(dist_df,pcid,by.x="pc2",by.y="id")
  dist_df$year = y
  return(dist_df)
}

#full_yearly_dist = fread("/Users/huangzm/Desktop/Social Network/HW4/full_yearly_dist.csv")
#names(full_yearly_dist)[1] = "id"

if (exists('full_yearly_dist')==F){
full_yearly_dist = foreach(y = 1985:2019, .combine = rbind)%do%{yearly_dist(y)}
write.csv(full_yearly_dist,file="/Users/huangzm/Desktop/Social Network/HW4/full_yearly_dist")}

co_maker_film = merge(maker_full_links,full_yearly_dist[,c(3,4,5,6)],
                      by=c("pcindex.x","pcindex.y","year"),all.x=T)

co_maker_film[,avg_dist:=mean(value,na.rm=T),by=list(year,pcindex.x)]

co_maker_pc = merge(maker_key1_pc,co_maker_film[,c("year","pcindex.x","avg_dist")],
                    by.x = c("year","pcindex"),by.y = c("year","pcindex.x"),all.x=T)

ggplot(co_maker_pc, aes(avg_dist, pc_new_key)) + 
  geom_smooth(method = "loess", se = T) + 
  labs(x = "Average Jaccard distance", y = "New keywords")

ggplot(co_maker_pc, aes(avg_dist, pc_new_comb)) + 
  geom_smooth(method = "loess", se = T) + 
  labs(x = "Average Jaccard distance", y = "New keywords")


# firms tend to be more innovative when cooperating with makers with more similarity


# Q3 ---------------------------------------------------------------------------

# get yealy return for each pc
box1 = merge(box,maker[,c("pindex","year","pcindex")],all=T)
box1[, yearly_coverage:=sum(release_coverage),by=list(pcindex,year)]
box1 = box1[yearly_coverage>0] #exclude 94 of 0 records
box1[,return:=sum(total_box)/sum(release_coverage),by=list(pcindex,year)]
box1[,normalized_return:=(return-mean(return,na.rm=T))/sd(return,na.rm=T),by=year]
maker_return = unique(box1,by=c("pcindex","year"))[,c(5,6,9)]

# add it to the main dataset
maker_key1_pc = merge(maker_key1_pc,maker_return,by = c("pcindex","year"),all.x=T)

summary(lm(normalized_return ~CCo+PCo+HCo+V1+V2+operation_year+sub+
                 factor(year),maker_key1_pc))

# Center co-production and hybrid co-production is more likely to have higher return, Center co-production has the highest return


# Q4 (A) -----------------------------------------------------------------------

# get number of new words in hybrid co
hybrid_film = maker_key1_pc[HCo>0][,c("pindex","pcindex","year")]
hybrid_key = merge(key,hybrid_film,by=c("pindex","year"))
hybrid_key[,hybrid_new:=sum(new),by=list(pcindex,year)]
hybrid_new = unique(hybrid_key,by=c("pcindex","year"))

solo_film = maker_key1_pc[CSolo>0|PSolo>0][,c("pindex","pcindex","year")]
solo_key = merge(key,solo_film,by=c("pindex","year"))
solo_key[,solo_new:=sum(new),by=list(pcindex,year)]
solo_new = unique(solo_key,by=c("pcindex","year"))


maker_key1_pc = merge(maker_key1_pc,hybrid_new[,c(2,5,6)],by=c("pcindex","year"),all.x=T)
maker_key1_pc = merge(maker_key1_pc,solo_new[,c(2,5,6)],by=c("pcindex","year"),all.x=T)

maker_key1_pc[is.na(hybrid_new)]$hybrid_new = 0

maker_key1_pc = maker_key1_pc[order(pcindex,year)]

maker_key1_pc[, total_hybrid_new:= cumsum(hybrid_new), by = "pcindex"]

summary(glm.nb(solo_new ~ total_hybrid_new+V1+V2+pcbox+operation_year+sub+
                 factor(year), maker_key1_pc[solo_new>0],offset(pc_new_film)))

# no significant impact 


# Q4 (B) -----------------------------------------------------------------------

summary(lm(normalized_return ~pc_new_key+CCo+PCo+HCo+
             V1+V2+operation_year+sub+factor(year),maker_key1_pc))

summary(lm(normalized_return ~pc_new_comb+CCo+PCo+HCo+
             V1+V2+operation_year+sub+factor(year),maker_key1_pc))

# introducing new keywords/new combination can gain the producer more return, and collaboration boost a producer's innovation


# EXTRA CREDIT -----------------------------------------------------------------
cast=fread('/Users/huangzm/Desktop/Social Network/HW4/film_cast_members.csv')
film_key = unique(maker_key,by="pindex")
cast_key = merge(cast,film_key[,c("pindex","num_new_key")],by="pindex")

cast_key = cast_key[order(nconst,year),c("pindex","nconst","year","num_new_key")]
cast_key[,career_new:=cumsum(num_new_key), by = "nconst"]
cast_key[,career_num_key:=career_new - num_new_key]

cast_key[,innovativeness:=sum(career_num_key), by = "pindex"]
film_inno = unique(cast_key,by="pindex")

inno_data = merge(film_inno[,c("pindex","innovativeness"),],maker_key,by="pindex")

inno_uniq = unique(inno_data, by="pindex")

summary(lm(innovativeness ~HCo+factor(year),inno_uniq))

# the coefficient of HCo is significant and positive, indicating engaging in the hybird co-production helps the company hire more innovative cast members
