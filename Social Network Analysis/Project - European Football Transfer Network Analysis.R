library(data.table)
library(igraph)
library(foreach)
library(plm)
library(rgl)

setwd("/Users/huangzm/Desktop/Social Network/soccer")
transfer = fread("transfers.csv")
clubs = fread("dict_clubs.csv")
player_perf = fread("stats_of_players.csv")
player_pos = fread("pos_stats_of_players.csv")
player_club = fread("players_in_clubs.csv")
league = fread("dict_leagues.csv")

# CLEANING TRANSFER DATA -------------------------------------------------------

# exclude records missing market value
transfer = transfer[!is.na(market_value)]
# exclude exclude useless records
trim_trans = transfer[is_loan+is_end_of_loan==0]
trim_trans = trim_trans[to_club_name!="Without Club"]
trim_trans = trim_trans[to_club_name!="Retired"]
trim_trans = trim_trans[to_club_name!="Career break"]
trim_trans = trim_trans[to_club_name!="Unknown"]
# transform the market value in millions
trim_trans$market_million = trim_trans$market_value/1000000


# SUMMARY STATISTICS OF TRANSFER -----------------------------------------------

# calculate average length between each two transfers on the same player
trim_trans = trim_trans[order(player_id,season)]
trim_trans[,window_player:= shift(season,type='lead')-season,by=list(player_id)]
mean(trim_trans$window_player,na.rm=T)

# calculate average length between each two transfers between the same two teams
trim_trans = trim_trans[order(from_club_id,to_club_id,season)]
trim_trans[,window_club:= shift(season,type='lead')-season,
           by=list(from_club_id,to_club_id)]

# 75% of the ties between the same two teams are renewed within 3 years
quantile(trim_trans$window_club,na.rm=T)

hist(trim_trans$window_club,xlab="Transfer Window"
     ,main="Average Transfer Window Between Two Clubs")





# GET CLUB CENTRALITY ----------------------------------------------------------

season_centrality = function(year){
  
  # using three years as decay window
  trim_transfer = trim_trans[season<year]
  trim_transfer = trim_transfer[order(from_club_id,to_club_id,season)]
  trim_transfer[, window_club:= shift(season,type='lead')-season,
                by=list(from_club_id,to_club_id)]
  trim_transfer = rbind(trim_transfer[is.na(window_club)],
                        trim_transfer[window_club<=3])
  trim_transfer = trim_transfer[order(from_club_id,to_club_id,season)]
  
  # use number of transfer as weights
  trim_transfer[,num_trans:=.N,by=list(from_club_id,to_club_id,season)]
  links = unique(trim_transfer,by=c('from_club_id','to_club_id'))
  links = links[,c('from_club_id','to_club_id',"num_trans")]
  
  # build the network
  names(links) = c("from","to","weight")
  net = graph.data.frame(links,directed = T)
  n = length(V(net))
  
  # compute centralities
  in_degree = strength(net,vids=V(net),mode="in",weights=E(net)$weight)/(n-1)
  out_degree = strength(net,vids=V(net),mode="out",weights=E(net)$weight)/(n-1)
  eigen = eigen_centrality(net,directed=T,weights=E(net)$weight)$vector
  centrality = data.table(club_id=V(net)$name,season=year,
                          in_degree,out_degree,eigen)
  
  return(centrality)}

# season_centrality(2006)

# get all degree for all seasons 
# (2002,2003,2004 have too few data, so start from 2005)
full_centrality = foreach(y = 2005:2020, .combine = rbind)%do%{season_centrality(y)}
full_centrality$club_id = as.integer(full_centrality$club_id)

# MODEL FREE INSIGHTS FROM CENTRALITY ------------------------------------------

club_cent = merge(full_centrality,clubs,by.x="club_id",by.y="id")
club_cent = club_cent[order(season,-in_degree)]
head(club_cent[season==2020],40)


# CALCULATE PLAYER QUALITY -----------------------------------------------------

# here i define quality as: 
# market value after transfer / market value before transfer
# if the index > 1, the player plays better than expectaion
# so the transfer quality is good for the buyer
# but it is a loss for the seller

player_club = player_club[order(player_id,season)]
player_club[,quality:=shift(market_value,type='lead')/market_value,
            by=list(player_id)]
player_club[,club_value:=mean(market_value,na.rm=T),
            by=list(season,club_id)]

# get value of each club by its player mean market value
club_value = unique(player_club[,c("club_id","season","club_value")],
                    by=c("club_id","season"))

# BUYER CENTRALITY AND PLAYER QUALITY ------------------------------------------

# join centrality
to_data = merge(trim_trans,full_centrality,
                by.x=c("to_club_id","season"),by.y=c("club_id","season"))

# join player performance
to_data = merge(to_data,player_perf,by=c("player_id","season"))

# join player quality
to_data = merge(to_data,player_club[,c("player_id","season","quality")],
                by=c("player_id","season"))

# join club value
to_data = merge(to_data,club_value,by=c("club_id","season"))

# only keep records showing player's performance in team he is transfered to
to_data = to_data[club_id==to_club_id]

# exclude records for "Others" league
to_data = to_data[league_name!="Others"]

# data engineering
to_data[,club_age:=season-min(season),by=to_club_id]
to_data[,avg_min:=sum(minutes_played)/sum(apps),by=list(player_id,season,to_club_id)]
to_data[,min_over_value:=avg_min/market_million]
to_data[,goals_over_value:=goals/market_million]
# normalize indegree across time 
to_data[,norm_in_degree:=(in_degree-mean(in_degree,na.rm=T))/sd(in_degree,na.rm=T),
        by=list(to_club_id)]
to_data[is.na(norm_in_degree)]$norm_in_degree = 
  to_data[is.na(norm_in_degree)]$in_degree

# summary statistics
library(corrplot)
cor = cor(to_data[!is.na(quality),c("in_degree","eigen","avg_min",
                                    "min_over_value","club_value","quality")])
corrplot(cor)

# adjusted average minutes VS. in-degree (sig, -)
summary(plm(min_over_value~poly(norm_in_degree,2)+club_age,
            data=to_data,model="within",effect="time",index="season"))

# adjusted average minutes VS. eigen (sig, -)
summary(plm(min_over_value~eigen+club_age+norm_in_degree,data=to_data,
            model="within",effect="individual",index="season"))

# quality VS. in-degree (nonsig)
summary(plm(quality~poly(norm_in_degree,2)+eigen+club_age+
              norm_in_degree:club_age,data=to_data,
            model="within",effect="individual",index="season"))

model_1 = plm(quality~I(norm_in_degree^2)+club_age+
              norm_in_degree:club_age,data=to_data,
              model="within",effect="individual",index="season")

in_degree_axis = seq(min(to_data$norm_in_degree),
                     max(to_data$norm_in_degree),length.out=100)
club_age_axis = seq(min(to_data$club_age),max(to_data$club_age),by=1)

train_DF = expand.grid(in_degree_axis,club_age_axis)
names(train_DF) = c("norm_in_degree","club_age")

train_DF$pred = model_1$coefficients[1]*train_DF$norm_in_degree^2+
  model_1$coefficients[2]*train_DF$club_age+model_1$coefficients[3]*
  train_DF$norm_in_degree*train_DF$club_age

plot3d(train_DF$norm_in_degree, train_DF$pred, train_DF$club_age)


# quality VS. eigen (sig, -)
summary(plm(quality~eigen+club_age,data=to_data,
            model="within",effect="individual",index="season"))

model_2 = plm(quality~eigen+club_age,data=to_data,
              model="within",effect="individual",index="season")

eigen_axis = seq(min(to_data$eigen),max(to_data$eigen),length.out=100)
club_age_axis = seq(min(to_data$club_age),max(to_data$club_age),by=1)
train_DF = expand.grid(eigen_axis,club_age_axis)
names(train_DF) = c("eigen","club_age")

train_DF$pred = model_2$coefficients[1]*train_DF$eigen+
  model_2$coefficients[2]*train_DF$club_age

plot3d(train_DF$eigen, train_DF$pred, train_DF$club_age)

# adjusted goals VS. in-degree (sig, -)
summary(plm(goals_over_value~in_degree+club_age,data=to_data,
            model="within",effect="individual",index="season"))  

# adjusted goals VS. eigen (sig, -)
summary(plm(goals_over_value~eigen+club_age,data=to_data,
            model="within",effect="individual",index="season"))  



# SELLER CENTRALITY AND PLAYER QUALITY ------------------------------------------

# join centrality
from_data = merge(trim_trans,full_centrality,
                by.x=c("from_club_id","season"),by.y=c("club_id","season"))

# join player performance
from_data = merge(from_data,player_perf,by=c("player_id","season"))

# join player quality
from_data = merge(from_data,player_club[,c("player_id","season","quality")],
                by=c("player_id","season"))

# only keep records showing player's performance in team he is transfered to
from_data = from_data[club_id==from_club_id]

# exclude records for "Others" league
from_data = from_data[league_name!="Others"]

# data engineering
from_data[,club_age:=season-min(season),by=from_club_id]
from_data[,avg_min:=sum(minutes_played)/sum(apps),by=list(player_id,season,from_club_id)]
from_data[,min_over_value:=avg_min/market_million]
from_data[,goals_over_value:=goals/market_million]

from_data[,norm_out_degree:=(out_degree-mean(out_degree,na.rm=T))
          /sd(out_degree,na.rm=T),by=list(from_club_id)]
from_data[is.na(norm_out_degree)]$norm_out_degree = 
  from_data[is.na(norm_out_degree)]$out_degree

#to_data[,norm_in_degree:=(in_degree-mean(in_degree))/sd(in_degree),by=list(season)]

# summary statistics
cor(from_data[!is.na(quality),c("in_degree","out_degree","eigen",
                              "avg_min","min_over_value","quality")])

# adjusted average minutes VS. in-degree (sig, -)
summary(plm(min_over_value~norm_out_degree+club_age,data=from_data,
            model="within",effect="individual",index="season"))

# adjusted average minutes VS. eigen (sig, -)
summary(plm(min_over_value~eigen+club_age,data=from_data,
            model="within",effect="individual",index="season"))

# quality VS. out-degree (sig, +)
summary(plm(quality~norm_out_degree+club_age,
            data=from_data,model="within",effect="individual",index="season"))

model_3=plm(quality~norm_out_degree+club_age,
            data=from_data,model="within",effect="individual",index="season")
out_degree_axis = seq(min(from_data$norm_out_degree),
                      max(from_data$norm_out_degree),length.out=100)
club_age_axis = seq(min(from_data$club_age),max(from_data$club_age),by=1)
train_DF = expand.grid(out_degree_axis,club_age_axis)
names(train_DF) = c("norm_out_degree","club_age")

train_DF$pred = model_3$coefficients[1]*train_DF$norm_out_degree+
  model_3$coefficients[2]*train_DF$club_age

plot3d(train_DF$norm_out_degree, train_DF$pred, train_DF$club_age)


# quality VS. eigen (sig, -)
summary(plm(quality~poly(eigen,2)+club_age+eigen:club_age,data=from_data,
            model="within",effect="individual",index="season"))

model_4 = plm(quality~poly(eigen,2)+club_age+eigen:club_age,data=from_data,
              model="within",effect="individual",index="season")

eigen_axis = seq(min(from_data$eigen),
                     max(from_data$eigen),length.out=100)
club_age_axis = seq(min(from_data$club_age),max(from_data$club_age),by=1)

train_DF = expand.grid(eigen_axis,club_age_axis)
names(train_DF) = c("eigen","club_age")

train_DF$pred = model_4$coefficients[1]*train_DF$eigen+
  model_4$coefficients[2]*train_DF$eigen^2+
  model_4$coefficients[3]*train_DF$club_age+
  model_4$coefficients[4]*train_DF$eigen*train_DF$club_age

plot3d(train_DF$eigen, train_DF$pred, train_DF$club_age)


# adjuested goals VS. in-degree (sig, -)
summary(plm(goals_over_value~out_degree+club_age,data=from_data,
            model="within",effect="individual",index="season"))  

# adjuested goals VS. eigen (sig, -)
summary(plm(goals_over_value~eigen+club_age,data=from_data,
            model="within",effect="individual",index="season"))  


############ CONCLUSION 2: 
# team who sell to a lot of other teams can sell their players at better price


# UNIQ BUYER -------------------------------------------------------------------

uniq_to = copy(to_data)
uniq_to[,club_avg_min:=mean(minutes_played),by=list(to_club_id,season)]
uniq_to[,club_avg_quality:=mean(quality),by=list(to_club_id,season)]
uniq_to[,club_avg_min_value:=mean(min_over_value),by=list(to_club_id,season)]
uniq_to = unique(uniq_to,by=c("to_club_id","season"))
uniq_to = uniq_to[order(season,-in_degree)]
uniq_to = uniq_to[order(club_name,season)]

summary(plm(club_avg_quality~poly(in_degree,2)+club_age+in_degree:club_age,
            data=uniq_to,model="within",effect="individual",index=c("season")))

# UNIQ SELLER ------------------------------------------------------------------

uniq_from = copy(from_data)
uniq_from[,club_avg_min:=mean(minutes_played),by=list(from_club_id,season)]
uniq_from[,club_avg_quality:=mean(quality),by=list(from_club_id,season)]
uniq_from[,club_avg_min_value:=mean(min_over_value),by=list(from_club_id,season)]
uniq_from = unique(uniq_from,by=c("to_club_id","season"))
uniq_from = uniq_from[order(season,-in_degree)]
uniq_from = uniq_from[order(club_name,season)]

summary(plm(club_avg_quality~out_degree+club_age,
            data=uniq_from,model="within",effect="individual",index=c("season")))

# DISCREPENACY  ----------------------------------------------------------------

library(hhi)
trim_trans[,num_to_club:=.N,by=list(from_club_id,to_club_id,season)]
trim_trans[,total_to_club:=.N,by=list(from_club_id,season)]

trim_trans[,hhi:=hhi(data.frame(num_to_club*100/total_to_club)),
           by=list(to_club_id,season)]

