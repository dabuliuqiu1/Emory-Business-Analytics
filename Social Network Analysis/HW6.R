library(data.table)
library(igraph)
library(foreach)
library(RSiena)

deal=fread('/Users/huangzm/Desktop/Social Network/HW6/deal_details.csv')
execs=fread('/Users/huangzm/Desktop/Social Network/HW6/execs.csv')
investors=fread('/Users/huangzm/Desktop/Social Network/HW6/investor_details.csv')
people=fread('/Users/huangzm/Desktop/Social Network/HW6/people.csv')
rep=fread('/Users/huangzm/Desktop/Social Network/HW6/representative_names.csv')
ind_inv=fread('/Users/huangzm/Desktop/Social Network/HW6/individual_investors.csv')
company=fread('/Users/huangzm/Desktop/Social Network/HW5/company_details.csv')


# PREPOCESSING -----------------------------------------------------------------

# filter 2000 onward
deal[,yy:=substring(Deal_Date,8,10)]
deal[,year:=ifelse(yy>19,paste("19",yy,sep=""),paste("20",yy,sep=""))]
deal[,year:=as.numeric(year)]
deal = deal[yy!=""]
deal_trim = deal[year>=2000]

# filter Deal Class
deal_trim = deal_trim[Deal_Class=="Venture Capital"]

# filter role
ind_inv = ind_inv[Role %in% 
                    c("Founder","Chief Executive Officer","Managing Director")]
ind_inv = ind_inv[,c(1:5)]

# exclude multi-identity person
inv_ent = merge(ind_inv,execs,by="PersonId",all=T)
inv_ent = inv_ent[is.na(CompanyId.x)|is.na(CompanyId.y)]

ind_inv = merge(ind_inv,inv_ent[,c(1)],by="PersonId")
execs = merge(execs,inv_ent[,c(1)],by="PersonId")

execs[,year.join:=as.numeric(substring(Date.Joined.Company,7,11))]
execs[,year.left:=as.numeric(substring(Date.Left.Company,7,11))]
execs = execs[order(PersonId,year.join)]

# generate full network
d = deal_trim[,c("DealId","CompanyId","year")]
full_data = merge(ind_inv[,c("DealId","PersonId","InvestorId")],d,by="DealId")
full_data = merge(full_data,execs[,c("CompanyId","PersonId")],by="CompanyId")
colnames(full_data) = gsub('\\.x\\b','.inv',names(full_data))
colnames(full_data) = gsub('\\.y\\b','.ent',names(full_data))


names(company)[1] = "CompanyId"
full_data = merge(full_data,company[,c("CompanyId","Primary_Industry_Group")],
                  by="CompanyId")

# For Commercial Product
c_data = full_data[Primary_Industry_Group=='Commercial Products']
c_data[,min(year),by=PersonId.inv] # start from 2005
c_data = c_data[year>=2005]
links = c_data[,c(3,6,5)]

inv_list = sort(unique(links$PersonId.inv))
ent_list = sort(unique(links$PersonId.ent))
vertices = c(inv_list,ent_list)
year_range = min(links$year):max(links$year)

# get bipartite network for each year
total = c()
for (y in year_range){
  l=links[year==y][,c(1,2)]
  names(l)=c("from","to")
  l=unique(l,by=c("from","to"))
  
  net = graph_from_data_frame(l, directed = TRUE, vertices = vertices)
  V(net)$type = V(net)$name %in% l[,2]
  #is_bipartite(net)
  d = as.data.table(as.matrix(as_adjacency_matrix(net)))
  d = d[1:length(unique(links$PersonId.inv)),
        (length(unique(links$PersonId.inv))+1):length(vertices)]
  
  matrix = as.matrix(as.matrix(d),"dgTMatrix")
  total = c(total,matrix)
}

a = array(total, dim=c(length(unique(links$PersonId.inv)), 
                     length(unique(links$PersonId.ent)), 
                     length(year_range)))

dependent = sienaDependent(a,type="bipartite", 
                           nodeSet=c("senders", "receivers"))
senders = sienaNodeSet(length(unique(links$PersonId.inv)),
                        nodeSetName="senders")
receivers = sienaNodeSet(length(unique(links$PersonId.ent)),
                            nodeSetName="receivers")


# 1.DYADIC PREDICTORS ----------------------------------------------------------

# 1) ETHINICITY ----------------------------------------------------------------

# get last name for investor and entreprenaur

c_data = merge(c_data,people[,c('PersonId','Last Name')],
               by.x="PersonId.inv",by.y="PersonId",all.x=T)
names(c_data)[8] = "Last_Name.inv"

c_data = merge(c_data,people[,c('PersonId','Last Name')],
               by.x="PersonId.ent",by.y="PersonId",all.x=T)
names(c_data)[9] = "Last_Name.ent"

c_data$Last_Name.inv = toupper(c_data$Last_Name.inv)
c_data$Last_Name.ent = toupper(c_data$Last_Name.ent)

c_data = merge(c_data,rep[,c('Name','race')],
               by.x="Last_Name.inv",by.y="Name",all.x=T)
c_data = merge(c_data,rep[,c('Name','race')],
               by.x="Last_Name.ent",by.y="Name",all.x=T)

# 2) GENDER -----------------------------------------------------------------------

c_data = merge(c_data,people[,c('PersonId','Gender')],
               by.x="PersonId.inv",by.y="PersonId",all.x=T)
c_data = merge(c_data,people[,c('PersonId','Gender')],
               by.x="PersonId.ent",by.y="PersonId",all.x=T)

# 3) TOP SCHOOL -------------------------------------------------------------------

# I download the THE ranking 2019 and choose the top 100 schools
# https://www.kaggle.com/mylesoneill/world-university-rankings
rank = fread('/Users/huangzm/Desktop/Social Network/HW6/ranking/timesData.csv')
top_university = rank[1:100,]$university_name
# prevent matching failure
top_university = c(top_university,c("Cambridge","UCLA","MIT","Oxford"))

education = merge(people,data.table(PersonId=vertices),by="PersonId")
education = education[,c('PersonId','Education')]

education$top_school = 0
for (top in top_university){
education[Education %like% top]$top_school = 
  education[Education %like% top]$top_school + 1
}
education[,top_school:=ifelse(top_school>0,1,0)]
education[Education=='',top_school:=NA]

c_data = merge(c_data,education[,c('PersonId','top_school')],
               by.x="PersonId.inv",by.y="PersonId",all.x=T)
c_data = merge(c_data,education[,c('PersonId','top_school')],
               by.x="PersonId.ent",by.y="PersonId",all.x=T)


# 4) GEOGRAPHIC -------------------------------------------------------------------

load('/Users/huangzm/Desktop/Social Network/HW6/edges_dist.RData')

inv_loc=unique(edges_dist,by="InvestorId")[,c('InvestorId','inv_lat','inv_lon')]
ent_loc=unique(edges_dist,by="CompanyId")[,c('CompanyId','comp_lat','comp_lon')]

c_data = merge(c_data,inv_loc,by="InvestorId",all.x=T)
c_data = merge(c_data,ent_loc,by="CompanyId",all.x=T)

# 5) EXPERIENCE -------------------------------------------------------------------

# get investor experience
inv_experience = merge(ind_inv,deal[,c('DealId','year')],by="DealId",all.x=T)
inv_experience[,experience:=2018-min(year,na.rm=T),by=PersonId]
inv_experience = unique(inv_experience[,c('PersonId','experience')],by="PersonId")

# get entrepreneur experience
execs_experience = merge(execs,deal[,c('CompanyId','year')],by="CompanyId",
                   all.x=T, allow.cartesian=T)
execs_experience[year>=year.join,earliest:=min(year),by=PersonId]
execs_experience[is.na(earliest),earliest:=min(year),by=PersonId]
execs_experience[,experience:=2018-earliest,by=PersonId]

execs_experience = unique(execs_experience[,c('PersonId','experience')],
                          by='PersonId')

c_data=merge(c_data,inv_experience,by.x="PersonId.inv",by.y="PersonId",all.x=T)
c_data=merge(c_data,execs_experience,by.x="PersonId.ent",by.y="PersonId",all.x=T)

# 6) DEGREE -----------------------------------------------------------------------

education$tech = 0
for (degree in c("Engineering","Ph.D")){
  education[Education %like% degree]$tech = 
    education[Education %like% degree]$tech + 1
}
education[,tech:=ifelse(tech>0,1,0)]

education$buss = 0
for (degree in c("MBA")){
  education[Education %like% degree]$buss = 
    education[Education %like% degree]$buss + 1
}
education[,buss:=ifelse(buss>0,1,0)]
education[Education=='',tech:=NA]
education[Education=='',buss:=NA]

c_data = merge(c_data,education[,c('PersonId','tech')],
               by.x="PersonId.inv",by.y="PersonId",all.x=T)
c_data = merge(c_data,education[,c('PersonId','tech')],
               by.x="PersonId.ent",by.y="PersonId",all.x=T)
c_data = merge(c_data,education[,c('PersonId','buss')],
               by.x="PersonId.inv",by.y="PersonId",all.x=T)
c_data = merge(c_data,education[,c('PersonId','buss')],
               by.x="PersonId.ent",by.y="PersonId",all.x=T)

# generate effects from above covariates ---------------------------------------

colnames(c_data) = gsub('\\.x\\b','.inv',names(c_data))
colnames(c_data) = gsub('\\.y\\b','.ent',names(c_data))

c_numeric = copy(c_data)

c_numeric[,race.inv:=ifelse(race.inv=="white",1,-1)]
c_numeric[,race.ent:=ifelse(race.ent=="white",1,-1)]

c_numeric[,Gender.inv:=ifelse(Gender.inv=="Male",1,-1)]
c_numeric[,Gender.ent:=ifelse(Gender.ent=="Male",1,-1)]

c_numeric[,top_school.inv:=ifelse(top_school.inv>0,1,-1)]
c_numeric[,top_school.ent:=ifelse(top_school.ent>0,1,-1)]
c_numeric[,top_school.inv:=ifelse(top_school.inv>0,1,-1)]
c_numeric[,top_school.ent:=ifelse(top_school.ent>0,1,-1)]

c_numeric[,buss.inv:=buss.inv+1]
c_numeric[,buss.ent:=buss.ent+1]
c_numeric[,tech.inv:=tech.inv+1]
c_numeric[,tech.ent:=tech.ent+1]

c_data_inv = unique(c_numeric,by="PersonId.inv")[order(PersonId.inv)]
c_data_ent = unique(c_numeric,by="PersonId.ent")[order(PersonId.ent)]

# Ethnic_homo
Ethnic_homo = outer(c_data_inv$race.inv,c_data_ent$race.ent)
Ethnic_homo = (Ethnic_homo + 1)/2
Ethnic_homo = coDyadCovar(Ethnic_homo,nodeSets=c("senders", "receivers"))

# Gender_homo
Gender_homo = outer(c_data_inv$Gender.inv,c_data_ent$Gender.ent)
Gender_homo = (Gender_homo + 1)/2
Gender_homo = coDyadCovar(Gender_homo,nodeSets=c("senders", "receivers"))

# TopSchool_homo
TopSchool_homo = outer(c_data_inv$top_school.inv,c_data_ent$top_school.ent)
TopSchool_homo = (TopSchool_homo + 1)/2
TopSchool_homo = coDyadCovar(TopSchool_homo,nodeSets=c("senders", "receivers"))

# Geographic_homo
library(geosphere)
Geographic_homo = distm(c_data_inv[,c('inv_lon','inv_lat')],
                        c_data_ent[,c('comp_lon','comp_lat')], fun = distGeo)
Geographic_homo = coDyadCovar(Geographic_homo,nodeSets=c("senders", "receivers"))

# Experience_homo
exp = data.table(merge(c_data_inv$experience.inv,c_data_ent$experience.ent))
exp[,diff:=abs(x-y)]
exp[,similarity:=(max(diff,na.rm=T)-diff)]
exp[,similarity:=(similarity-mean(similarity,na.rm=T))/sd(similarity,na.rm=T)]
Experience_homo = matrix(exp$similarity,nrow=length(inv_list))
Experience_homo = coDyadCovar(Experience_homo,nodeSets=c("senders", "receivers"))

# Degree_homo
Degree_homo = outer(c_data_inv$tech.inv,c_data_ent$buss.ent) + 
              outer(c_data_inv$buss.inv,c_data_ent$tech.ent)
# since I set have degree as 2, no degree as 1,
# then one skill complementary means: entry >= (1*2+2*1) = 4
Degree_homo[!is.na(Degree_homo)]=ifelse(Degree_homo[!is.na(Degree_homo)]>=4,1,0)
Degree_homo = coDyadCovar(Degree_homo,nodeSets=c("senders", "receivers"))



# 2.INDIVIDUAL PREDICTORS ------------------------------------------------------

# 1) ethnic
Ethnic_ind = c_data_ent$race.ent
Ethnic_ind = (-Ethnic_ind+1)/2 # convert from (-1,1) to (1,0)
Ethnic_ind = coCovar(Ethnic_ind,nodeSet='receivers')

# 2) gender
Gender_ind = c_data_ent$Gender.ent
Gender_ind = (-Gender_ind+1)/2 # convert from (-1,1) to (1,0)
Gender_ind = coCovar(Gender_ind,nodeSet='receivers')

# 3) top school
TopSchool_ind = c_data_ent$top_school.ent
TopSchool_ind = (TopSchool_ind+1)/2 # convert from (-1,1) to (0,1)
TopSchool_ind = coCovar(TopSchool_ind,nodeSet='receivers')

# 4) geographic hub
top_city = company[,.N,by=City][order(-N)][2:11] # first one is empty value
geo = merge(execs[,c(1,2)],data.table(PersonId=ent_list),by="PersonId")
geo = unique(geo,by="PersonId")
geo = merge(geo,company[,c('CompanyId','City')],by="CompanyId",all.x=T)
geo = merge(geo,top_city,by="City",all.x=T)

geo$top_city = 0
geo[!is.na(N)]$top_city = 1
geo[is.na(City)]$top_city = NA
geo[City==""]$top_city = NA

geo = geo[order(PersonId)]
Geographic_ind = geo$top_city
Geographic_ind = coCovar(Geographic_ind,nodeSet='receivers')

# 5) experience
Experience_ind = c_data_ent$experience.ent
Experience_ind = coCovar(Experience_ind,nodeSet='receivers')

# 6) business
Business_ind = c_data_ent$buss.ent
Business_ind = Business_ind-1 # convert from (1,2) to (0,1)
Business_ind = coCovar(Business_ind,nodeSet='receivers')

# 7) tech
Tech_ind = c_data_ent$tech.ent
Tech_ind = Tech_ind-1 # convert from (1,2) to (0,1)
Tech_ind = coCovar(Tech_ind,nodeSet='receivers')

# 8) venture round
ent_round = merge(execs,deal[,c('CompanyId','year')],by="CompanyId",
                         all.x=T, allow.cartesian=T)
ent_round = merge(ent_round,data.table(PersonId=ent_list),by="PersonId")
ent_round[year.join<=year,rounds:=.N,by=PersonId]
ent_round[,rounds:=max(rounds,na.rm=T),by=PersonId]
Round_ind = unique(ent_round[,c("PersonId","rounds")],by="PersonId")
Round_ind = Round_ind[order(PersonId)]$rounds
Round_ind = coCovar(Round_ind,nodeSet='receivers')

# 3.TIME JOIN AND LEAVE --------------------------------------------------------

time_inv = copy(c_data)
time_inv[,join:=min(year,na.rm=T)-2004,by=PersonId.inv]
time_inv[,leave:=max(year,na.rm=T)-2004+1,by=PersonId.inv]
time_inv[leave>max(year,na.rm=T)-2004]$leave = 
  time_inv[leave>max(year,na.rm=T)-2004]$leave - 1
time_inv = unique(time_inv[,c("PersonId.inv","join","leave")],by="PersonId.inv")
time_inv = time_inv[order(PersonId.inv)][,c(2,3)]
time_inv = as.list(as.data.table(t(as.matrix(time_inv))))

time_effect_inv = sienaCompositionChange(time_inv, nodeSet = "senders")

time_ent = copy(c_data)
time_ent[,join:=min(year,na.rm=T)-2004,by=PersonId.ent]
time_ent[,leave:=max(year,na.rm=T)-2004+1,by=PersonId.ent]
time_ent[leave>max(year,na.rm=T)-2004]$leave = 
  time_ent[leave>max(year,na.rm=T)-2004]$leave - 1
time_ent = unique(time_ent[,c("PersonId.ent","join","leave")],by="PersonId.ent")
time_ent = time_ent[order(PersonId.ent)][,c(2,3)]
time_ent = as.list(as.data.table(t(as.matrix(time_ent))))

time_effect_ent = sienaCompositionChange(time_ent, nodeSet = "receivers")

# GATHER ALL EFFECTS
Siena = sienaDataCreate(dependent,
                        # dyadic
                        Ethnic_homo,Gender_homo,TopSchool_homo,
                        Geographic_homo,Experience_homo,Degree_homo,
                        # constant
                        Ethnic_ind,Gender_ind,TopSchool_ind,Geographic_ind,
                        Experience_ind,Business_ind,Tech_ind,Round_ind,
                        # time
                        time_effect_inv,time_effect_ent,
                        nodeSets=list(senders,receivers))

Effects = getEffects(Siena)
Effects = includeEffects(Effects,cycle4,outActSqrt,inPopSqrt,outInAss)
Effects = includeEffects(Effects,X,interaction1="Ethnic_homo")
Effects = includeEffects(Effects,X,interaction1="Gender_homo")
Effects = includeEffects(Effects,X,interaction1="TopSchool_homo")
Effects = includeEffects(Effects,X,interaction1="Geographic_homo")
Effects = includeEffects(Effects,X,interaction1="Experience_homo")
Effects = includeEffects(Effects,X,interaction1="Degree_homo")
Effects = includeEffects(Effects,altX,interaction1="Ethnic_ind")
Effects = includeEffects(Effects,altX,interaction1="Gender_ind")
Effects = includeEffects(Effects,altX,interaction1="TopSchool_ind")
Effects = includeEffects(Effects,altX,interaction1="Geographic_ind")
Effects = includeEffects(Effects,altX,interaction1="Experience_ind")
Effects = includeEffects(Effects,altX,interaction1="Business_ind")
Effects = includeEffects(Effects,altX,interaction1="Tech_ind")
Effects = includeEffects(Effects,altX,interaction1="Round_ind")

sienaModelCreate(cond = FALSE)
Algo=sienaAlgorithmCreate(projname='CoEvol_results',diagonalize = 0.2)

#siena_result = siena07(Algo,data=Siena,effects=Effects,
                       #initC = TRUE)
#CoEvolutionResults



################################################################################
################################################################################

full_data[,.N,by=Primary_Industry_Group]
c_data = full_data[Primary_Industry_Group=='Retail']

c_data[,min(year),by=PersonId.inv] # start from 2005
c_data[,.N,by=year]
min_year = 2014
c_data = c_data[year>=min_year]
links = c_data[,c(3,6,5)]

inv_list = sort(unique(links$PersonId.inv))
ent_list = sort(unique(links$PersonId.ent))
vertices = c(inv_list,ent_list)
year_range = min(links$year):max(links$year)

# get bipartite network for each year
total = c()
for (y in year_range){
  l=links[year==y][,c(1,2)]
  names(l)=c("from","to")
  l=unique(l,by=c("from","to"))
  
  net = graph_from_data_frame(l, directed = TRUE, vertices = vertices)
  V(net)$type = V(net)$name %in% l[,2]
  #is_bipartite(net)
  d = as.data.table(as.matrix(as_adjacency_matrix(net)))
  d = d[1:length(unique(links$PersonId.inv)),
        (length(unique(links$PersonId.inv))+1):length(vertices)]
  
  matrix = as.matrix(as.matrix(d),"dgTMatrix")
  total = c(total,matrix)
}

a = array(total, dim=c(length(unique(links$PersonId.inv)), 
                       length(unique(links$PersonId.ent)), 
                       length(year_range)))

dependent = sienaDependent(a,type="bipartite", 
                           nodeSet=c("senders", "receivers"))
senders = sienaNodeSet(length(unique(links$PersonId.inv)),
                       nodeSetName="senders")
receivers = sienaNodeSet(length(unique(links$PersonId.ent)),
                         nodeSetName="receivers")


# 1.DYADIC PREDICTORS ----------------------------------------------------------

# 1) ETHINICITY ----------------------------------------------------------------

# get last name for investor and entreprenaur

c_data = merge(c_data,people[,c('PersonId','Last Name')],
               by.x="PersonId.inv",by.y="PersonId",all.x=T)
names(c_data)[8] = "Last_Name.inv"

c_data = merge(c_data,people[,c('PersonId','Last Name')],
               by.x="PersonId.ent",by.y="PersonId",all.x=T)
names(c_data)[9] = "Last_Name.ent"

c_data$Last_Name.inv = toupper(c_data$Last_Name.inv)
c_data$Last_Name.ent = toupper(c_data$Last_Name.ent)

c_data = merge(c_data,rep[,c('Name','race')],
               by.x="Last_Name.inv",by.y="Name",all.x=T)
c_data = merge(c_data,rep[,c('Name','race')],
               by.x="Last_Name.ent",by.y="Name",all.x=T)

# 2) GENDER -----------------------------------------------------------------------

c_data = merge(c_data,people[,c('PersonId','Gender')],
               by.x="PersonId.inv",by.y="PersonId",all.x=T)
c_data = merge(c_data,people[,c('PersonId','Gender')],
               by.x="PersonId.ent",by.y="PersonId",all.x=T)

# 3) TOP SCHOOL -------------------------------------------------------------------

# I download the THE ranking 2019 and choose the top 100 schools
# https://www.kaggle.com/mylesoneill/world-university-rankings
rank = fread('/Users/huangzm/Desktop/Social Network/HW6/ranking/timesData.csv')
top_university = rank[1:100,]$university_name
# prevent matching failure
top_university = c(top_university,c("Cambridge","UCLA","MIT","Oxford"))

education = merge(people,data.table(PersonId=vertices),by="PersonId")
education = education[,c('PersonId','Education')]

education$top_school = 0
for (top in top_university){
  education[Education %like% top]$top_school = 
    education[Education %like% top]$top_school + 1
}
education[,top_school:=ifelse(top_school>0,1,0)]
education[Education=='',top_school:=NA]

c_data = merge(c_data,education[,c('PersonId','top_school')],
               by.x="PersonId.inv",by.y="PersonId",all.x=T)
c_data = merge(c_data,education[,c('PersonId','top_school')],
               by.x="PersonId.ent",by.y="PersonId",all.x=T)


# 4) GEOGRAPHIC -------------------------------------------------------------------

load('/Users/huangzm/Desktop/Social Network/HW6/edges_dist.RData')

inv_loc=unique(edges_dist,by="InvestorId")[,c('InvestorId','inv_lat','inv_lon')]
ent_loc=unique(edges_dist,by="CompanyId")[,c('CompanyId','comp_lat','comp_lon')]

c_data = merge(c_data,inv_loc,by="InvestorId",all.x=T)
c_data = merge(c_data,ent_loc,by="CompanyId",all.x=T)

# 5) EXPERIENCE -------------------------------------------------------------------

# get investor experience
inv_experience = merge(ind_inv,deal[,c('DealId','year')],by="DealId",all.x=T)
inv_experience[,experience:=2018-min(year,na.rm=T),by=PersonId]
inv_experience = unique(inv_experience[,c('PersonId','experience')],by="PersonId")

# get entrepreneur experience
execs_experience = merge(execs,deal[,c('CompanyId','year')],by="CompanyId",
                         all.x=T, allow.cartesian=T)
execs_experience[year>=year.join,earliest:=min(year),by=PersonId]
execs_experience[is.na(earliest),earliest:=min(year),by=PersonId]
execs_experience[,experience:=2018-earliest,by=PersonId]

execs_experience = unique(execs_experience[,c('PersonId','experience')],
                          by='PersonId')

c_data=merge(c_data,inv_experience,by.x="PersonId.inv",by.y="PersonId",all.x=T)
c_data=merge(c_data,execs_experience,by.x="PersonId.ent",by.y="PersonId",all.x=T)

# 6) DEGREE -----------------------------------------------------------------------

education$tech = 0
for (degree in c("Engineering","Ph.D")){
  education[Education %like% degree]$tech = 
    education[Education %like% degree]$tech + 1
}
education[,tech:=ifelse(tech>0,1,0)]

education$buss = 0
for (degree in c("MBA")){
  education[Education %like% degree]$buss = 
    education[Education %like% degree]$buss + 1
}
education[,buss:=ifelse(buss>0,1,0)]
education[Education=='',tech:=NA]
education[Education=='',buss:=NA]

c_data = merge(c_data,education[,c('PersonId','tech')],
               by.x="PersonId.inv",by.y="PersonId",all.x=T)
c_data = merge(c_data,education[,c('PersonId','tech')],
               by.x="PersonId.ent",by.y="PersonId",all.x=T)
c_data = merge(c_data,education[,c('PersonId','buss')],
               by.x="PersonId.inv",by.y="PersonId",all.x=T)
c_data = merge(c_data,education[,c('PersonId','buss')],
               by.x="PersonId.ent",by.y="PersonId",all.x=T)

# generate effects from above covariates ---------------------------------------

colnames(c_data) = gsub('\\.x\\b','.inv',names(c_data))
colnames(c_data) = gsub('\\.y\\b','.ent',names(c_data))

c_numeric = copy(c_data)

c_numeric[,race.inv:=ifelse(race.inv=="white",1,-1)]
c_numeric[,race.ent:=ifelse(race.ent=="white",1,-1)]

c_numeric[,Gender.inv:=ifelse(Gender.inv=="Male",1,-1)]
c_numeric[,Gender.ent:=ifelse(Gender.ent=="Male",1,-1)]

c_numeric[,top_school.inv:=ifelse(top_school.inv>0,1,-1)]
c_numeric[,top_school.ent:=ifelse(top_school.ent>0,1,-1)]
c_numeric[,top_school.inv:=ifelse(top_school.inv>0,1,-1)]
c_numeric[,top_school.ent:=ifelse(top_school.ent>0,1,-1)]

c_numeric[,buss.inv:=buss.inv+1]
c_numeric[,buss.ent:=buss.ent+1]
c_numeric[,tech.inv:=tech.inv+1]
c_numeric[,tech.ent:=tech.ent+1]

c_data_inv = unique(c_numeric,by="PersonId.inv")[order(PersonId.inv)]
c_data_ent = unique(c_numeric,by="PersonId.ent")[order(PersonId.ent)]

# Ethnic_homo
Ethnic_homo = outer(c_data_inv$race.inv,c_data_ent$race.ent)
Ethnic_homo = (Ethnic_homo + 1)/2
Ethnic_homo = coDyadCovar(Ethnic_homo,nodeSets=c("senders", "receivers"))

# Gender_homo
Gender_homo = outer(c_data_inv$Gender.inv,c_data_ent$Gender.ent)
Gender_homo = (Gender_homo + 1)/2
Gender_homo = coDyadCovar(Gender_homo,nodeSets=c("senders", "receivers"))

# TopSchool_homo
TopSchool_homo = outer(c_data_inv$top_school.inv,c_data_ent$top_school.ent)
TopSchool_homo = (TopSchool_homo + 1)/2
TopSchool_homo = coDyadCovar(TopSchool_homo,nodeSets=c("senders", "receivers"))

# Geographic_homo
library(geosphere)
Geographic_homo = distm(c_data_inv[,c('inv_lon','inv_lat')],
                        c_data_ent[,c('comp_lon','comp_lat')], fun = distGeo)
Geographic_homo = coDyadCovar(Geographic_homo,nodeSets=c("senders", "receivers"))

# Experience_homo
exp = data.table(merge(c_data_inv$experience.inv,c_data_ent$experience.ent))
exp[,diff:=abs(x-y)]
exp[,similarity:=(max(diff,na.rm=T)-diff)]
exp[,similarity:=(similarity-mean(similarity,na.rm=T))/sd(similarity,na.rm=T)]
Experience_homo = matrix(exp$similarity,nrow=length(inv_list))
Experience_homo = coDyadCovar(Experience_homo,nodeSets=c("senders", "receivers"))

# Degree_homo
Degree_homo = outer(c_data_inv$tech.inv,c_data_ent$buss.ent) + 
  outer(c_data_inv$buss.inv,c_data_ent$tech.ent)
# since I set have degree as 2, no degree as 1,
# then one skill complementary means: entry >= (1*2+2*1) = 4
Degree_homo[!is.na(Degree_homo)]=ifelse(Degree_homo[!is.na(Degree_homo)]>=4,1,0)
Degree_homo = coDyadCovar(Degree_homo,nodeSets=c("senders", "receivers"))



# 2.INDIVIDUAL PREDICTORS ------------------------------------------------------

# 1) ethnic
Ethnic_ind = c_data_ent$race.ent
Ethnic_ind = (-Ethnic_ind+1)/2 # convert from (-1,1) to (1,0)
Ethnic_ind = coCovar(Ethnic_ind,nodeSet='receivers')

# 2) gender
Gender_ind = c_data_ent$Gender.ent
Gender_ind = (-Gender_ind+1)/2 # convert from (-1,1) to (1,0)
Gender_ind = coCovar(Gender_ind,nodeSet='receivers')

# 3) top school
TopSchool_ind = c_data_ent$top_school.ent
TopSchool_ind = (TopSchool_ind+1)/2 # convert from (-1,1) to (0,1)
TopSchool_ind = coCovar(TopSchool_ind,nodeSet='receivers')

# 4) geographic hub
top_city = company[,.N,by=City][order(-N)][2:11] # first one is empty value
geo = merge(execs[,c(1,2)],data.table(PersonId=ent_list),by="PersonId")
geo = unique(geo,by="PersonId")
geo = merge(geo,company[,c('CompanyId','City')],by="CompanyId",all.x=T)
geo = merge(geo,top_city,by="City",all.x=T)

geo$top_city = 0
geo[!is.na(N)]$top_city = 1
geo[is.na(City)]$top_city = NA
geo[City==""]$top_city = NA

geo = geo[order(PersonId)]
Geographic_ind = geo$top_city
Geographic_ind = coCovar(Geographic_ind,nodeSet='receivers')

# 5) experience
Experience_ind = c_data_ent$experience.ent
Experience_ind = coCovar(Experience_ind,nodeSet='receivers')

# 6) business
Business_ind = c_data_ent$buss.ent
Business_ind = Business_ind-1 # convert from (1,2) to (0,1)
Business_ind = coCovar(Business_ind,nodeSet='receivers')

# 7) tech
Tech_ind = c_data_ent$tech.ent
Tech_ind = Tech_ind-1 # convert from (1,2) to (0,1)
Tech_ind = coCovar(Tech_ind,nodeSet='receivers')

# 8) venture round
ent_round = merge(execs,deal[,c('CompanyId','year')],by="CompanyId",
                  all.x=T, allow.cartesian=T)
ent_round = merge(ent_round,data.table(PersonId=ent_list),by="PersonId")
ent_round[year.join<=year,rounds:=.N,by=PersonId]
ent_round[,rounds:=max(rounds,na.rm=T),by=PersonId]
Round_ind = unique(ent_round[,c("PersonId","rounds")],by="PersonId")
Round_ind = Round_ind[order(PersonId)]$rounds
Round_ind = coCovar(Round_ind,nodeSet='receivers')

# 3.TIME JOIN AND LEAVE --------------------------------------------------------

time_inv = copy(c_data)
time_inv[,join:=min(year,na.rm=T)-min_year+1,by=PersonId.inv]
time_inv[,leave:=max(year,na.rm=T)-min_year+2,by=PersonId.inv]
time_inv[leave>max(year,na.rm=T)-min_year+1]$leave = 
  time_inv[leave>max(year,na.rm=T)-min_year+1]$leave - 1
time_inv = unique(time_inv[,c("PersonId.inv","join","leave")],by="PersonId.inv")
time_inv = time_inv[order(PersonId.inv)][,c(2,3)]
time_inv = as.list(as.data.table(t(as.matrix(time_inv))))

time_effect_inv = sienaCompositionChange(time_inv, nodeSet = "senders")

time_ent = copy(c_data)
time_ent[,join:=min(year,na.rm=T)-min_year+1,by=PersonId.ent]
time_ent[,leave:=max(year,na.rm=T)-min_year+2,by=PersonId.ent]
time_ent[leave>max(year,na.rm=T)-min_year+1]$leave = 
  time_ent[leave>max(year,na.rm=T)-min_year+1]$leave - 1
time_ent = unique(time_ent[,c("PersonId.ent","join","leave")],by="PersonId.ent")
time_ent = time_ent[order(PersonId.ent)][,c(2,3)]
time_ent = as.list(as.data.table(t(as.matrix(time_ent))))

time_effect_ent = sienaCompositionChange(time_ent, nodeSet = "receivers")

# GATHER ALL EFFECTS
Siena = sienaDataCreate(dependent,
                        # dyadic
                        Ethnic_homo,Gender_homo,TopSchool_homo,
                        Geographic_homo,Experience_homo,Degree_homo,
                        # constant
                        Ethnic_ind,Gender_ind,TopSchool_ind,Geographic_ind,
                        Experience_ind,Business_ind,Tech_ind,Round_ind,
                        # time
                        time_effect_inv,time_effect_ent,
                        nodeSets=list(senders,receivers))

Effects = getEffects(Siena)
Effects = includeEffects(Effects,cycle4,outActSqrt,inPopSqrt,outInAss)
Effects = includeEffects(Effects,X,interaction1="Ethnic_homo")
Effects = includeEffects(Effects,X,interaction1="Gender_homo")
Effects = includeEffects(Effects,X,interaction1="TopSchool_homo")
Effects = includeEffects(Effects,X,interaction1="Geographic_homo")
Effects = includeEffects(Effects,X,interaction1="Experience_homo")
Effects = includeEffects(Effects,X,interaction1="Degree_homo")
Effects = includeEffects(Effects,altX,interaction1="Ethnic_ind")
Effects = includeEffects(Effects,altX,interaction1="Gender_ind")
Effects = includeEffects(Effects,altX,interaction1="TopSchool_ind")
Effects = includeEffects(Effects,altX,interaction1="Geographic_ind")
Effects = includeEffects(Effects,altX,interaction1="Experience_ind")
Effects = includeEffects(Effects,altX,interaction1="Business_ind")
Effects = includeEffects(Effects,altX,interaction1="Tech_ind")
Effects = includeEffects(Effects,altX,interaction1="Round_ind")

sienaModelCreate(cond = FALSE)
Algo=sienaAlgorithmCreate(projname='CoEvol_results',diagonalize = 0.2)

#siena_result = siena07(Algo,data=Siena,effects=Effects,initC = TRUE)

#siena_result

#siena_result2 = siena07(Algo,data=Siena,effects=Effects,nbrNodes=2, 
                       #useCluster=TRUE,initC = TRUE,prevAns=siena_result)


