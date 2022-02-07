create database HW1_airbnb;
use HW1_airbnb;

# 1. create a full table to load data
DROP TABLE IF EXISTS full_data;
create table full_data(
id int not null, 
`name` text,
host_id int,
host_name text,
neighbourhood_group text,
neighbourhood text,
latitude float,
longtitude float,
room_type text,
price int,
minimum_nights int,
number_of_reviews int,
last_review date,
reviews_per_month float,
calculated_host_listings_count int,
availability_365 int,
primary key (id)
);

# 2. load data to full tabel
load data local infile '/Users/huangzm/Desktop/big data/AirBnb_NYC_2019.csv'
into table full_data 
fields terminated by ',' 
ENCLOSED BY '"'
lines terminated by '\n'
ignore 1 lines
(@id,@`name`,@host_id,@host_name,@neighbourhood_group,@neighbourhood,
@latitude,@longtitude,@room_type,@price,@minimum_nights,@number_of_reviews,
@last_review,@reviews_per_month,@calculated_host_listings_count,@availability_365)
SET 
id = NULLIF(@id,''),
`name` = NULLIF(@`name`,''),
host_id = NULLIF(@host_id,''),
host_name = NULLIF(@host_name,''),
neighbourhood_group = NULLIF(@neighbourhood_group,''),
neighbourhood = NULLIF(@neighbourhood,''),
latitude = NULLIF(@latitude,''),
longtitude = NULLIF(@longtitude,''),
room_type = NULLIF(@room_type,''),
price = NULLIF(@price,''),
minimum_nights = NULLIF(@minimum_nights,''),
number_of_reviews = NULLIF(@number_of_reviews,''),
last_review = NULLIF(@last_review,''),
reviews_per_month = NULLIF(@reviews_per_month,''),
calculated_host_listings_count = NULLIF(@calculated_host_listings_count,''),
availability_365 = NULLIF(@availability_365,'');

# 3. create table "host" and load data
create table `host`(
host_id int,
host_name text,
primary key (host_id))
select distinct(host_id), host_name from full_data;

# 4. create table "neighbourhood" and load data
create table neighbourhood(
neighbourhood varchar(50),
neighbourhood_group text,
primary key (neighbourhood))
select distinct(neighbourhood), neighbourhood_group from full_data;

# 5. create table "room", load data, and add foreign keys
create table room(
id int not null, 
`name` text,
host_id int,
host_name text,
neighbourhood_group text,
neighbourhood varchar(50),
latitude float,
longtitude float,
room_type text,
price int,
minimum_nights int,
number_of_reviews int,
last_review date,
reviews_per_month float,
calculated_host_listings_count int,
availability_365 int,
primary key (id))
select * from full_data;

alter table room 
drop column host_name,
drop column neighbourhood_group,
add foreign key (host_id) references `host`(host_id),
add foreign key (neighbourhood) references neighbourhood(neighbourhood);
