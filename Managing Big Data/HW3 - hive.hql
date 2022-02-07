DROP TABLE IF EXISTS nyTaxi;
CREATE EXTERNAL TABLE nyTaxi
     (VendorID INT,
     lpep_pickup_datetime STRING,
     lpep_dropoff_datetime STRING,
     store_and_fwd_flag STRING,
     RatecodeID INT,
     PULocationID INT,
     DOLocationID INT,
     passenger_count INT,
     trip_distance FLOAT,
     fare_amount FLOAT,
     extra FLOAT,
     mta_tax FLOAT,
     tip_amount FLOAT,
     tolls_amount FLOAT,
     ehail_fee FLOAT,
     improvement_surcharge FLOAT,
     total_amount FLOAT,
     payment_type INT,
     trip_type INT)
ROW FORMAT DELIMITED
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
STORED AS TEXTFILE
LOCATION 's3://671zhua273hive/hive';

LOAD DATA INPATH 's3://671zhua273hive/hive/tripdata.csv' INTO TABLE nyTaxi;

SELECT DISTINCT RatecodeID from nyTaxi;

SELECT * FROM nyTaxi WHERE RatecodeID==1;