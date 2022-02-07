daily = LOAD 's3://671nysezhua273/NYSE_daily' AS (nyse:chararray, stock:chararray, date:chararray, p1:float, p2:float, p3:float, p4:float, volume:int, close_price:float);

dividends = LOAD 's3://671nysezhua273/NYSE_dividends' AS (nyse:chararray, stock:chararray, date:chararray, dividends:float);

joined = JOIN daily BY (stock, date),  dividends BY (stock, date); 

dp = FOREACH joined GENERATE *, dividends / close_price AS div_per_close: float;

desc_dp = ORDER dp BY div_per_close DESC;
dpmax = LIMIT desc_dp 1; 

asc_dp = ORDER dp BY div_per_close ASC;
dpmin = LIMIT asc_dp 1; 

dump dpmax;
dump dpmin
