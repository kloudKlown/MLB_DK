use mlb;

select distinct * from pitchers limit 100000;
select distinct * from batters limit 100000;

SELECT distinct substr(Player,1,locate('(',Player)-2) FROM Batters;

select PLayerName from BBsavantBatters LIMIT 10;



select distinct * from  batters where Date like '%2018%' and Player like '%Cano%';
select distinct * from  pitchers where Date like '%2018%' limit 100000;

select * from bbsavantbatters_2018;
select * from bbsavantbatters_2017;

select * from bbsavantpitchers_2018;
select * from bbsavantpitchers_2017;

-- delete from batters;

-- delete from  pitchers where Date like '%2018%' and 'Act Pts' is not NULL;

-- delete from  batters where Date like '%2018%' and 'Act Pts' is not NULL;

--  delete from  originallist