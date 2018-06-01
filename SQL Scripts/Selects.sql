

select distinct * from mlb.batters where Date like '%2018%' limit 50000;
select distinct * from mlb.pitchers where Date like '%6/9/2017%' limit 50000;


select distinct * from mlb.batters where Date like '%2018%' limit 50000;
select distinct * from mlb.pitchers where Date like '%2018%' limit 50000;

-- delete from mlb.batters where Date like '%2018%';
-- delete from mlb.pitchers where Date like '%2018%';


select distinct Player, PlayerID, concat('https://baseballsavant.mlb.com/player?player_id=',PlayerID,'&pos=SS&player_type=batter&season=2018&tab=zone_chart_tab#'), Pos from mlb.originallist;

-- delete from mlb.bbsavantbatters_2018

select * from mlb.bbsavantbatters_2018
