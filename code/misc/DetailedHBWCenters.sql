----------------------------------------------------------------------------------------------
--The whole code can be used to get employment centers based on total employment of LEHD
----------------------------------------------------------------------------------------------
-- 1. first step (from line 4 to line 24) is to calculate the employment density for each TAZ
----------------------------------------------------------------------------------------------
--add column employment density 
--------------------------------
alter table lehd.taz2161 add column empdensitylehd numeric;


--------------------------------------------------------------
-- get and undate the employment density of LEHD totempdensity 
--------------------------------------------------------------
begin;
with w as (
select newtaz, totemplehd/(f_area*0.0929/1000000) as empdensitylehd from lehd.taz2161 order by newtaz) 

update lehd.taz2161 t1
set empdensitylehd=t2.empdensitylehd
from w t2
where t1.newtaz = t2.newtaz;
commit;
--------------------------------------------------
--2. second step (from line 24 to 40) to  is to identify adjacent TAZs
--------------------------------------------------
--get adjacent tazs------
-------------------------

with q as (
select * from lehd.taz2161 
where empdensitylehd > 2000
order by newtaz)


select a.newtaz newtaz2000, b.newtaz newtaz_b, a.empdensitylehd, b.empdensitylehd empdensitylehd_b, a.totemplehd, b.totemplehd totemplehd_b
from q a join q b on (ST_DWithin(a.geom, b.geom, 1.0))
where a.newtaz != b.newtaz
order by a.newtaz, b.newtaz
;

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- 2.1 the resutls of this step is exported as "empDensityLEHD2000.csv", and this file in imported into python to get the affiliation to TAZ and centres, the result is save as 
--- "groupsLEHD2000.csv". Then file "groupsLEHD2000.csv" is upload to server(sapporo) as table lehd.groupsLEHD2000 in putty.
      
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------
--- 3. the third step (from line 49 to line 61)  is to add index of groups (centers) to table lehd.taz2161
------------------------------------------------------------------------------
--update groupLEHD***(index of group) 
---------------------------------------------

alter table lehd.taz2161 add column groupLEHD2000 integer;

begin;
update lehd.taz2161 t1
set groupLEHD2000=t2.group
from lehd."groupsLEHD2000" t2
where t1.newtaz = t2.id;
commit;

-------------------------------------------------------------------------------------------------------------
--- 4. the fourth step (from line 64 to line 79) is to calculate the total employment of  group (centers)---- 
-------------------------------------------------------------------------------------------------------------
--update group***totemp(total employment of group) 
----------------------------------------------------

alter table lehd.taz2161 add column grouplehd2000totemp numeric;

begin;
with w as (select grouplehd2000, sum(totemplehd) from lehd.taz2161 group by grouplehd2000 order by grouplehd2000)


update lehd.taz2161 t1
set grouplehd2000totemp=t2.sum
from w t2
where t1.grouplehd2000 = t2.grouplehd2000;
commit;
