-------------------------
--get adjacent tazs------
-------------------------
-- Final solution is to measure longest line
-- within small buffered overlap. This accounts for
-- polygons that should touch, but due to data 
-- quality issues are slightly separated along their
-- borders. Measurement of longest line will be slightly
-- off from actual border, but not by enough to matter.

drop table if exists lehd.tmp_joe;
create table lehd.tmp_joe as
(with q as (
select ST_Buffer(geom, 5.0) geom, geom geom_actual, newtaz from lehd.taz2161 
where empdensitylehd > 2000
order by newtaz)

select a.newtaz a_taz, b.newtaz b_taz, ST_Union(a.geom_actual, b.geom_actual) geom,
  ST_Length(ST_LongestLine(ST_Intersection(a.geom, b.geom), ST_Intersection(a.geom, b.geom))) / 5280 longest_miles
from q a join q b on (ST_Intersects(a.geom, b.geom) and 
    ST_Length(ST_LongestLine(ST_Intersection(a.geom, b.geom), 
    ST_Intersection(a.geom, b.geom))) / 5280 >= 0.1)
where a.newtaz != b.newtaz
order by a.newtaz, b.newtaz);