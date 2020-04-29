


startMyProject()

##  Norway (Grunkretser) ----
grunnkretser18_path <-
  dataBase_path %>%
  file.path('Norway/Basisdata_0000_Norge_25833_Grunnkretser2018_GML.gml')
##  literally a line file

x <- st_read(grunnkretser18_path)
##  Okay the above doesn't work but how about Sweden -----

## We have downloaded all the stats files
deso_path <-
  dataBase_path %>%
  file.path('Sweden/DeSO')

y <-
  st_read(
  dsn = deso_path,
  layer = 'Bef_Bakgrund_region'
  )

##  I assume that sv_bakgr is sweish and utl_bakgr is other?
y[, 1] %>% plot
##  okay so only some regions are there?
