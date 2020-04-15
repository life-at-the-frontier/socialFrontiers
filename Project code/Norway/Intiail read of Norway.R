
startMyProject()

grunnkretser18_path <-
  dataBase_path %>%
  file.path('Norway/Basisdata_0000_Norge_25833_Grunnkretser2018_GML.gml')
##  literally a line file

x <- st_read(grunnkretser18_path)

