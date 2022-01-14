## required inputs: FIPS file AP3, NAICS mapping, 
## facilities file with emissions, marginal damages low, medium, tall, tall2
## income adjustment coefficients, wildfire files from EPA
## tall, tall2 files
## effective heights files for 2014 and 2017

## Load Packages, set working directory
library(tidyverse)
library(foreign)
library(readstata13)

setwd('C:/Users/ptsch/Documents/GitHub/CompanionPaper')

# Income adjustment factors can also be found in separate .csv file
inc.adjustment<-tibble(c(2008, 2011, 2014, 2017),
                       c(0.999312286, 0.995604867, 1.013200986, 1.032267994))
names(inc.adjustment)<-c('Year', 'Factor')


### Quick overview over which dollar value the VMR parameter has each year
9421854*0.995604867 #2011 VMR
9421854*1.013200986 #2014 VMR
9421854*1.032267994 #2017 VMR

## Some useful groups
mds<-c('MD.NH3', 'MD.NOx', 'MD.PM25', 'MD.SO2', 'MD.VOC')
emi<-c('NH3', 'NOx', 'PM25', 'SO2', 'VOC')
e.emi<-c('E.NH3', 'E.NOx', 'E.PM25', 'E.SO2', 'E.VOC')
ged<-c('GED.NH3', 'GED.NOx', 'GED.PM25', 'GED.SO2', 'GED.VOC')

## Read in FIPS list for correct order
FIPS<-read.csv('fips_apeep.csv')
names(FIPS)[3]<-"FIPS"
FIPS<-FIPS[3]

# Function Inputs: FIPS file AP3, 
## facilities file with emissions, marginal damages low, medium, tall, tall2
## tall, tall2 stata files
## effective heights files for 2014 and 2017
## 
# Function Outputs: 'facilities_emi_md_ged_[year]' .csv file
ged.sectoral.facilities<-function(year){
  ## Read in AP3 Tall and Tall 2 data, remove unneeded columns
  tall<-read.dta13("Tall1_List_AP2.dta")
  tall<-tall[, c('eis', 'fips')]
  names(tall)<-c('eis', 'FIPS')
  tall2<-read.dta13("Tall2_List_AP2_Update.dta")
  tall2<-tall2[, c('eisidentifier', 'fips2')]
  names(tall2)<-c("eis", 'FIPS')
  tall2$FIPS<-as.integer(tall2$FIPS)
  
  facilities<-read.csv(paste(year, '_facilities.csv', sep=''))
  facilities<-facilities[, c('eis', 'FIPS', 'NAICS',emi)]
  # effective heights available only for 2014 and 2017
  if (year < 2015){
    eff.heights<-read.table('eff_heights_2014.csv', sep = ',', header = T)
  } else {
    eff.heights<-read.table('eff_heights_2017.csv', sep = ',', header = T)}
  eff.heights<-eff.heights[, c('eis', 'FIPS', 'eff_height', 
                               'ap3.assignment', 'easiur.assignment')]
  
  tall.only<-tall%>% group_by(eis) %>% summarize()
  tall2.only<-tall2 %>% group_by(eis) %>% summarize()
  
  # Remove duplicates from Tall 2 (they are contained in Tall)
  tall2.only<-tall2.only[!tall2.only$eis==717611,]
  tall2.only<-tall2.only[!tall2.only$eis==8183111,]
  tall2.only<-tall2.only[!tall2.only$eis==7335511,]
  tall2.only<-tall2.only[!tall2.only$eis==6815611,]

  # merge tall/2 with fac
  tall.only<-left_join(tall.only, facilities)
  tall2.only<-left_join(tall2.only, facilities)
  
  tall.only$eff_height<-999
  tall.only$ap3.assignment<-'Tall'
  tall.only$easiur.assignment<-'Tall 300'
  
  tall2.only$eff_height<-999
  tall2.only$ap3.assignment<-'Tall2'
  tall2.only$easiur.assignment<-'Tall 300'
  
  facilities<-anti_join(facilities, tall.only, by='eis')
  facilities<-anti_join(facilities, tall2.only, by = 'eis')
  
  # merge eff heights with facilities
  facilities.low_med<-left_join(facilities, eff.heights)
  facilities.low_med$ap3.assignment[is.na(facilities.low_med$ap3.assignment)] = 'Low'
  facilities.low_med$easiur.assignment[is.na(facilities.low_med$easiur.assignment)] = 'Area'
  
  ## Read in MD files, FIPS codes
  MD.L<-read.csv((paste('md_L_', year, '.csv', sep='')), header = F)
  colnames(MD.L)<-mds
  MD.L<-cbind(FIPS, MD.L)
  MD.L$ap3.assignment<-'Low'
  MD.M<-read.csv((paste('md_M_', year, '.csv', sep='')), header = F)
  colnames(MD.M)<-mds
  MD.M<-cbind(FIPS, MD.M)
  MD.M$ap3.assignment<-'Medium'
  MD.T<-read.csv((paste('md_T_', year, '.csv', sep='')), header = F)
  colnames(MD.T)<-mds
  MD.T$ap3.assignment<-'Tall'
  MD.T<-cbind(tall, MD.T)
  MD.T<-MD.T %>% group_by(eis) %>% summarize_all(first)
  
  MD.T2<-read.csv((paste('md_T2_', year, '.csv', sep='')), header = F)
  colnames(MD.T2)<-mds
  MD.T2$ap3.assignment<-'Tall2'
  MD.T2<-cbind(tall2, MD.T2)
  MD.T2<-MD.T2 %>% group_by(eis) %>% summarize_all(first)
  MD.T2<-MD.T2[!MD.T2$eis==717611,]
  MD.T2<-MD.T2[!MD.T2$eis==8183111,]
  MD.T2<-MD.T2[!MD.T2$eis==7335511,]
  MD.T2<-MD.T2[!MD.T2$eis==6815611,]
  
  MD.fac<-rbind(MD.L, MD.M)
  
  facilities.low_med<-left_join(facilities.low_med, MD.fac, by=c('FIPS', 'ap3.assignment'))
  tall.only<-left_join(tall.only, MD.T)
  tall2.only<-left_join(tall2.only, MD.T)
  
  
  facilities.grouped<-rbind(facilities.low_med , tall.only, tall2.only)
  facilities.grouped<-facilities.grouped[-which(is.na(facilities.grouped$FIPS)),]
  rm(facilities, facilities.low_med, tall.only, tall2.only)
  rm(MD.L, MD.M, MD.T, MD.T2, MD.fac)
  
  facilities.GED<-facilities.grouped[, emi] * facilities.grouped[, mds]
  names(facilities.GED)<-ged
  facilities.GED<-facilities.GED %>% mutate(GED.sum = rowSums(facilities.GED))
  print(colSums(facilities.GED, na.rm=T)/10^9)
  facilities.final<-cbind(facilities.grouped, facilities.GED)
  write.table(facilities.final, paste('facilities_emi_md_ged_', year, '.csv', sep=''),
              sep=',')
}

#ged.sectoral.facilities(2008)
ged.sectoral.facilities(2011)
ged.sectoral.facilities(2014)
ged.sectoral.facilities(2017)

# Function Inputs: FIPS file AP3, 
## area source file with emissions, marginal damages area
## scc to naics mapping file
## 
# Function Outputs: 'facilities_emi_md_ged_[year]' .csv file
ged.sectoral.area<-function(year){
  area.sources<-read.table(paste('area_for_naics_', year, '.csv', sep=''), 
                           sep = ',',header = T)
  ## Remove non-contiguous areas by FIPS code and tribal emissions
  # Creating a vector with every possible FIPS code for the 5 non-contiguous areas
  # plus water areas and whatever 88xxx code is
  AK<-2000:2999
  AS<-60000:60999
  HI<-15000:15999
  PR<-72000:72999
  VI<-78000:78999
  rest<-c(85000:85999, 88000:88999)
  
  non.cont<-as.data.frame(c(AK, AS, HI, PR, VI, rest))
  colnames(non.cont)[1]<-"FIPS"
  area.sources<-anti_join(area.sources, non.cont)
  
  rm(non.cont, AK, AS, HI, PR, VI, rest)
  
  
  area.sources<-area.sources %>% 
    pivot_wider(names_from = Pollutant, values_from = Emissions)
  
  print(colSums(area.sources[, 6:11], na.rm=T))
  #area.sources$SCC<-as.integer(area.sources$SCC)
  MD.A<-read.csv((paste('md_A_', year, '.csv', sep='')), header = F)
  colnames(MD.A)<-mds
  MD.A<-cbind(FIPS, MD.A)
  scc.list<-read.table('scc_list_new_sector_layout.csv', sep=',', header = T)
  #scc.list$SCC<-as.numeric(as.character(scc.list$SCC))
  #scc.list<-scc.list[-which(is.na(scc.list$SCC)),]
  area.sources<-left_join(area.sources, scc.list, by = 'SCC')
  
  write.table(area.sources, paste('area_sources_detailed_', year, '.csv', sep=''),
              sep=',', row.names = F)
  
  ###### stop here for detailed analysis on area sources
  
  area.sources<-area.sources[, c('FIPS', 'SCC', 'Ammonia', 'Nitrogen Oxides',
                                 'PM2.5 Primary (Filt + Cond)', 'Sulfur Dioxide',
                                 'VOC_A', 'VOC_B', 'naics.sector', 'naics.subsector')]
  names(area.sources)<-c('FIPS', 'SCC', emi[1:4], 'VOC_A', 'VOC_B',
                         'naics.sector', 'naics.subsector')
  # group by sector and FIPS
  area.sources.sector<-area.sources %>% group_by_at(vars(FIPS, naics.sector)) %>% 
    summarize(
    NH3 = sum(NH3, na.rm=T),
    NOx = sum(NOx, na.rm=T),
    PM25 = sum(PM25, na.rm=T),
    SO2 = sum(SO2, na.rm=T),
    VOC_A = sum(VOC_A, na.rm=T),
    VOC_B = sum(VOC_B, na.rm=T),
    naics.subsector = first(naics.subsector))

  area.sources.sector<-left_join(area.sources.sector, MD.A, by = 'FIPS')
  
  area.sources.sector<-area.sources.sector %>% mutate(VOC = VOC_A + VOC_B)
  area.sources.sector<-area.sources.sector[, c('FIPS', 'NH3', 'NOx', 'PM25', 'SO2',
                                               'VOC', mds, 
                                               'naics.sector', 'naics.subsector')]
  area.sources.ged<-area.sources.sector[, emi] * area.sources.sector[, mds]
  names(area.sources.ged)<-ged
  area.sources.ged<-area.sources.ged %>% mutate(GED.sum = rowSums(area.sources.ged))
  print(colSums(area.sources.ged, na.rm=T)/10^9)
  area.sources.final<-cbind(area.sources.sector, area.sources.ged)
  write.table(area.sources.final, paste('area_sources_emi_md_ged_', year, '.csv', sep=''),
              sep=',', row.names = F)
}

#ged.sectoral.area(2008)
ged.sectoral.area(2011)
ged.sectoral.area(2014)
ged.sectoral.area(2017)

#year<-2017
ged.totals<-function(year){
  area<-read.table(paste('area_sources_emi_md_ged_', year, '.csv', sep=''),
                   sep=',', header = T)
  facilities<-read.table(paste('facilities_emi_md_ged_', year, '.csv', sep=''),
                   sep=',')
  ## Read in NAICS Mapping file
  naics.map<-read.csv('sector_map.csv')[, 1:3]
  names(naics.map)<-c('NAICS', 'naics.sector', 'naics.subsector')
  
  ## Read in VA data and inflate (July 2012 to July 2020: 1.13093)
  va.data<-read.table('VA_for_ModelYears.csv', sep=',', header = T)
  va.data[, 2:ncol(va.data)]<-va.data[, 2:ncol(va.data)] * 1.13093
  
  va.data<-va.data[, c('Sector', paste('VA', year, sep='.'))]

  
  ## join by naics code
  facilities<-left_join(facilities, naics.map)
  
  ## income adjust and inflate
  ## $9421854 in July 2020 price
  ## MDs are in $9186210; 9421854/9186210 = 1.025652
  inflator <- 1.025652
  adj.factor <- as.double( inc.adjustment[which(inc.adjustment$Year == year), 'Factor'])
  
  # replace if functions by better version later
  area[, 14:19]<-area[, 14:19]*inflator *adj.factor
  
  facilities[, 17:22]<-facilities[, 17:22]*inflator *adj.factor

  
  # by sector
  area.sector<-area %>% group_by(naics.sector) %>% 
    summarize(
      E.NH3 = sum(NH3, na.rm=T),
      E.NOx = sum(NOx, na.rm=T),
      E.PM25 = sum(PM25, na.rm=T),
      E.SO2 = sum(SO2, na.rm=T),
      E.VOC = sum(VOC, na.rm=T),
      GED.NH3 = sum(GED.NH3, na.rm=T),
      GED.NOx = sum(GED.NOx, na.rm=T),
      GED.PM25 = sum(GED.PM25, na.rm=T),
      GED.SO2 = sum(GED.SO2, na.rm=T),
      GED.VOC = sum(GED.VOC, na.rm=T),
      GED.sum = sum(GED.sum, na.rm=T))
  # by subsector
  area.subsector<-area %>% group_by(naics.subsector) %>% 
    summarize(
      E.NH3 = sum(NH3, na.rm=T),
      E.NOx = sum(NOx, na.rm=T),
      E.PM25 = sum(PM25, na.rm=T),
      E.SO2 = sum(SO2, na.rm=T),
      E.VOC = sum(VOC, na.rm=T),
      GED.NH3 = sum(GED.NH3, na.rm=T),
      GED.NOx = sum(GED.NOx, na.rm=T),
      GED.PM25 = sum(GED.PM25, na.rm=T),
      GED.SO2 = sum(GED.SO2, na.rm=T),
      GED.VOC = sum(GED.VOC, na.rm=T),
      GED.sum = sum(GED.sum, na.rm=T))

  # by sector
  facilities.sector<-facilities %>% group_by(naics.sector) %>% 
    summarize(
      E.NH3 = sum(NH3, na.rm=T),
      E.NOx = sum(NOx, na.rm=T),
      E.PM25 = sum(PM25, na.rm=T),
      E.SO2 = sum(SO2, na.rm=T),
      E.VOC = sum(VOC, na.rm=T),
      GED.NH3 = sum(GED.NH3, na.rm=T),
      GED.NOx = sum(GED.NOx, na.rm=T),
      GED.PM25 = sum(GED.PM25, na.rm=T),
      GED.SO2 = sum(GED.SO2, na.rm=T),
      GED.VOC = sum(GED.VOC, na.rm=T),
      GED.sum = sum(GED.sum, na.rm=T))
  # by subsector
  facilities.subsector<-facilities %>% group_by(naics.subsector) %>% 
    summarize(
      E.NH3 = sum(NH3, na.rm=T),
      E.NOx = sum(NOx, na.rm=T),
      E.PM25 = sum(PM25, na.rm=T),
      E.SO2 = sum(SO2, na.rm=T),
      E.VOC = sum(VOC, na.rm=T),
      GED.NH3 = sum(GED.NH3, na.rm=T),
      GED.NOx = sum(GED.NOx, na.rm=T),
      GED.PM25 = sum(GED.PM25, na.rm=T),
      GED.SO2 = sum(GED.SO2, na.rm=T),
      GED.VOC = sum(GED.VOC, na.rm=T),
      GED.sum = sum(GED.sum, na.rm=T))

  ged.combined.sector<-rbind(area.sector[, c('naics.sector', e.emi, ged, 'GED.sum')],
                             facilities.sector[, c('naics.sector', e.emi, ged, 'GED.sum')])
  ged.combined.subsector<-rbind(area.subsector[, c('naics.subsector', e.emi, ged, 'GED.sum')],
                             facilities.subsector[, c('naics.subsector', e.emi, ged, 'GED.sum')])

  sector.total<-ged.combined.sector %>% group_by(naics.sector) %>%
    summarize(E.NH3 = sum(E.NH3, na.rm=T),
              E.NOx = sum(E.NOx, na.rm=T),
              E.PM25 = sum(E.PM25, na.rm=T),
              E.SO2 = sum(E.SO2, na.rm=T),
              E.VOC = sum(E.VOC, na.rm=T),
              GED.NH3 = sum(GED.NH3, na.rm=T),
              GED.NOx = sum(GED.NOx, na.rm=T),
              GED.PM25 = sum(GED.PM25, na.rm=T),
              GED.SO2 = sum(GED.SO2, na.rm=T),
              GED.VOC = sum(GED.VOC, na.rm=T),
              GED.sum = sum(GED.sum, na.rm=T))
  subsector.total<-ged.combined.subsector %>% group_by(naics.subsector) %>%
    summarize(E.NH3 = sum(E.NH3, na.rm=T),
              E.NOx = sum(E.NOx, na.rm=T),
              E.PM25 = sum(E.PM25, na.rm=T),
              E.SO2 = sum(E.SO2, na.rm=T),
              E.VOC = sum(E.VOC, na.rm=T),
              GED.NH3 = sum(GED.NH3, na.rm=T),
              GED.NOx = sum(GED.NOx, na.rm=T),
              GED.PM25 = sum(GED.PM25, na.rm=T),
              GED.SO2 = sum(GED.SO2, na.rm=T),
              GED.VOC = sum(GED.VOC, na.rm=T),
              GED.sum = sum(GED.sum, na.rm=T))
  write.table(sector.total, paste('GED_companion_Sector_', year, '.csv', sep=''), 
              sep=',', row.names=F)
  write.table(subsector.total, paste('GED_companion_Subsector_', year, '.csv', sep=''), 
              sep=',', row.names=F)
}
ged.totals(2011)
ged.totals(2014)
ged.totals(2017)

#######

#setwd('C:/Users/ptsch/Documents/Github/Wildfires')

#mds<-c('MD.NH3', 'MD.NOx', 'MD.PM25', 'MD.SO2', 'MD.VOC')
#emi<-c('NH3', 'NOx', 'PM25', 'SO2', 'VOC')
#ged<-c('GED.NH3', 'GED.NOx', 'GED.PM25', 'GED.SO2', 'GED.VOC')

#SCC.list.EPA<-read.csv('SCC_List_L4.csv',header =T)
#SCC.list.EPA$SCC<-as.numeric(SCC.list.EPA$SCC)


wildfire.NEI.detailed<-function(year){
  wildfire.raw<-read_delim(paste(year, '_eventfires_countyscc.csv', sep=''),
                           delim = ',', col_names = T, )
  if (year == 2011){
    wildfire.filtered<-wildfire.raw[, c('state_and_county_fips_code', 'scc', 
                                        'description', 'total_emissions')]}
  
  if (year == 2014){
    wildfire.filtered<-wildfire.raw[, c('fips', 'SCC', 
                                        'pollutant desc', 'total emissions')]
  }
  
  if (year == 2017){
    wildfire.filtered<-wildfire.raw[, c('fips code', 'scc',
                                        'pollutant desc', 'total emissions')]}
  
  names(wildfire.filtered)<- c('FIPS', 'SCC', 'description', 'emissions')
  
  wildfire.filtered<-wildfire.filtered %>% filter(description == "Sulfur Dioxide" |
                                                    description == "Ammonia" |
                                                    description == "Nitrogen Oxides" |
                                                    description == "PM2.5 Primary (Filt + Cond)" |
                                                    description == "Volatile Organic Compounds" |
                                                    description == 'Methane' |
                                                    description == "Carbon Dioxide")
  
  wildfire.wide<-wildfire.filtered %>% 
    pivot_wider(id_cols = c('FIPS', 'SCC'), 
                names_from = description,
                values_from = emissions)
  write.table(wildfire.wide, paste('wildfires_detailed_', year, '.csv', sep = ''), 
              sep = ',', row.names = F)
}
wildfire.NEI.detailed(2011)
wildfire.NEI.detailed(2014)
wildfire.NEI.detailed(2017)

ged.wildfires<-function(year){
  setwd('C:/Users/ptsch/Documents/GitHub/Wildfires')
  wildfires<-read.table(paste('wildfires_detailed_', year, '.csv', sep=''), 
                           sep = ',',header = T)
  setwd('C:/Users/ptsch/Documents/GitHub/CompanionPaper')
  ## Remove non-contiguous areas by FIPS code and tribal emissions
  # Creating a vector with every possible FIPS code for the 5 non-contiguous areas
  # plus water areas and whatever 88xxx code is
  AK<-2000:2999
  AS<-60000:60999
  HI<-15000:15999
  PR<-72000:72999
  VI<-78000:78999
  rest<-c(85000:85999, 88000:88999)
  
  non.cont<-as.data.frame(c(AK, AS, HI, PR, VI, rest))
  colnames(non.cont)[1]<-"FIPS"
  wildfires<-anti_join(wildfires, non.cont)
  
  rm(non.cont, AK, AS, HI, PR, VI, rest)
  
  print(colSums(wildfires[, 3:9], na.rm=T))
  emission.totals<-data.frame(colSums(wildfires[, 3:9], na.rm=T))
  names(emission.totals)<-paste('Y_', year, sep='')
  wildfire.climate<-as_tibble(colSums(wildfires[, c('Methane', 'Carbon.Dioxide')], na.rm=T))
  names(wildfire.climate)<-paste('Y.', year, sep = '')
  write.table(wildfire.climate, paste('wildfire_climate_', year, '.csv', sep=''),
              sep=',')
  #wildfires$SCC<-as.integer(wildfires$SCC)
  MD.A<-read.csv((paste('md_A_', year, '.csv', sep='')), header = F)
  colnames(MD.A)<-mds
  MD.A<-cbind(FIPS, MD.A)

  wildfires<-wildfires[, c('FIPS', 'Ammonia', 'Nitrogen.Oxides',
                                 'PM2.5.Primary..Filt...Cond.', 'Sulfur.Dioxide',
                                 'Volatile.Organic.Compounds')]
  names(wildfires)<-c('FIPS', emi)
  
  # group by sector and FIPS
  wildfires<-wildfires %>% group_by(FIPS) %>% 
    summarize(
      NH3 = sum(NH3, na.rm=T),
      NOx = sum(NOx, na.rm=T),
      PM25 = sum(PM25, na.rm=T),
      SO2 = sum(SO2, na.rm=T),
      VOC = sum(VOC, na.rm=T))
  
  wildfires<-left_join(wildfires, MD.A, by = 'FIPS')

  wildfires.GED<-wildfires[, emi] * wildfires[, mds]
  
  # inflate and income-adjust
  inc.adjustment<-tibble(c(2008, 2011, 2014, 2017),
                         c(0.999312286, 0.995604867, 1.013200986, 1.032267994))
  names(inc.adjustment)<-c('Year', 'Factor')
  
  ## income adjust and inflate
  ## $9421854 in July 2020 price
  ## MDs are in $9186210; 9421854/9186210 = 1.025652
  inflator <- 1.025652
  adj.factor <- as.double(inc.adjustment[which(inc.adjustment$Year == year), 'Factor'])
  # 
  wildfires.GED<-wildfires.GED * inflator * adj.factor
  
  names(wildfires.GED)<-ged
  wildfires.GED<-wildfires.GED %>% mutate(GED.sum = rowSums(wildfires.GED))


  print(colSums(wildfires.GED, na.rm=T)/10^9)
  wildfires.final<-cbind(wildfires, wildfires.GED)
  write.table(wildfires.final, paste('wildfires_emi_md_ged_', year, '.csv', sep=''),
              sep=',', row.names = F)
}

ged.wildfires(2011)
ged.wildfires(2014)
ged.wildfires(2017)
