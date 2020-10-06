# Tara Hudiburg
# 
# ===========================
# 
# CURRENT FILENAME: 

# User - Defined varibles/objects
# Must be defined by user
#########################################
directory=choose.dir()
file.name="LCAIn_or.csv"
file.name.out.1 = "LCAout_or.txt"
file.name.out.2 = "LCAout_or2.txt"

#Input file to read in

LCAIn <-read.table(paste0(directory,"\\",file.name),sep = ",",skip=1,col.names = c("ID","Region","State","Year","TotABC.TgCyr","Merch.TgCyr","Distance.to.mill", "Distance.to.use"))
first.year <- 1900
last.year <- 2050
##########################################

#clean up old objects from prior runs
rm(list = ls(pattern = "this.year.cohort")) 
##########################################

# Inititalize variables

LCA.out <- LCAIn    ##!!

# Initiate datasets
LCA.Annual <- NULL
LCA.Annual.2 <-NULL

# For different categories
num.rows <- length(unique(LCA.out[,"ID"]))

#===========================================
#LCA
#===================================
# LCA with sensitivity analysis (SA) included. The SA provides a range based on the assumptions. 

#============================================
# Product Chain coefficients (denoted by a .c)

# used for WD1 calculations; this does not vary for SA
#============================================

################################
#citations
#Smith, J. E., Heath, L., Skog, K. E., & Birdsey, R. (2006). Methods for Calculating Forest Ecosystem and Harvested Carbon with Standard Estimates 
#for Forest Types of the United States Gen. Tech. Rep. NE-343. Retrieved from Newtown Square, PA: 

#Harmon, M. E., Harmon, J. M., Ferrell, W. K., & Brooks, D. (1996). Modeling carbon stores in Oregon and Washington 
#forest products: 1900-1992. Climatic Change, 33(4), 521-550. 

#Harmon, M. E., Moreno, A., & Domingo, J. B. (2009). Effects of partial harvest on the carbon stores in 
#Douglas-fir/western hemlock forests: a simulation study. Ecosystems, 12(5), 777-791.

#Harmon, M. E., & Marks, B. (2002). Effects of silvicultural practices on carbon stores in Douglas-fir western hemlock forests in the Pacific 
#Northwest, USA: results from a simulation model. Canadian Journal of Forest Research, 32(5), 863-877. 

#Turner, D. P., Koerper, G. J., Harmon, M. E., & Lee, J. J. (1995). 
#A carbon budget for forests of the conterminous United States. Ecological Applications, 5(2), 421-436. 

#Smyth, C., Rampley, G., Lemprière, T. C., Schwab, O., & Kurz, W. A. (2017). Estimating product and energy 
#substitution benefits in national-scale mitigation analyses for Canada. GCB Bioenergy, 9(6), 1071-1084.

#Dugan, A. J., Birdsey, R., Mascorro, V. S., Magnan, M., Smyth, C. E., Olguin, M., & Kurz, W. A. (2018). A systems approach to assess climate 
#change mitigation options in landscapes of the United States forest sector. Carbon Balance and Management, 13(1), 13. 

#McNamee, P., Adams, P. W. R., McManus, M. C., Dooley, B., Darvell, L. I., Williams, A., & Jones, J. M. (2016). An assessment of the torrefaction of 
#North American pine and life cycle greenhouse gas emissions. Energy Conversion and Management, 113(Supplement C), 177-188. 
#doi:https://doi.org/10.1016/j.enconman.2016.01.006

#Dymond, C. C. (2012). Forest carbon in North America: annual storage and emissions from British Columbia's harvest, 1965-2065. 
#Carbon Balance and Management, 7, 8-8. doi:10.1186/1750-0680-7-8

#Skog, K. E. (2008). Sequestration of carbon in harvested wood products for the United States. 
#Forest products journal. Vol. 58, no. 6 (June 2008): Pages 56-72. 
################################

# General comments
#TotABC.TgCyr is the total aboveground biomass. 
#Merch.TgCyr is the growing stock volume/mass that goes to the mill (on the trucks)

#Turner et al (1995) and others have estimated that total ABC to merchantable = 1.30 to 2.0; This number has improved again in recent decades.
#Both at the harvest site and at the mill; Changes to this ratio regionally or over time are determined by the user in the input file

#Berg 2016 and TPO report 
#Only 2 to 8% of removed growing stock volume is a mill 'residue' (has declined in last decade). 
#However this does not include the non-growing stock residue (~30% of total ABC; foliage, branches, tops, stumps)
#and 8 - 11% (Pacific Coast Softwoods) of growing stock residues left behind as slash (often burned)

#This code calculates the fraction of growing stock volume removed that is roundwood AT THE MILL, versus fuelwood or bark 
#It relies on the user to determine how much slash is left implicitly behind over time in the input file (TotABC.TgCyr - Merch.TgCyr)
#This script uses these two variables to determine residues over time

###############################

#Fraction of growing stock volume that is softwood or hardwood. Table D8 Smith et al., 2006. These total to '1' and there are no losses.
fraction.pnw.e.sw.c <- .980
fraction.pnw.e.hw.c <- .020 
fraction.pnw.w.sw.c <- .890
fraction.pnw.w.hw.c <- .110

fraction.psw.sw.c <- .829
fraction.psw.hw.c <- .171

fraction.rm.n.sw.c <-.983
fraction.rm.n.hw.c <-.017
fraction.rm.s.sw.c <-.865
fraction.rm.s.hw.c <-.135

#fraction that is sawlog or pulpwood. Table D8 Smith et al., 2006. Fraction sw or hw that is sawtimber size (23 cm dbh cutoff)
# These also total to '1' and there are no losses.
fraction.pnw.e.sw.sawlog.c <- .865
fraction.pnw.e.hw.sawlog.c <- .501
fraction.pnw.w.sw.sawlog.c <- .911
fraction.pnw.w.hw.sawlog.c <- .538

fraction.pnw.e.sw.pulpwood.c <- 1.00 - fraction.pnw.e.sw.sawlog.c
fraction.pnw.e.hw.pulpwood.c <- 1.00 - fraction.pnw.e.hw.sawlog.c
fraction.pnw.w.sw.pulpwood.c <- 1.00 - fraction.pnw.w.sw.sawlog.c
fraction.pnw.w.hw.pulpwood.c <- 1.00 - fraction.pnw.w.hw.sawlog.c

fraction.psw.sw.sawlog.c <- .925
fraction.psw.hw.sawlog.c <- .308

fraction.psw.sw.pulpwood.c <- 1.00 - fraction.psw.sw.sawlog.c
fraction.psw.hw.pulpwood.c <- 1.00 - fraction.psw.hw.sawlog.c

fraction.rm.n.sw.sawlog.c <-.734
fraction.rm.n.hw.sawlog.c <-.442
fraction.rm.s.sw.sawlog.c <-.742
fraction.rm.s.hw.sawlog.c <-.337

fraction.rm.n.sw.pulpwood.c <-1.00 - fraction.rm.n.sw.sawlog.c
fraction.rm.n.hw.pulpwood.c <-1.00 - fraction.rm.n.hw.sawlog.c
fraction.rm.s.sw.pulpwood.c <-1.00 - fraction.rm.s.sw.sawlog.c
fraction.rm.s.hw.pulpwood.c <-1.00 - fraction.rm.s.hw.sawlog.c


# ratio roundwood to industrial roundwood (Table 5; Smith et al., 2006); This removes bark and fuelwood for the sawlog portions; some of this can go to paper stream
ratio.pnw.sw.sawlog.industrial.c <- .965
ratio.pnw.hw.sawlog.industrial.c <- .721
ratio.pnw.sw.pulpwood.industrial.c <- 1.099
ratio.pnw.hw.pulpwood.industrial.c <- .324

ratio.psw.sw.sawlog.industrial.c <- .965
ratio.psw.hw.sawlog.industrial.c <- .721
ratio.psw.sw.pulpwood.industrial.c <- 1.099
ratio.psw.hw.pulpwood.industrial.c <- .324

ratio.rm.sw.sawlog.industrial.c <- .994
ratio.rm.hw.sawlog.industrial.c <- .832
ratio.rm.sw.pulpwood.industrial.c <- 2.413
ratio.rm.hw.pulpwood.industrial.c <- 1.336

# product fractions (table d6 Smith et al., 2006, the rest is fuelwood or other emissions, WD1); This includes the internally recycled to paper.
#These are fractions prior to 1990; The TPO report indicated that there is very little waste wood and that it is down to 8% as it all goes to paper or nonstructural
#increasing these fractions to have 2- to 8 % waste after 1990.


#PNW westside
#pre-1990
fraction.sw.sawlog.lumber.pnw.w.c <- 0.455  
fraction.sw.sawlog.plywood.pnw.w.c <- 0.089
fraction.old.sw.sawlog.nonstructural.pnw.w.c <- 0.009
fraction.old.sw.sawlog.paper.pnw.w.c <- 0.187
fraction.old.sw.sawlog.emitted.pnw.w.c <- 0.260 
#after 1990
fraction.sw.sawlog.nonstructural.pnw.w.c <- 0.109
fraction.sw.sawlog.paper.pnw.w.c <- 0.287
fraction.sw.sawlog.emitted.pnw.w.c <- 0.060


#pre-1990 (not changing hardwood ratios)
fraction.hw.sawlog.lumber.pnw.w.c <- 0.16  
fraction.hw.sawlog.plywood.pnw.w.c <- 0.14
fraction.old.hw.sawlog.nonstructural.pnw.w.c <- 0.002
fraction.old.hw.sawlog.paper.pnw.w.c <- 0.229
fraction.old.hw.sawlog.emitted.pnw.w.c <- .469
#post-1990
fraction.hw.sawlog.nonstructural.pnw.w.c <- 0.002
fraction.hw.sawlog.paper.pnw.w.c <- 0.229
fraction.hw.sawlog.emitted.pnw.w.c <- .469

#not changing pulpwood ratios; 
fraction.sw.pulpwood.paper.pnw.w.c <- 0.500
fraction.hw.pulpwood.paper.pnw.w.c <- .229
fraction.sw.pulpwood.emitted.pnw.w.c <- .500
fraction.hw.pulpwood.emitted.pnw.w.c <- .469

#PNW eastside
#pre-1990
fraction.sw.sawlog.lumber.pnw.e.c <- 0.422  
fraction.sw.sawlog.plywood.pnw.e.c <- 0.069
fraction.old.sw.sawlog.nonstructural.pnw.e.c <- 0.001
fraction.old.sw.sawlog.paper.pnw.e.c <- 0.145
fraction.old.sw.sawlog.emitted.pnw.e.c <- 0.363
#post-1990
fraction.sw.sawlog.nonstructural.pnw.e.c <- 0.187
fraction.sw.sawlog.paper.pnw.e.c <- 0.260
fraction.sw.sawlog.emitted.pnw.e.c <- 0.062

#pre-1990
fraction.hw.sawlog.lumber.pnw.e.c <- 0.039  
fraction.hw.sawlog.plywood.pnw.e.c <- 0.301
fraction.old.hw.sawlog.nonstructural.pnw.e.c <- 0.015
fraction.old.hw.sawlog.paper.pnw.e.c <- 0.213
fraction.old.hw.sawlog.emitted.pnw.e.c <- 0.432
#post-1990
fraction.hw.sawlog.nonstructural.pnw.e.c <- 0.015
fraction.hw.sawlog.paper.pnw.e.c <- 0.213
fraction.hw.sawlog.emitted.pnw.e.c <- 0.432

#paper stream; TPO report does not address paper stream
fraction.sw.pulpwood.paper.pnw.e.c <- 0.145
fraction.hw.pulpwood.paper.pnw.e.c <- 0.213
fraction.sw.pulpwood.emitted.pnw.e.c <- 0.363
fraction.hw.pulpwood.emitted.pnw.e.c <- 0.432

#pacific southwest (report did not address southwest; assume ~12% losses after 1990)
#pre-1990
fraction.sw.sawlog.lumber.psw.c <- 0.454  
fraction.sw.sawlog.plywood.psw.c <- 0.00
fraction.sw.sawlog.nonstructural.psw.c <- 0.04
fraction.sw.sawlog.paper.psw.c <- 0.181
fraction.sw.sawlog.emitted.psw.c <- 0.325
#post-1990
fraction.sw.sawlog.nonstructural.psw.c <- 0.104
fraction.sw.sawlog.paper.psw.c <- 0.281
fraction.sw.sawlog.emitted.psw.c <- 0.125

#pre-1990
fraction.hw.sawlog.lumber.psw.c <- 0.039  
fraction.hw.sawlog.plywood.psw.c <- 0.301
fraction.hw.sawlog.nonstructural.psw.c <- 0.015
fraction.hw.sawlog.paper.psw.c <- 0.213
fraction.hw.sawlog.emitted.psw.c <- 0.432
#post-1990
fraction.hw.sawlog.nonstructural.psw.c <- 0.015
fraction.hw.sawlog.paper.psw.c <- 0.213
fraction.hw.sawlog.emitted.psw.c <- 0.432

#no change over time
fraction.sw.pulpwood.paper.psw.c <- 0.181
fraction.hw.pulpwood.paper.psw.c <- 0.213
fraction.sw.pulpwood.emitted.psw.c <- 0.325
fraction.hw.pulpwood.emitted.psw.c <- 0.432

#Rocky mountains   (Idaho and Montana had very high utilization rates)         
#pre-1990
fraction.sw.sawlog.lumber.rm.c <- 0.402  
fraction.sw.sawlog.plywood.rm.c <- 0.054
fraction.sw.sawlog.nonstructural.rm.c <- 0.033
fraction.sw.sawlog.paper.rm.c <- 0.215
fraction.sw.sawlog.emitted.rm.c <- 0.296
#post-1990
fraction.sw.sawlog.nonstructural.rm.c <- 0.169
fraction.sw.sawlog.paper.rm.c <- 0.315
fraction.sw.sawlog.emitted.rm.c <- 0.06

#pre-1990
fraction.hw.sawlog.lumber.rm.c <- 0.039  
fraction.hw.sawlog.plywood.rm.c <- 0.301
fraction.hw.sawlog.nonstructural.rm.c <- 0.015
fraction.hw.sawlog.paper.rm.c <- 0.213
fraction.hw.sawlog.emitted.rm.c <- 0.432
#post-1990
fraction.hw.sawlog.nonstructural.rm.c <- 0.015
fraction.hw.sawlog.paper.rm.c <- 0.213
fraction.hw.sawlog.emitted.rm.c <- 0.432

#no change over time; paper
fraction.sw.pulpwood.paper.rm.c <- 0.215
fraction.hw.pulpwood.paper.rm.c <- 0.213
fraction.sw.pulpwood.emitted.rm.c <- 0.296
fraction.hw.pulpwood.emitted.rm.c <- 0.432

# Coefficients for wood products to specific wood products
sw.lumber.Singlefamily.homes <- 0.585
sw.lumber.Multifamily.homes <- 0.031
sw.lumber.Mobile.homes  <- 0.039
sw.lumber.NonResidential.construction <-0.08
sw.lumber.Manufacturing.furniture <- 0.062
sw.lumber.Shipping <- 0.078
sw.lumber.OtherWood.c <- 0.126

hw.lumber.Singlefamily.homes <- 0.78
hw.lumber.Multifamily.homes  <- 0.004
hw.lumber.Mobile.homes <- 0.002
hw.lumber.NonResidential.construction <- 0.083
hw.lumber.Manufacturing.furniture  <- 0.378
hw.lumber.Shipping <- 0.447
hw.lumber.OtherWood.c <- 0.007

sw.plywood.Singlefamily.homes <- 0.577
sw.plywood.Multifamily.homes <- 0.033
sw.plywood.Mobile.homes <- 0.035
sw.plywood.NonResidential.construction <- 0.091
sw.plywood.Manufacturing.furniture <- 0.179
sw.plywood.Shipping <- 0.076
sw.plywood.OtherWood.c <- 0.009

hw.plywood.Singlefamily.homes <- 0.242
hw.plywood.Multifamily.homes <- 0.019
hw.plywood.Mobile.homes  <- 0.037
hw.plywood.NonResidential.construction <- 0.053
hw.plywood.Manufacturing.furniture <- 0.450
hw.plywood.Shipping <- 0.06
hw.plywood.OtherWood.c <- 0.139

sw.nonstructural.Singlefamily.homes <- 0.242
sw.nonstructural.Multifamily.homes <- 0.019
sw.nonstructural.Mobile.homes <- 0.037
sw.nonstructural.NonResidential.construction <- 0.053
sw.nonstructural.Manufacturing.furniture <-0.450
sw.nonstructural.Shipping <- 0.06
sw.nonstructural.OtherWood.c <-0.139

hw.nonstructural.Singlefamily.homes <- 0.242
hw.nonstructural.Multifamily.homes <- 0.019
hw.nonstructural.Mobile.homes <- 0.037
hw.nonstructural.NonResidential.construction <- 0.053
hw.nonstructural.Manufacturing.furniture <-0.450
hw.nonstructural.Shipping <- 0.06
hw.nonstructural.OtherWood.c <-0.139

fraction.sf.short.c <- 0.23
fraction.sf.long.c <- 0.77

fraction.mf.short.c <- 0.23
fraction.mf.long.c <- 0.77

fraction.nr.short.c <- 0.23
fraction.nr.long.c <- 0.77

#Half lives
# half lives are T1/2 = 0.693/lamba where lambda is 'k'
# Table from Skog et al 2000, Smith 2006, Dymond 2012

residue.decay.rate <- .17  #for residue not burned;mean value from Harmon 1996 STANDCARB
slash.burn.rate <- 0.50 #moderate burn rate of all leftover pools; not really changing much; Harmon 1996 STANDCARB

Single.family.k.c <- 0.00924 # 75 year half life
Multi.family.k.c <- 0.00924
Mobile.home.k.c <- 0.0231
NonResidential.k.c <- 0.0198
FurnitureManufactoring.k.c <-0.0231
Shipping.k.c <- 0.1155
OtherWood.k.c <- 0.0231
Paper.k.c <- 0.2772

Single.family.k.c.max <- 0.00693 # 100 year half life
Multi.family.k.c.max <- 0.00693
Mobile.home.k.c.max <- 0.01386
NonResidential.k.c.max <- .0099
FurnitureManufactoring.k.c.max <-0.017325
Shipping.k.c.max <- 0.05775
OtherWood.k.c.max <- 0.03465
Paper.k.c.max <- 0.1155  

Single.family.k.c.min <- 0.01386 # 50 year half life
Multi.family.k.c.min <- 0.01386
Mobile.home.k.c.min <- 0.1386
NonResidential.k.c.min <- 0.03465
FurnitureManufactoring.k.c.min <-0.03465
Shipping.k.c.min <- 0.1155
OtherWood.k.c.min <- 0.017325
Paper.k.c.min <- 0.693  

wood.product.landfill.c <- 0.40 #percent of the wood no longer in use that goes to landfill; average across western regions from table c9
wood.net.recycle.c <- 0.05
wood.product.degradable.landfill.c <- 0.33
wood.product.degradable.landfill.k <- 0.0435

paper.landfill.c <-0.20  # accounts for recycling rate (net value)
paper.net.recycle.c <-0.34 #only for the current year's WD.2
paper.degradable.landfill.c <- 0.66
paper.degradable.landfill.k <-0.0435

# Harmon cohort-component parameters
#A modified Chapman-Richards function is used to model the proportion of stores remaining in a population of building components created the same year:
#PStorecompt= (1-(1-exp[-k.c t])Lag.c
#where PStorecompt are the portional component stores at time t, the time since the component was created,  kcompt is the rate-constant of loss for the component per year; and Lagcomp is a dimensionless parameter that creates a time lag in the time between a components creation and the minimal life span of that component (Figure 1).  Given that it is intuitively easier to envision the average life span of a component than the rate-constant of loss, kcompt can be computed as:
#kcompt = 3/ average component life span.  Thus, an average life span of 25 years would yield a kcompt of 0.12 per year.   
sf.long.lag.c <- 20
mf.long.lag.c <- 20
nr.long.lag.c <- 20

sf.short.lag.c <- 20  #years
mf.short.lag.c <- 20
nr.short.lag.c<- 20

sf.long.k.c.min <- .06 #100, 75, and 50 year turnovers
sf.long.k.c.max <- .03
sf.long.k.c.avg <- .04

mf.long.k.c.min <- .06 #100, 75, and 50 year turnovers
mf.long.k.c.max <- .03
mf.long.k.c.avg <- .04

nr.long.k.c.min <- .12 #75, 50, 25 year average turnovers
nr.long.k.c.max <- .04
nr.long.k.c.avg <- .06

sf.short.k.c <- 0.12 #25 year average turnover
mf.short.k.c <-0.12 
nr.short.k.c <- 0.12

# Product chain FFE coefficients ; These are Tg C emitted per Tg C biomass. Clark et al, 2011
harv.E.c <- 0.0303 # harvest FFE, mult by total.tgc; 
wood.manu.E.c <- 0.0026  # multiply by tgC; 
tran.E.perkm.c = 0.000155 #based on about a 150km roundtrip distance 
#WA reports by sector and all mills/paper products
#average non-biogenic emissions from WA paper mills from 2012 - 2016 (~0.34 Tg C year) DIVIDED (citation pdf below)
# Divided by the amount of pulpwood/woodwaste that is sent to the paper mill (~38% of merchantable wood removed ends up in paper stream )

paper.manu.E.c <- 0.15  #https://ecology.wa.gov/DOE/files/a2/a2207148-0b88-4818-ad12-5f33e22a157c.pdf

# Bioenergy FFE coefficients; calculates using McNamee et al. 2016 
torr.E.c <- .0094 #natural gas used for torrefaction, pellitizing, and drying
wc.drying.E.C <- 0.0104
torr.pelletizing.wc.E.c <- 0.122
wc.pelletizing.E.c<- 0.114

#Boardman Coal plant substitution coefficients
boardman.torr.ffe.e.sub <- 1.00 # combustion emissions displacement for coal only
boardman.chip.ffe.e.sub <- 0.59 #combustion emissions displacement for coal only



# wood product substitution coefficients. Smythe et al 2017
wood.sub.c<- 0.54 #Smythe 2017 (includes comparative extraction emissions)

chp.millresidue.sub.c <- 0.68 # Smythe 2017 (average of constant versus constrained supply)

#Wells to tank (emissions avoided by not using 'coal' or 'natural gas' because of extraction)
energy.WTT.sub.c <- 0.25 #based on average for powder river basin coal (low) and natural gas (higher than coal)

#============================================
# Loop through each year, scen, and calculation
#===========================================

TotWoodProd.singlefamily.avg <- 0.00
TotWoodProd.multifamily.avg <- 0.00
TotWoodProd.mobilehomes.avg <- 0.00
TotWoodProd.nonresidential.avg <- 0.00
TotWoodProd.shipping.avg <- 0.00
TotWoodProd.furniture.manufactoring.avg <- 0.00
TotWoodProd.otherwood.avg <- 0.00

TotWoodProd.singlefamily.min <- 0.00
TotWoodProd.multifamily.min <- 0.00
TotWoodProd.mobilehomes.min <- 0.00
TotWoodProd.nonresidential.min <- 0.00
TotWoodProd.shipping.min <- 0.00
TotWoodProd.furniture.manufactoring.min <- 0.00
TotWoodProd.otherwood.min <- 0.00

TotWoodProd.singlefamily.max <- 0.00
TotWoodProd.multifamily.max <- 0.00
TotWoodProd.mobilehomes.max <- 0.00
TotWoodProd.nonresidential.max <- 0.00
TotWoodProd.shipping.max <- 0.00
TotWoodProd.furniture.manufactoring.max <- 0.00
TotWoodProd.otherwood.max <- 0.00

TotWoodProd.wood.avg <- 0.00
TotWoodProd.wood.min <- 0.00
TotWoodProd.wood.max <- 0.00

TotWoodProd.wood.ccm.max <- 0.00
TotWoodProd.wood.ccm.min <- 0.00
TotWoodProd.wood.ccm.avg <- 0.00

TotWoodProd.paper.avg <- 0.00
TotWoodProd.paper.min <- 0.00
TotWoodProd.paper.max <- 0.00

TotWoodProd.avg <- 0.00
TotWoodProd.min<- 0.00
TotWoodProd.max<-0.00

TotWoodProd.ccm.avg <- 0.00
TotWoodProd.ccm.min <- 0.00
TotWoodProd.ccm.max <- 0.00

Residues.forest.total <- 0.00
Residues.forest.decay.emitted <- 0.00
Redidues.forest.combust.emitted <- 0.00
Residues.TgCyr <- 0.00

landfill.permanent.wood.avg <- 0.00
landfill.permanent.wood.ccm.avg <- 0.00
landfill.permanent.wood.min <- 0.00
landfill.permanent.wood.ccm.min <- 0.00
landfill.permanent.wood.max <- 0.00
landfill.permanent.wood.ccm.max<- 0.00

landfill.degradable.wood.avg <- 0.00
landfill.degradable.wood.ccm.avg <- 0.00
landfill.degradable.wood.min <- 0.00
landfill.degradable.wood.ccm.min <- 0.00
landfill.degradable.wood.max <- 0.00
landfill.degradable.wood.ccm.max <- 0.00

landfill.permanent.paper.avg <- 0.00
landfill.permanent.paper.min <- 0.00
landfill.permanent.paper.max <- 0.00
landfill.degradable.paper.avg <- 0.00
landfill.degradable.paper.min <- 0.00
landfill.degradable.paper.max <- 0.00


for (x in 1:num.rows){ # OUTPUT notes; can treat each row individually as long as in date order. the total wood products TARA
                       #  pool will be a running total for the state. WD1 will be row specific. WD2 will be for the whole pool each year for the state
      #set each amount to 0 for each new roW (in case there is no harvest and nothing happens)
  
       Merch.TgCyr.sw <- 0.00
       Merch.TgCyr.hw <- 0.00
       
       Residues.TgCyr <- 0.00
       Residues.forest.decay.emitted <- 0.00
       Residues.forest.combust.emitted <- 0.00
      
       fraction.sawlog.sw <- 0.00
       fraction.sawlog.hw <- 0.00
       fraction.pulpwood.sw <- 0.00
       fraction.pulpwood.hw <- 0.00
       
       #Industrial fractions
       fraction.sawlog.sw.product <- 0.00
       fraction.sawlog.hw.product <- 0.00
       fraction.pulpwood.sw.product <- 0.00     
       fraction.pulpwood.hw.product <- 0.00
       
       #Split into product streams
       sw.fraction.lumber.product <- 0.00
       hw.fraction.lumber.product <- 0.00
       sw.fraction.plywood.product <- 0.00
       hw.fraction.plywood.product <- 0.00
       sw.fraction.nonstructural.product <- 0.00
       hw.fraction.nonstructural.product <- 0.00
       fraction.wood.product <- 0.00
       fraction.paper.product <- 0.00
       
       #Split into end uses
       fraction.single.family.homes <- 0.00
       fraction.sf.short <- 0.00
       fraction.sf.long <- 0.00
       fraction.multi.family.homes <- 0.00
       fraction.mobile.homes <-0.00
       fraction.furniture.manufactoring <- 0.00
       fraction.shipping <-0.00
       fraction.other.wood <- 0.00
       fraction.nonresidential <- 0.00
       
       WD.1.avg<-0.00
       WD.1.min<-0.00
       WD.1.max<-0.00
       
       WD.2.wood.singlefamily.avg <- 0.00
       WD.2.wood.sf.short.avg <- 0.00
       WD.2.wood.sf.long.avg <- 0.00
       
       WD.2.wood.multifamily.avg <- 0.00
       WD.2.wood.mf.short.avg <- 0.00
       WD.2.wood.mf.long.avg <- 0.00
       
       WD.2.wood.mobilehomes.avg <- 0.00
       
       WD.2.wood.nonresidential.avg <- 0.00
       WD.2.wood.nr.short.avg <- 0.00
       WD.2.wood.nr.long.avg <- 0.00
       
       WD.2.wood.furniture.manufactoring.avg <- 0.00
       WD.2.wood.shipping.avg <- 0.00
       WD.2.wood.otherwood.avg <-0.00
       WD.2.avg<-0.00
       WD.2.ccm.avg <- 0.00
       
       WD.2.wood.singlefamily.min <- 0.00
       WD.2.wood.sf.short.min <- 0.00
       WD.2.wood.sf.long.min <- 0.00
       
       WD.2.wood.multifamily.min <- 0.00
       WD.2.wood.mf.short.min <- 0.00
       WD.2.wood.mf.long.min <- 0.00
       
       WD.2.wood.mobilehomes.min <- 0.00
       
       WD.2.wood.nonresidential.min <- 0.00
       WD.2.wood.nr.short.min <- 0.00
       WD.2.wood.nr.long.min <- 0.00
       
       WD.2.wood.furniture.manufactoring.min <- 0.00
       WD.2.wood.shipping.min <- 0.00
       WD.2.wood.otherwood.min <-0.00
       WD.2.min<-0.00
       WD.2.ccm.min <- 0.00
       
       WD.2.wood.singlefamily.max <- 0.00
       WD.2.wood.sf.short.max <- 0.00
       WD.2.wood.sf.long.max <- 0.00
       
       WD.2.wood.multifamily.max <- 0.00
       WD.2.wood.mf.short.max <- 0.00
       WD.2.wood.mf.long.max <- 0.00
       
       WD.2.wood.mobilehomes.max <- 0.00
       
       WD.2.wood.nonresidential.max <- 0.00
       WD.2.wood.nr.short.max <- 0.00
       WD.2.wood.nr.long.max <- 0.00
       
       WD.2.wood.furniture.manufactoring.max <- 0.00
       WD.2.wood.shipping.max <- 0.00
       WD.2.wood.otherwood.max <-0.00
       WD.2.max<-0.00
       WD.2.ccm.max <- 0.00
       
       WD.2.paper.avg<- 0.00
       WD.2.paper.max<- 0.00
       WD.2.paper.min<-0.00
       
       WD.2.wood.avg<- 0.00
       WD.2.wood.ccm.avg <- 0.00
       
       WD.2.wood.max<- 0.00
       WD.2.wood.ccm.max<- 0.00
       
       WD.2.wood.min<-0.00
       WD.2.wood.ccm.min <- 0.00
       
       paper.net.recycle.avg <- 0.00
       paper.net.recycle.max <- 0.00
       paper.net.recycle.min <- 0.00
       
       wood.net.recycle.singlefamily.avg <- 0.00
       wood.net.recycle.multifamily.avg <- 0.00
       wood.net.recycle.mobilehomes.avg <- 0.00
       wood.net.recycle.nonresidential.avg <- 0.00
       wood.net.recycle.furniture.manufactoring.avg <- 0.00
       wood.net.recycle.shipping.avg <- 0.00
       wood.net.recycle.otherwood.avg <- 0.00
       wood.net.recycle.avg <- 0.00
       
       wood.net.recycle.singlefamily.min <- 0.00
       wood.net.recycle.multifamily.min <- 0.00
       wood.net.recycle.mobilehomes.min <- 0.00
       wood.net.recycle.nonresidential.min <- 0.00
       wood.net.recycle.furniture.manufactoring.min <- 0.00
       wood.net.recycle.shipping.min <- 0.00
       wood.net.recycle.otherwood.min <- 0.00
       wood.net.recycle.min <- 0.00
         
       wood.net.recycle.singlefamily.max <- 0.00
       wood.net.recycle.multifamily.max <- 0.00
       wood.net.recycle.mobilehomes.max <- 0.00
       wood.net.recycle.nonresidential.max <- 0.00
       wood.net.recycle.furniture.manufactoring.max <- 0.00
       wood.net.recycle.shipping.max <- 0.00
       wood.net.recycle.otherwood.max <- 0.00
       wood.net.recycle.max <- 0.00
       
       # New product (annual)
       TotWoodProd.singlefamily.new <- 0.00
       TotWoodProd.sf.short.new <- 0.00
       TotWoodProd.sf.long.new <- 0.00
       
       TotWoodProd.multifamily.new <- 0.00
       TotWoodProd.mf.short.new <- 0.00
       TotWoodProd.mf.long.new <- 0.00
       
       TotWoodProd.mobilehomes.new <- 0.00
       
       TotWoodProd.nonresidential.new <- 0.00
       TotWoodProd.nr.short.new <- 0.00
       TotWoodProd.nr.long.new <- 0.00
       
       TotWoodProd.shipping.new <- 0.00
       TotWoodProd.furniture.manufactoring.new <- 0.00
       TotWoodProd.otherwood.new <- 0.00
       
       TotWoodProd.new <-0.00
       
       TotWoodProd.sf.short.avg <- 0.00
       TotWoodProd.sf.short.min <- 0.00
       TotWoodProd.sf.short.max <- 0.00
       TotWoodProd.sf.long.avg <- 0.00
       TotWoodProd.sf.long.min <- 0.00
       TotWoodProd.sf.long.max <- 0.00
       
       TotWoodProd.sf.short.avg.all <- 0.00
       TotWoodProd.sf.long.avg.all <- 0.00
       TotWoodProd.sf.short.min.all <- 0.00
       TotWoodProd.sf.long.min.all <- 0.00
       TotWoodProd.sf.short.max.all <- 0.00
       TotWoodProd.sf.long.max.all <- 0.00
       
       TotWoodProd.mf.short.avg <- 0.00
       TotWoodProd.mf.short.min <- 0.00
       TotWoodProd.mf.short.max <- 0.00
       TotWoodProd.mf.long.avg <- 0.00
       TotWoodProd.mf.long.min <- 0.00
       TotWoodProd.mf.long.max <- 0.00
       
       TotWoodProd.mf.short.avg.all <- 0.00
       TotWoodProd.mf.long.avg.all <- 0.00
       TotWoodProd.mf.short.min.all <- 0.00
       TotWoodProd.mf.long.min.all <- 0.00
       TotWoodProd.mf.short.max.all <- 0.00
       TotWoodProd.mf.long.max.all <- 0.00
       
       TotWoodProd.nr.short.avg <- 0.00
       TotWoodProd.nr.short.min <- 0.00
       TotWoodProd.nr.short.max <- 0.00
       TotWoodProd.nr.long.avg <- 0.00
       TotWoodProd.nr.long.min <- 0.00
       TotWoodProd.nr.long.max <- 0.00
       
       TotWoodProd.nr.short.avg.all <- 0.00
       TotWoodProd.nr.long.avg.all <- 0.00
       TotWoodProd.nr.short.min.all <- 0.00
       TotWoodProd.nr.long.min.all <- 0.00
       TotWoodProd.nr.short.max.all <- 0.00
       TotWoodProd.nr.long.max.all <- 0.00
       
       
       TotWoodProd.wood.new <-0.00
       TotWoodProd.paper.new <- 0.00
       
       landfill.wood.avg.new <- 0.00
       landfill.wood.ccm.avg.new <- 0.00
       landfill.wood.min.new <- 0.00
       landfill.wood.ccm.min.new <- 0.00
       landfill.wood.max.new <- 0.00
       landfill.wood.ccm.max.new <- 0.00
       
       landfill.paper.avg.new <- 0.00
       landfill.paper.min.new <- 0.00
       landfill.paper.max.new <- 0.00
       
       landfill.wood.emissions.avg <-0.00
       landfill.wood.emissions.ccm.avg <- 0.00
       landfill.wood.emissions.min <- 0.00
       landfill.wood.emissions.ccm.min <-0.00
       landfill.wood.emissions.max <- 0.00
       landfill.wood.emissions.ccm.max <- 0.00
       
       landfill.paper.emissions.avg <- 0.00
       landfill.paper.emissions.min <- 0.00
       landfill.paper.emissions.max <- 0.00
       
       harv.E <- 0.00
       harv.tran.E <- 0.00
       wood.manu.E <-0.00
       paper.manu.E <-0.00
       prod.tran.E <- 0.00
       
       TotTransEmis.TgCyr <-0.00
       TotManuEmis.TgCyr <- 0.00
       TotProdEmis.TgCyr <- 0.00
       Tot.TorrEmis.TgCyr <- 0.00
       Tot.WCEmis.TgCyr <- 0.00
          
       wood.sub.25 <- 0.00
       wood.sub.50 <-0.00
       wood.sub.75 <-0.00
       wood.sub.100 <- 0.00
       
       mill.residues.energy.sub.50 <-0.00
       mill.residues.energy.sub.75 <-0.00
       
       boardman.wc.sub <- 0.00
       boardman.torr.sub<-0.00
      
       #boardman stuff  TARA
       torr.E.sub <- 0.00
       torr.FFE.WTT.sub <- 0.00
       wc.E.sub <- 0.00
       wc.FFE.WTT.sub <- 0.00
       Torr.Total.manu.E <- 0.00
       Torr.trans.E <-0.00
       Torr.combust.E <-0.00
       WC.Total.manu.E <- 0.00
       WC.trans.E <-0.00
       WC.combust.E <-0.00
       
      
       TotABC.TgCyr <- 0.00
       this.year.short <- 0.00
       this.year.long <- 0.00
       
      ID <- LCA.out[x,"ID"]
      ID <- as.character(ID)
      State <- LCA.out[x,"State"]
      Region <- LCA.out[x,"Region"]
      Region <- as.character(Region)
      year <- LCA.out[x,"Year"]
      distance.to.mill <- as.double(LCA.out[x,"Distance.to.mill"])
      distance.to.use <- as.double(LCA.out[x,"Distance.to.use"])
      TotABC.TgCyr <- as.double(LCA.out[x,"TotABC.TgCyr"])
      Merch.TgCyr <- as.double(LCA.out[x,"Merch.TgCyr"])
      
        if (Region == 'pnw.w'|| Region == 'pnw') {
        
        #split into sw and hw fractions
        Merch.TgCyr.sw <- Merch.TgCyr*fraction.pnw.w.sw.c   
        Merch.TgCyr.hw <- Merch.TgCyr*fraction.pnw.w.hw.c
       
        # Residue fractions (slash and growing stock not taken to mill)
        Residues.TgCyr<- TotABC.TgCyr - Merch.TgCyr
        
        #split into sawlog and pulpwood
        
        fraction.sawlog.sw <- fraction.pnw.w.sw.sawlog.c * Merch.TgCyr.sw
        fraction.sawlog.hw <- fraction.pnw.w.hw.sawlog.c * Merch.TgCyr.hw
        fraction.pulpwood.sw <- fraction.pnw.w.sw.pulpwood.c * Merch.TgCyr.sw
        fraction.pulpwood.hw <- fraction.pnw.w.hw.pulpwood.c * Merch.TgCyr.hw
        
        #WD1 losses due to industrial ratios (these are very small)
        fraction.sawlog.sw.product <- ratio.pnw.sw.sawlog.industrial.c * fraction.sawlog.sw
        fraction.sawlog.hw.product <- ratio.pnw.hw.sawlog.industrial.c * fraction.sawlog.hw
        fraction.pulpwood.sw.product <- ratio.pnw.sw.pulpwood.industrial.c * fraction.pulpwood.sw     
        fraction.pulpwood.hw.product <- ratio.pnw.hw.pulpwood.industrial.c * fraction.pulpwood.hw
        
        #split into product streams
        fraction.sw.lumber.product <- fraction.sawlog.sw.product*fraction.sw.sawlog.lumber.pnw.w.c 
        fraction.hw.lumber.product <- fraction.sawlog.hw.product*fraction.hw.sawlog.lumber.pnw.w.c
        fraction.sw.plywood.product <- fraction.sawlog.sw.product*fraction.sw.sawlog.plywood.pnw.w.c  
        fraction.hw.plywood.product <- fraction.sawlog.hw.product *fraction.hw.sawlog.plywood.pnw.w.c
        fraction.sw.nonstructural.product <- fraction.sawlog.sw.product*fraction.sw.sawlog.nonstructural.pnw.w.c 
        fraction.hw.nonstructural.product <- fraction.sawlog.hw.product *fraction.hw.sawlog.nonstructural.pnw.w.c
        
        fraction.paper.product <- fraction.sawlog.sw.product*fraction.sw.sawlog.paper.pnw.w.c + fraction.sawlog.hw.product*fraction.hw.sawlog.paper.pnw.w.c +fraction.pulpwood.sw.product*fraction.sw.pulpwood.paper.pnw.w.c + fraction.pulpwood.hw.product*fraction.hw.pulpwood.paper.pnw.w.c
        
        } # End pnw.w 
      
      if (Region == 'pnw.e') {
        
        #split into sw and hw fractions
        Merch.TgCyr.sw <- Merch.TgCyr*fraction.pnw.w.sw.c 
        Merch.TgCyr.hw <- Merch.TgCyr*fraction.pnw.w.hw.c
        
        
        # Residue fractions (slash and growing stock not taken to mill)
        Residues.TgCyr<- TotABC.TgCyr - Merch.TgCyr
        
        #split into sawlog and pulpwood
        
        fraction.sawlog.sw <- fraction.pnw.e.sw.sawlog.c * Merch.TgCyr.sw
        fraction.sawlog.hw <- fraction.pnw.e.hw.sawlog.c * Merch.TgCyr.hw
        fraction.pulpwood.sw <- fraction.pnw.e.sw.pulpwood.c * Merch.TgCyr.sw
        fraction.pulpwood.hw <- fraction.pnw.e.hw.pulpwood.c * Merch.TgCyr.hw
        
        #WD1 losses due to industrial ratios
        fraction.sawlog.sw.product <- ratio.pnw.sw.sawlog.industrial.c * fraction.sawlog.sw
        fraction.sawlog.hw.product <- ratio.pnw.hw.sawlog.industrial.c * fraction.sawlog.hw
        fraction.pulpwood.sw.product <- ratio.pnw.sw.pulpwood.industrial.c * fraction.pulpwood.sw     
        fraction.pulpwood.hw.product <- ratio.pnw.hw.pulpwood.industrial.c * fraction.pulpwood.hw
        
        #split into product streams
        fraction.sw.lumber.product <- fraction.sawlog.sw.product*fraction.sw.sawlog.lumber.pnw.e.c 
        fraction.hw.lumber.product <- fraction.sawlog.hw.product*fraction.hw.sawlog.lumber.pnw.e.c
        fraction.sw.plywood.product <- fraction.sawlog.sw.product*fraction.sw.sawlog.plywood.pnw.e.c  
        fraction.hw.plywood.product <- fraction.sawlog.hw.product *fraction.hw.sawlog.plywood.pnw.e.c
        fraction.sw.nonstructural.product <- fraction.sawlog.sw.product*fraction.sw.sawlog.nonstructural.pnw.e.c 
        fraction.hw.nonstructural.product <- fraction.sawlog.hw.product *fraction.hw.sawlog.nonstructural.pnw.e.c
        
        fraction.paper.product <- fraction.sawlog.sw.product*fraction.sw.sawlog.paper.pnw.e.c + fraction.sawlog.hw.product*fraction.hw.sawlog.paper.pnw.e.c +fraction.pulpwood.sw.product*fraction.sw.pulpwood.paper.pnw.e.c + fraction.pulpwood.hw.product*fraction.hw.pulpwood.paper.pnw.e.c
        
      } # End pnw.e 
      
      if (Region == 'psw') {
        
        #split into sw and hw fractions
        Merch.TgCyr.sw <- Merch.TgCyr*fraction.pnw.w.sw.c 
        Merch.TgCyr.hw <- Merch.TgCyr*fraction.pnw.w.hw.c
        
        # Residue fractions (slash and growing stock not taken to mill)
        Residues.TgCyr<- TotABC.TgCyr - Merch.TgCyr
        
        #split into sawlog and pulpwood
        
        fraction.sawlog.sw <- fraction.psw.sw.sawlog.c * Merch.TgCyr.sw
        fraction.sawlog.hw <- fraction.psw.hw.sawlog.c * Merch.TgCyr.hw
        fraction.pulpwood.sw <- fraction.psw.sw.pulpwood.c * Merch.TgCyr.sw
        fraction.pulpwood.hw <- fraction.psw.hw.pulpwood.c * Merch.TgCyr.hw
        
        #WD1 losses due to industrial ratios
        fraction.sawlog.sw.product <- ratio.psw.sw.sawlog.industrial.c * fraction.sawlog.sw
        fraction.sawlog.hw.product <- ratio.psw.hw.sawlog.industrial.c * fraction.sawlog.hw
        fraction.pulpwood.sw.product <- ratio.psw.sw.pulpwood.industrial.c * fraction.pulpwood.sw     
        fraction.pulpwood.hw.product <- ratio.psw.hw.pulpwood.industrial.c * fraction.pulpwood.hw
        
        #split into product streams
        fraction.sw.lumber.product <- fraction.sawlog.sw.product*fraction.sw.sawlog.lumber.psw.c 
        fraction.hw.lumber.product <- fraction.sawlog.hw.product*fraction.hw.sawlog.lumber.psw.c
        fraction.sw.plywood.product <- fraction.sawlog.sw.product*fraction.sw.sawlog.plywood.psw.c  
        fraction.hw.plywood.product <- fraction.sawlog.hw.product *fraction.hw.sawlog.plywood.psw.c
        fraction.sw.nonstructural.product <- fraction.sawlog.sw.product*fraction.sw.sawlog.nonstructural.psw.c 
        fraction.hw.nonstructural.product <- fraction.sawlog.hw.product *fraction.hw.sawlog.nonstructural.psw.c
        
        fraction.paper.product <- fraction.sawlog.sw.product*fraction.sw.sawlog.paper.psw.c + fraction.sawlog.hw.product*fraction.hw.sawlog.paper.psw.c +fraction.pulpwood.sw.product*fraction.sw.pulpwood.paper.psw.c + fraction.pulpwood.hw.product*fraction.hw.pulpwood.paper.psw.c
        
      } # End psw
      
      if (Region == 'rm.n') {
        
        #split into sw and hw fractions
        Merch.TgCyr.sw <- Merch.TgCyr*fraction.pnw.w.sw.c 
        Merch.TgCyr.hw <- Merch.TgCyr*fraction.pnw.w.hw.c
       
        # Residue fractions (slash and growing stock not taken to mill)
        Residues.TgCyr<- TotABC.TgCyr - Merch.TgCyr
        
        #split into sawlog and pulpwood
        
        fraction.sawlog.sw <- fraction.rm.n.sw.sawlog.c * Merch.TgCyr.sw
        fraction.sawlog.hw <- fraction.rm.n.hw.sawlog.c * Merch.TgCyr.hw
        fraction.pulpwood.sw <- fraction.rm.n.sw.pulpwood.c * Merch.TgCyr.sw
        fraction.pulpwood.hw <- fraction.rm.n.hw.pulpwood.c * Merch.TgCyr.hw
        
        #WD1 losses due to industrial ratios
        fraction.sawlog.sw.product <- ratio.rm.sw.sawlog.industrial.c * fraction.sawlog.sw
        fraction.sawlog.hw.product <- ratio.rm.hw.sawlog.industrial.c * fraction.sawlog.hw
        fraction.pulpwood.sw.product <- ratio.rm.sw.pulpwood.industrial.c * fraction.pulpwood.sw     
        fraction.pulpwood.hw.product <- ratio.rm.hw.pulpwood.industrial.c * fraction.pulpwood.hw
        
        #split into product streams
        fraction.sw.lumber.product <- fraction.sawlog.sw.product*fraction.sw.sawlog.lumber.rm.w.c 
        fraction.hw.lumber.product <- fraction.sawlog.hw.product*fraction.hw.sawlog.lumber.rm.w.c
        fraction.sw.plywood.product <- fraction.sawlog.sw.product*fraction.sw.sawlog.plywood.rm.w.c  
        fraction.hw.plywood.product <- fraction.sawlog.hw.product *fraction.hw.sawlog.plywood.rm.w.c
        fraction.sw.nonstructural.product <- fraction.sawlog.sw.product*fraction.sw.sawlog.nonstructural.rm.w.c 
        fraction.hw.nonstructural.product <- fraction.sawlog.hw.product *fraction.hw.sawlog.nonstructural.rm.w.c
        
        fraction.paper.product <- fraction.sawlog.sw.product*fraction.sw.sawlog.paper.rm.w.c + fraction.sawlog.hw.product*fraction.hw.sawlog.paper.rm.w.c +fraction.pulpwood.sw.product*fraction.sw.pulpwood.paper.rm.w.c + fraction.pulpwood.hw.product*fraction.hw.pulpwood.paper.rm.w.c
        
      } # End rm.n 
      
      if (Region == 'rm.s') {
        
        #split into sw and hw fractions
        Merch.TgCyr.sw <- Merch.TgCyr*fraction.pnw.w.sw.c 
        Merch.TgCyr.hw <- Merch.TgCyr*fraction.pnw.w.hw.c
        
        # Residue fractions (slash and growing stock not taken to mill)
        Residues.TgCyr<- TotABC.TgCyr - Merch.TgCyr
        
        #split into sawlog and pulpwood
        
        fraction.sawlog.sw <- fraction.rm.s.sw.sawlog.c * Merch.TgCyr.sw
        fraction.sawlog.hw <- fraction.rm.s.hw.sawlog.c * Merch.TgCyr.hw
        fraction.pulpwood.sw <- fraction.rm.s.sw.pulpwood.c * Merch.TgCyr.sw
        fraction.pulpwood.hw <- fraction.rm.s.hw.pulpwood.c * Merch.TgCyr.hw
        
        #WD1 losses due to industrial ratios
        fraction.sawlog.sw.product <- ratio.rm.sw.sawlog.industrial.c * fraction.sawlog.sw
        fraction.sawlog.hw.product <- ratio.rm.hw.sawlog.industrial.c * fraction.sawlog.hw
        fraction.pulpwood.sw.product <- ratio.rm.sw.pulpwood.industrial.c * fraction.pulpwood.sw     
        fraction.pulpwood.hw.product <- ratio.rm.hw.pulpwood.industrial.c * fraction.pulpwood.hw
        
        #split into product streams
        fraction.sw.lumber.product <- fraction.sawlog.sw.product*fraction.sw.sawlog.lumber.rm.w.c 
        fraction.hw.lumber.product <- fraction.sawlog.hw.product*fraction.hw.sawlog.lumber.rm.w.c
        fraction.sw.plywood.product <- fraction.sawlog.sw.product*fraction.sw.sawlog.plywood.rm.w.c  
        fraction.hw.plywood.product <- fraction.sawlog.hw.product *fraction.hw.sawlog.plywood.rm.w.c
        fraction.sw.nonstructural.product <- fraction.sawlog.sw.product*fraction.sw.sawlog.nonstructural.rm.w.c 
        fraction.hw.nonstructural.product <- fraction.sawlog.hw.product *fraction.hw.sawlog.nonstructural.rm.w.c
        
        fraction.paper.product <- fraction.sawlog.sw.product*fraction.sw.sawlog.paper.rm.w.c + fraction.sawlog.hw.product*fraction.hw.sawlog.paper.rm.w.c +fraction.pulpwood.sw.product*fraction.sw.pulpwood.paper.rm.w.c + fraction.pulpwood.hw.product*fraction.hw.pulpwood.paper.rm.w.c
        
      } # End rm.s
      
      
      #split into end uses
      fraction.single.family.homes <- (fraction.sw.lumber.product * sw.lumber.Singlefamily.homes)+(fraction.hw.lumber.product*hw.lumber.Singlefamily.homes) + (fraction.sw.plywood.product*sw.plywood.Singlefamily.homes)+(fraction.hw.plywood.product*hw.plywood.Singlefamily.homes)+(fraction.sw.nonstructural.product*sw.nonstructural.Singlefamily.homes) + (fraction.hw.nonstructural.product*hw.nonstructural.Singlefamily.homes)
      fraction.sf.short <- fraction.single.family.homes * fraction.sf.short.c
      fraction.sf.long <-  fraction.single.family.homes * fraction.sf.long.c
      
      fraction.multi.family.homes <- (fraction.sw.lumber.product * sw.lumber.Multifamily.homes)+(fraction.hw.lumber.product*hw.lumber.Multifamily.homes) + (fraction.sw.plywood.product*sw.plywood.Multifamily.homes)+(fraction.hw.plywood.product*hw.plywood.Multifamily.homes)+(fraction.sw.nonstructural.product*sw.nonstructural.Multifamily.homes) + (fraction.hw.nonstructural.product*hw.nonstructural.Multifamily.homes)
      fraction.mf.short <- fraction.multi.family.homes * fraction.mf.short.c
      fraction.mf.long <-  fraction.multi.family.homes * fraction.mf.long.c
      
      fraction.mobile.homes <- (fraction.sw.lumber.product * sw.lumber.Mobile.homes)+(fraction.hw.lumber.product*hw.lumber.Mobile.homes) + (fraction.sw.plywood.product*sw.plywood.Mobile.homes)+(fraction.hw.plywood.product*hw.plywood.Mobile.homes)+(fraction.sw.nonstructural.product*sw.nonstructural.Mobile.homes) + (fraction.hw.nonstructural.product*hw.nonstructural.Mobile.homes)
      
      fraction.nonresidential <- (fraction.sw.lumber.product * sw.lumber.NonResidential.construction)+(fraction.hw.lumber.product*hw.lumber.NonResidential.construction) + (fraction.sw.plywood.product*sw.plywood.NonResidential.construction)+(fraction.hw.plywood.product*hw.plywood.NonResidential.construction)+(fraction.sw.nonstructural.product*sw.nonstructural.NonResidential.construction) + (fraction.hw.nonstructural.product*hw.nonstructural.NonResidential.construction)
      fraction.nr.short <- fraction.nonresidential * fraction.nr.short.c
      fraction.nr.long <-  fraction.nonresidential * fraction.nr.long.c
      
      fraction.shipping <- (fraction.sw.lumber.product * sw.lumber.Shipping)+(fraction.hw.lumber.product*hw.lumber.Shipping) + (fraction.sw.plywood.product*sw.plywood.Shipping)+(fraction.hw.plywood.product*hw.plywood.Shipping)+(fraction.sw.nonstructural.product*sw.nonstructural.Shipping) + (fraction.hw.nonstructural.product*hw.nonstructural.Shipping)
      fraction.furniture.manufactoring <- (fraction.sw.lumber.product * sw.lumber.Manufacturing.furniture)+(fraction.hw.lumber.product*hw.lumber.Manufacturing.furniture) + (fraction.sw.plywood.product*sw.plywood.Manufacturing.furniture)+(fraction.hw.plywood.product*hw.plywood.Manufacturing.furniture)+(fraction.sw.nonstructural.product*sw.nonstructural.Manufacturing.furniture) + (fraction.hw.nonstructural.product*hw.nonstructural.Manufacturing.furniture)
      fraction.other.wood <- (fraction.sw.lumber.product * sw.lumber.OtherWood.c)+(fraction.hw.lumber.product*hw.lumber.OtherWood.c) + (fraction.sw.plywood.product*sw.plywood.OtherWood.c)+(fraction.hw.plywood.product*hw.plywood.OtherWood.c)+(fraction.sw.nonstructural.product*sw.nonstructural.OtherWood.c) + (fraction.hw.nonstructural.product*hw.nonstructural.OtherWood.c)
        
      fraction.wood.product <- fraction.single.family.homes+fraction.multi.family.homes+fraction.mobile.homes+fraction.furniture.manufactoring+fraction.shipping+fraction.nonresidential+fraction.other.wood
        
      #WD.1 (Total annual losses at mill; some burned for energy)
      WD.1 <- (Merch.TgCyr.hw+Merch.TgCyr.sw) - (fraction.wood.product + fraction.paper.product)
      
      # set this years inputs
      TotWoodProd.singlefamily.new <- fraction.single.family.homes
      TotWoodProd.sf.short.new <- fraction.sf.short
      TotWoodProd.sf.long.new <- fraction.sf.long
      
      TotWoodProd.multifamily.new <- fraction.multi.family.homes
      TotWoodProd.mf.short.new <- fraction.mf.short
      TotWoodProd.mf.long.new <- fraction.mf.long
      
      TotWoodProd.mobilehomes.new <- fraction.mobile.homes
        
      TotWoodProd.nonresidential.new <- fraction.nonresidential
      TotWoodProd.nr.short.new <- fraction.nr.short
      TotWoodProd.nr.long.new <- fraction.nr.long
      
      TotWoodProd.shipping.new <- fraction.shipping
      TotWoodProd.furniture.manufactoring.new <- fraction.furniture.manufactoring
      TotWoodProd.otherwood.new <- fraction.other.wood
      
      TotWoodProd.paper.new <- fraction.paper.product
      TotWoodProd.wood.new <- fraction.wood.product
      
      #total new inputs, plus variables for half life sensitivies
      TotWoodProd.new<- TotWoodProd.wood.new + TotWoodProd.paper.new
     
      #BAU Residue Decay and combustion. New residues (inputs) each year is Residues.Tgcyr
      #Assuming 50% of slash is burned
      
      #Residue Fractions
      Residues.forest.decay.emitted <- (Residues.forest.total*residue.decay.rate)
      Residues.forest.total <- Residues.forest.total- Residues.forest.decay.emitted +Residues.TgCyr*slash.burn.rate  #assume 50% burned onsite after 1980
      Residues.forest.combust.emitted <- Residues.TgCyr*slash.burn.rate

     # Loops for building cohorts
      
      this.year.cohort <- NULL
      number.rows <- last.year+1 - year
      newname <- paste0("this.year.cohort",".",year)
      for (i in 1:number.rows) {
        TotWoodProd.sf.short.avg <- round(TotWoodProd.sf.short.new*(1-(1-exp(-sf.short.k.c*i))^sf.short.lag.c),digits = 6)
        TotWoodProd.sf.short.min <- round(TotWoodProd.sf.short.new*(1-(1-exp(-sf.short.k.c*i))^sf.short.lag.c),digits = 6)
        TotWoodProd.sf.short.max <- round(TotWoodProd.sf.short.new*(1-(1-exp(-sf.short.k.c*i))^sf.short.lag.c),digits = 6)
        TotWoodProd.sf.long.avg <-  round(TotWoodProd.sf.long.new*(1-(1-exp(-sf.long.k.c.avg*i))^sf.long.lag.c), digits = 6)
        TotWoodProd.sf.long.min <-  round(TotWoodProd.sf.long.new*(1-(1-exp(-sf.long.k.c.min*i))^sf.long.lag.c), digits = 6)
        TotWoodProd.sf.long.max <-  round(TotWoodProd.sf.long.new*(1-(1-exp(-sf.long.k.c.max*i))^sf.long.lag.c), digits = 6)
        
        TotWoodProd.mf.short.avg <- round(TotWoodProd.mf.short.new*(1-(1-exp(-mf.short.k.c*i))^mf.short.lag.c),digits = 6)
        TotWoodProd.mf.short.min <- round(TotWoodProd.mf.short.new*(1-(1-exp(-mf.short.k.c*i))^mf.short.lag.c),digits = 6)
        TotWoodProd.mf.short.max <- round(TotWoodProd.mf.short.new*(1-(1-exp(-mf.short.k.c*i))^mf.short.lag.c),digits = 6)
        TotWoodProd.mf.long.avg <-  round(TotWoodProd.mf.long.new*(1-(1-exp(-mf.long.k.c.avg*i))^mf.long.lag.c), digits = 6)
        TotWoodProd.mf.long.min <-  round(TotWoodProd.mf.long.new*(1-(1-exp(-mf.long.k.c.min*i))^mf.long.lag.c), digits = 6)
        TotWoodProd.mf.long.max <-  round(TotWoodProd.mf.long.new*(1-(1-exp(-mf.long.k.c.max*i))^mf.long.lag.c), digits = 6)
        
        TotWoodProd.nr.short.avg <- round(TotWoodProd.nr.short.new*(1-(1-exp(-nr.short.k.c*i))^nr.short.lag.c),digits = 6)
        TotWoodProd.nr.short.min <- round(TotWoodProd.nr.short.new*(1-(1-exp(-nr.short.k.c*i))^nr.short.lag.c),digits = 6)
        TotWoodProd.nr.short.max <- round(TotWoodProd.nr.short.new*(1-(1-exp(-nr.short.k.c*i))^nr.short.lag.c),digits = 6)
        TotWoodProd.nr.long.avg <-  round(TotWoodProd.nr.long.new*(1-(1-exp(-nr.long.k.c.avg*i))^nr.long.lag.c), digits = 6)
        TotWoodProd.nr.long.min <-  round(TotWoodProd.nr.long.new*(1-(1-exp(-nr.long.k.c.min*i))^nr.long.lag.c), digits = 6)
        TotWoodProd.nr.long.max <-  round(TotWoodProd.nr.long.new*(1-(1-exp(-nr.long.k.c.max*i))^nr.long.lag.c), digits = 6)
        
        this.year.cohort <- rbind(this.year.cohort,c("ID" = i, "Year" = year-1+i,"TotWoodProd.sf.short.avg"= TotWoodProd.sf.short.avg, 
                            "TotWoodProd.sf.short.min" = TotWoodProd.sf.short.min,"TotWoodProd.sf.short.max" = TotWoodProd.sf.short.max,
                            "TotWoodProd.sf.long.avg" = TotWoodProd.sf.long.avg,"TotWoodProd.sf.long.min"=TotWoodProd.sf.long.min,"TotWoodProd.sf.long.max"=TotWoodProd.sf.long.max,
                            "TotWoodProd.mf.short.avg"= TotWoodProd.mf.short.avg,"TotWoodProd.mf.short.min" = TotWoodProd.mf.short.min,"TotWoodProd.mf.short.max" = TotWoodProd.mf.short.max,
                            "TotWoodProd.mf.long.avg" = TotWoodProd.mf.long.avg,"TotWoodProd.mf.long.min"=TotWoodProd.mf.long.min,"TotWoodProd.mf.long.max"=TotWoodProd.mf.long.max,
                            "TotWoodProd.nr.short.avg"= TotWoodProd.nr.short.avg,"TotWoodProd.nr.short.min" = TotWoodProd.nr.short.min,"TotWoodProd.nr.short.max" = TotWoodProd.nr.short.max,
                            "TotWoodProd.nr.long.avg" = TotWoodProd.nr.long.avg,"TotWoodProd.nr.long.min"=TotWoodProd.nr.long.min,"TotWoodProd.nr.long.max"=TotWoodProd.nr.long.max))
      }
       
      assign(newname,this.year.cohort) # this saves one each year
       
      dataframes <- mget(paste("this.year.cohort",".",first.year:year,sep=""), envir=.GlobalEnv) #gets all existing dfs
      num.dfs <- length(unique(dataframes))
      print(num.dfs)
      for (a in 1:num.dfs) { #loop through dfs each year
        cohort <- dataframes[[a]]
        num.df.rows <- length(unique(cohort[,"Year"]))
        for (b in 1:num.df.rows){
          if(cohort[b,"Year"]==year){
            TotWoodProd.sf.short.avg.all <- TotWoodProd.sf.short.avg.all + as.double(cohort[b,"TotWoodProd.sf.short.avg"])
            TotWoodProd.sf.long.avg.all <- TotWoodProd.sf.long.avg.all + as.double(cohort[b,"TotWoodProd.sf.long.avg"])
            TotWoodProd.sf.short.min.all <- TotWoodProd.sf.short.min.all + as.double(cohort[b,"TotWoodProd.sf.short.min"])
            TotWoodProd.sf.long.min.all <- TotWoodProd.sf.long.min.all + as.double(cohort[b,"TotWoodProd.sf.long.min"])
            TotWoodProd.sf.short.max.all <- TotWoodProd.sf.short.max.all + as.double(cohort[b,"TotWoodProd.sf.short.max"])
            TotWoodProd.sf.long.max.all <- TotWoodProd.sf.long.max.all + as.double(cohort[b,"TotWoodProd.sf.long.max"])
            
            TotWoodProd.mf.short.avg.all <- TotWoodProd.mf.short.avg.all + as.double(cohort[b,"TotWoodProd.mf.short.avg"])
            TotWoodProd.mf.long.avg.all <- TotWoodProd.mf.long.avg.all + as.double(cohort[b,"TotWoodProd.mf.long.avg"])
            TotWoodProd.mf.short.min.all <- TotWoodProd.mf.short.min.all + as.double(cohort[b,"TotWoodProd.mf.short.min"])
            TotWoodProd.mf.long.min.all <- TotWoodProd.mf.long.min.all + as.double(cohort[b,"TotWoodProd.mf.long.min"])
            TotWoodProd.mf.short.max.all <- TotWoodProd.mf.short.max.all + as.double(cohort[b,"TotWoodProd.mf.short.max"])
            TotWoodProd.mf.long.max.all <- TotWoodProd.mf.long.max.all + as.double(cohort[b,"TotWoodProd.mf.long.max"])
            
            TotWoodProd.nr.short.avg.all <- TotWoodProd.nr.short.avg.all + as.double(cohort[b,"TotWoodProd.nr.short.avg"])
            TotWoodProd.nr.long.avg.all <- TotWoodProd.nr.long.avg.all + as.double(cohort[b,"TotWoodProd.nr.long.avg"])
            TotWoodProd.nr.short.min.all <- TotWoodProd.nr.short.min.all + as.double(cohort[b,"TotWoodProd.nr.short.min"])
            TotWoodProd.nr.long.min.all <- TotWoodProd.nr.long.min.all + as.double(cohort[b,"TotWoodProd.nr.long.min"])
            TotWoodProd.nr.short.max.all <- TotWoodProd.nr.short.max.all + as.double(cohort[b,"TotWoodProd.nr.short.max"])
            TotWoodProd.nr.long.max.all <- TotWoodProd.nr.long.max.all + as.double(cohort[b,"TotWoodProd.nr.long.max"])
          } # end if
          
        } #end loop ind df rows
      } #end loop through dataframes; the tot stores are now updated
      if (year > 1900) {
      WD.2.wood.sf.short.avg <- (as.double(LCA.Annual.2[x-1,"sf.short.avg"])+TotWoodProd.sf.short.new)- TotWoodProd.sf.short.avg.all
      WD.2.wood.sf.short.min <- (as.double(LCA.Annual.2[x-1,"sf.short.min"])+TotWoodProd.sf.short.new)- TotWoodProd.sf.short.min.all
      WD.2.wood.sf.short.max <- (as.double(LCA.Annual.2[x-1,"sf.short.max"])+TotWoodProd.sf.short.new)- TotWoodProd.sf.short.max.all
      
      WD.2.wood.mf.short.avg <- (as.double(LCA.Annual.2[x-1,"mf.short.avg"])+TotWoodProd.mf.short.new)- TotWoodProd.mf.short.avg.all
      WD.2.wood.mf.short.min <- (as.double(LCA.Annual.2[x-1,"mf.short.min"])+TotWoodProd.mf.short.new)- TotWoodProd.mf.short.min.all
      WD.2.wood.mf.short.max <- (as.double(LCA.Annual.2[x-1,"mf.short.max"])+TotWoodProd.mf.short.new)- TotWoodProd.mf.short.max.all
      
      WD.2.wood.nr.short.avg <- (as.double(LCA.Annual.2[x-1,"nr.short.avg"])+TotWoodProd.nr.short.new)- TotWoodProd.nr.short.avg.all
      WD.2.wood.nr.short.min <- (as.double(LCA.Annual.2[x-1,"nr.short.min"])+TotWoodProd.nr.short.new)- TotWoodProd.nr.short.min.all
      WD.2.wood.nr.short.max <- (as.double(LCA.Annual.2[x-1,"nr.short.max"])+TotWoodProd.nr.short.new)- TotWoodProd.nr.short.max.all
      
      WD.2.wood.sf.long.avg <- (as.double(LCA.Annual.2[x-1,"sf.long.avg"]) + TotWoodProd.sf.long.new) - TotWoodProd.sf.long.avg.all
      WD.2.wood.sf.long.min <- (as.double(LCA.Annual.2[x-1,"sf.long.min"]) + TotWoodProd.sf.long.new) - TotWoodProd.sf.long.min.all
      WD.2.wood.sf.long.max <- (as.double(LCA.Annual.2[x-1,"sf.long.max"]) + TotWoodProd.sf.long.new) - TotWoodProd.sf.long.max.all
      
      WD.2.wood.mf.long.avg <- (as.double(LCA.Annual.2[x-1,"mf.long.avg"]) + TotWoodProd.mf.long.new) - TotWoodProd.mf.long.avg.all
      WD.2.wood.mf.long.min <- (as.double(LCA.Annual.2[x-1,"mf.long.min"]) + TotWoodProd.mf.long.new) - TotWoodProd.mf.long.min.all
      WD.2.wood.mf.long.max <- (as.double(LCA.Annual.2[x-1,"mf.long.max"]) + TotWoodProd.mf.long.new) - TotWoodProd.mf.long.max.all
      
      WD.2.wood.nr.long.avg <- (as.double(LCA.Annual.2[x-1,"nr.long.avg"]) + TotWoodProd.nr.long.new) - TotWoodProd.nr.long.avg.all
      WD.2.wood.nr.long.min <- (as.double(LCA.Annual.2[x-1,"nr.long.min"]) + TotWoodProd.nr.long.new) - TotWoodProd.nr.long.min.all
      WD.2.wood.nr.long.max <- (as.double(LCA.Annual.2[x-1,"nr.long.max"]) + TotWoodProd.nr.long.new) - TotWoodProd.nr.long.max.all
      }
      
      # WD2 without CCM
      
      WD.2.wood.singlefamily.avg <- (TotWoodProd.singlefamily.avg*Single.family.k.c)
      WD.2.wood.nonresidential.avg <- (TotWoodProd.nonresidential.avg*NonResidential.k.c)
      WD.2.wood.furniture.manufactoring.avg <- (TotWoodProd.furniture.manufactoring.avg*FurnitureManufactoring.k.c)
      WD.2.wood.shipping.avg <- (TotWoodProd.shipping.avg*Shipping.k.c)
      WD.2.wood.otherwood.avg <- (TotWoodProd.otherwood.avg*OtherWood.k.c)
      WD.2.wood.avg <- WD.2.wood.singlefamily.avg+WD.2.wood.multifamily.avg+WD.2.wood.mobilehomes.avg+WD.2.wood.nonresidential.avg+WD.2.wood.furniture.manufactoring.avg+WD.2.wood.shipping.avg+WD.2.wood.otherwood.avg
      WD.2.wood.ccm.avg <- WD.2.wood.sf.short.avg+WD.2.wood.sf.long.avg+WD.2.wood.mf.short.avg+WD.2.wood.mf.long.avg+WD.2.wood.mobilehomes.avg+WD.2.wood.nr.short.avg+WD.2.wood.nr.long.avg+WD.2.wood.furniture.manufactoring.avg+WD.2.wood.shipping.avg+WD.2.wood.otherwood.avg
      
      WD.2.wood.singlefamily.min <- (TotWoodProd.singlefamily.min*Single.family.k.c.min)
      WD.2.wood.multifamily.min <- (TotWoodProd.multifamily.min*Multi.family.k.c.min)
      WD.2.wood.mobilehomes.min <- (TotWoodProd.mobilehomes.min*Mobile.home.k.c.min)
      WD.2.wood.nonresidential.min <- (TotWoodProd.nonresidential.min*NonResidential.k.c.min)
      WD.2.wood.furniture.manufactoring.min <- (TotWoodProd.furniture.manufactoring.min*FurnitureManufactoring.k.c.min)
      WD.2.wood.shipping.min <- (TotWoodProd.shipping.min*Shipping.k.c.min)
      WD.2.wood.otherwood.min <- (TotWoodProd.otherwood.min*OtherWood.k.c.min)
      WD.2.wood.min <- WD.2.wood.singlefamily.min+WD.2.wood.multifamily.min+WD.2.wood.mobilehomes.min+WD.2.wood.nonresidential.min+WD.2.wood.furniture.manufactoring.min+WD.2.wood.shipping.min+WD.2.wood.otherwood.min
      WD.2.wood.ccm.min <- WD.2.wood.sf.short.min+WD.2.wood.sf.long.min+WD.2.wood.mf.short.min+WD.2.wood.mf.long.min+WD.2.wood.mobilehomes.min+WD.2.wood.nr.short.min+WD.2.wood.nr.long.min+WD.2.wood.furniture.manufactoring.min+WD.2.wood.shipping.min+WD.2.wood.otherwood.min
      
      WD.2.wood.singlefamily.max <- (TotWoodProd.singlefamily.max*Single.family.k.c.max)
      WD.2.wood.multifamily.max <- (TotWoodProd.multifamily.max*Multi.family.k.c.max)
      WD.2.wood.mobilehomes.max <- (TotWoodProd.mobilehomes.max*Mobile.home.k.c.max)
      WD.2.wood.nonresidential.max <- (TotWoodProd.nonresidential.max*NonResidential.k.c.max)
      WD.2.wood.furniture.manufactoring.max <- (TotWoodProd.furniture.manufactoring.max*FurnitureManufactoring.k.c.max)
      WD.2.wood.shipping.max <- (TotWoodProd.shipping.max*Shipping.k.c.max)
      WD.2.wood.otherwood.max <- (TotWoodProd.otherwood.max*OtherWood.k.c.max)
      WD.2.wood.max <- WD.2.wood.singlefamily.max+WD.2.wood.multifamily.max+WD.2.wood.mobilehomes.max+WD.2.wood.nonresidential.max+WD.2.wood.furniture.manufactoring.max+WD.2.wood.shipping.max+WD.2.wood.otherwood.max
      WD.2.wood.ccm.max <- WD.2.wood.sf.short.max+WD.2.wood.sf.long.max+WD.2.wood.mf.short.max+WD.2.wood.mf.long.max+WD.2.wood.mobilehomes.max+WD.2.wood.nr.short.max+WD.2.wood.nr.long.max+WD.2.wood.furniture.manufactoring.max+WD.2.wood.shipping.max+WD.2.wood.otherwood.max
      
      # Recycle rates for wood (no recycle for ccm method; this is built into the lag parameter)
      wood.net.recycle.singlefamily.avg <- WD.2.wood.singlefamily.avg*wood.net.recycle.c
      wood.net.recycle.multifamily.avg <- WD.2.wood.multifamily.avg*wood.net.recycle.c
      wood.net.recycle.mobilehomes.avg <- WD.2.wood.mobilehomes.avg*wood.net.recycle.c
      wood.net.recycle.nonresidential.avg <- WD.2.wood.nonresidential.avg*wood.net.recycle.c
      wood.net.recycle.furniture.manufactoring.avg <- WD.2.wood.furniture.manufactoring.avg*wood.net.recycle.c
      wood.net.recycle.shipping.avg <- WD.2.wood.shipping.avg*wood.net.recycle.c
      wood.net.recycle.otherwood.avg <- WD.2.wood.otherwood.avg*wood.net.recycle.c
      
      wood.net.recycle.avg <- wood.net.recycle.singlefamily.avg+wood.net.recycle.multifamily.avg+wood.net.recycle.mobilehomes.avg+wood.net.recycle.nonresidential.avg+wood.net.recycle.furniture.manufactoring.avg+wood.net.recycle.shipping.avg+wood.net.recycle.otherwood.avg
      
      wood.net.recycle.singlefamily.min <- WD.2.wood.singlefamily.min*wood.net.recycle.c
      wood.net.recycle.multifamily.min <- WD.2.wood.multifamily.min*wood.net.recycle.c
      wood.net.recycle.mobilehomes.min <- WD.2.wood.mobilehomes.min*wood.net.recycle.c
      wood.net.recycle.nonresidential.min <- WD.2.wood.nonresidential.min*wood.net.recycle.c
      wood.net.recycle.furniture.manufactoring.min <- WD.2.wood.furniture.manufactoring.min*wood.net.recycle.c
      wood.net.recycle.shipping.min <- WD.2.wood.shipping.min*wood.net.recycle.c
      wood.net.recycle.otherwood.min <- WD.2.wood.otherwood.min*wood.net.recycle.c
      wood.net.recycle.min <- wood.net.recycle.singlefamily.min+wood.net.recycle.multifamily.min+wood.net.recycle.mobilehomes.min+wood.net.recycle.nonresidential.min+wood.net.recycle.furniture.manufactoring.min+wood.net.recycle.shipping.min+wood.net.recycle.otherwood.min
        
      wood.net.recycle.singlefamily.max <- WD.2.wood.singlefamily.max*wood.net.recycle.c
      wood.net.recycle.multifamily.max <- WD.2.wood.multifamily.max*wood.net.recycle.c
      wood.net.recycle.mobilehomes.max <- WD.2.wood.mobilehomes.max*wood.net.recycle.c
      wood.net.recycle.nonresidential.max <- WD.2.wood.nonresidential.max*wood.net.recycle.c
      wood.net.recycle.furniture.manufactoring.max <- WD.2.wood.furniture.manufactoring.max*wood.net.recycle.c
      wood.net.recycle.shipping.max <- WD.2.wood.shipping.max*wood.net.recycle.c
      wood.net.recycle.otherwood.max <- WD.2.wood.otherwood.max*wood.net.recycle.c
      wood.net.recycle.max <- wood.net.recycle.singlefamily.max+wood.net.recycle.multifamily.max+wood.net.recycle.mobilehomes.max+wood.net.recycle.nonresidential.max+wood.net.recycle.furniture.manufactoring.max+wood.net.recycle.shipping.max+wood.net.recycle.otherwood.max
      
      # subtract recycled from wd2 because it goes back in the pool prior to redefining the pools.  ##!!
      
      WD.2.wood.singlefamily.avg <- WD.2.wood.singlefamily.avg - wood.net.recycle.singlefamily.avg
      WD.2.wood.multifamily.avg <- WD.2.wood.multifamily.avg - wood.net.recycle.multifamily.avg
      WD.2.wood.mobilehomes.avg <- WD.2.wood.mobilehomes.avg - wood.net.recycle.mobilehomes.avg
      WD.2.wood.nonresidential.avg <- WD.2.wood.nonresidential.avg - wood.net.recycle.nonresidential.avg
      WD.2.wood.furniture.manufactoring.avg <- WD.2.wood.furniture.manufactoring.avg - wood.net.recycle.furniture.manufactoring.avg
      WD.2.wood.shipping.avg <- WD.2.wood.shipping.avg - wood.net.recycle.shipping.avg
      WD.2.wood.otherwood.avg <- WD.2.wood.otherwood.avg - wood.net.recycle.otherwood.avg
      WD.2.wood.avg <- WD.2.wood.avg - wood.net.recycle.avg
      
      WD.2.wood.singlefamily.min <- WD.2.wood.singlefamily.min - wood.net.recycle.singlefamily.min
      WD.2.wood.multifamily.min <- WD.2.wood.multifamily.min - wood.net.recycle.multifamily.min
      WD.2.wood.mobilehomes.min <- WD.2.wood.mobilehomes.min - wood.net.recycle.mobilehomes.min
      WD.2.wood.nonresidential.min <- WD.2.wood.nonresidential.min - wood.net.recycle.nonresidential.min
      WD.2.wood.furniture.manufactoring.min <- WD.2.wood.furniture.manufactoring.min - wood.net.recycle.furniture.manufactoring.min
      WD.2.wood.shipping.min <- WD.2.wood.shipping.min - wood.net.recycle.shipping.min
      WD.2.wood.otherwood.min <- WD.2.wood.otherwood.min - wood.net.recycle.otherwood.min
      WD.2.wood.min <- WD.2.wood.min - wood.net.recycle.min
      
      WD.2.wood.singlefamily.max <- WD.2.wood.singlefamily.max - wood.net.recycle.singlefamily.max
      WD.2.wood.multifamily.max <- WD.2.wood.multifamily.max - wood.net.recycle.multifamily.max
      WD.2.wood.mobilehomes.max <- WD.2.wood.mobilehomes.max - wood.net.recycle.mobilehomes.max
      WD.2.wood.nonresidential.max <- WD.2.wood.nonresidential.max - wood.net.recycle.nonresidential.max
      WD.2.wood.furniture.manufactoring.max <- WD.2.wood.furniture.manufactoring.max - wood.net.recycle.furniture.manufactoring.max
      WD.2.wood.shipping.max <- WD.2.wood.shipping.max - wood.net.recycle.shipping.max
      WD.2.wood.otherwood.max <- WD.2.wood.otherwood.max - wood.net.recycle.otherwood.max
      WD.2.wood.max <- WD.2.wood.max - wood.net.recycle.max
      
      # Landfill
      
      if (year >1950){
      landfill.wood.avg.new <- WD.2.wood.avg * wood.product.landfill.c*.5 # start landfilling at half rates
      landfill.wood.ccm.avg.new <- WD.2.wood.ccm.avg *wood.product.landfill.c *.5
      landfill.wood.min.new <- WD.2.wood.min * wood.product.landfill.c *.5
      landfill.wood.ccm.min.new <- WD.2.wood.ccm.min * wood.product.landfill.c *.5
      landfill.wood.max.new <- WD.2.wood.max * wood.product.landfill.c*.5
      landfill.wood.ccm.max.new<- WD.2.wood.ccm.max * wood.product.landfill.c *.5
      
      landfill.permanent.wood.avg <- landfill.permanent.wood.avg + landfill.wood.avg.new * (1- wood.product.degradable.landfill.c)
      landfill.permanent.wood.ccm.avg <- landfill.permanent.wood.ccm.avg + landfill.wood.ccm.avg.new * (1- wood.product.degradable.landfill)
      
      landfill.permanent.wood.min <- landfill.permanent.wood.min + landfill.wood.min.new * (1- wood.product.degradable.landfill.c)
      landfill.permanent.wood.ccm.min <- landfill.permanent.wood.ccm.min + landfill.wood.ccm.min.new * (1- wood.product.degradable.landfill)
      
      landfill.permanent.wood.max <- landfill.permanent.wood.max + landfill.wood.max.new * (1- wood.product.degradable.landfill.c)
      landfill.permanent.wood.ccm.max <- landfill.permanent.wood.ccm.max + landfill.wood.ccm.max.new * (1- wood.product.degradable.landfill)
      
      landfill.degradable.wood.avg <- landfill.degradable.wood.avg + landfill.wood.avg.new * wood.product.degradable.landfill.c
      landfill.degradable.wood.ccm.avg <- landfill.degradable.wood.ccm.avg + landfill.wood.ccm.avg.new * wood.product.degradable.landfill
      
      landfill.degradable.wood.min <- landfill.degradable.wood.min + landfill.wood.min.new * wood.product.degradable.landfill.c
      landfill.degradable.wood.ccm.min <- landfill.degradable.wood.ccm.min + landfill.wood.ccm.min.new * wood.product.degradable.landfill
      
      landfill.degradable.wood.max <- landfill.degradable.wood.max + landfill.wood.max.new * wood.product.degradable.landfill.c
      landfill.degradable.wood.ccm.max <- landfill.degradable.wood.ccm.max + landfill.wood.ccm.max.new * wood.product.degradable.landfill
      
      #landfill emissions
      landfill.wood.emissions.avg  <- landfill.degradable.wood.avg*wood.product.degradable.landfill.k
      landfill.wood.emissions.ccm.avg  <- landfill.degradable.wood.ccm.avg*wood.product.degradable.landfill.k
      
      landfill.wood.emissions.min  <- landfill.degradable.wood.min*wood.product.degradable.landfill.k
      landfill.wood.emissions.ccm.min  <- landfill.degradable.wood.ccm.min*wood.product.degradable.landfill.k
      
      landfill.wood.emissions.max  <- landfill.degradable.wood.max*wood.product.degradable.landfill.k
      landfill.wood.emissions.ccm.max  <- landfill.degradable.wood.ccm.max*wood.product.degradable.landfill.k
      }
      
      if (year >1955){
        landfill.wood.avg.new <- WD.2.wood.avg * wood.product.landfill.c*.75 # start landfilling at half rates
        landfill.wood.ccm.avg.new <- WD.2.wood.ccm.avg *wood.product.landfill.c *.75
        landfill.wood.min.new <- WD.2.wood.min * wood.product.landfill.c *.75
        landfill.wood.ccm.min.new <- WD.2.wood.ccm.min * wood.product.landfill.c *.75
        landfill.wood.max.new <- WD.2.wood.max * wood.product.landfill.c*.75
        landfill.wood.ccm.max.new<- WD.2.wood.ccm.max * wood.product.landfill.c *.75
        
        landfill.permanent.wood.avg <- landfill.permanent.wood.avg + landfill.wood.avg.new * (1- wood.product.degradable.landfill.c)
        landfill.permanent.wood.ccm.avg <- landfill.permanent.wood.ccm.avg + landfill.wood.ccm.avg.new * (1- wood.product.degradable.landfill)
        
        landfill.permanent.wood.min <- landfill.permanent.wood.min + landfill.wood.min.new * (1- wood.product.degradable.landfill.c)
        landfill.permanent.wood.ccm.min <- landfill.permanent.wood.ccm.min + landfill.wood.ccm.min.new * (1- wood.product.degradable.landfill)
        
        landfill.permanent.wood.max <- landfill.permanent.wood.max + landfill.wood.max.new * (1- wood.product.degradable.landfill.c)
        landfill.permanent.wood.ccm.max <- landfill.permanent.wood.ccm.max + landfill.wood.ccm.max.new * (1- wood.product.degradable.landfill)
        
        landfill.degradable.wood.avg <- landfill.degradable.wood.avg + landfill.wood.avg.new * wood.product.degradable.landfill.c
        landfill.degradable.wood.ccm.avg <- landfill.degradable.wood.ccm.avg + landfill.wood.ccm.avg.new * wood.product.degradable.landfill
        
        landfill.degradable.wood.min <- landfill.degradable.wood.min + landfill.wood.min.new * wood.product.degradable.landfill.c
        landfill.degradable.wood.ccm.min <- landfill.degradable.wood.ccm.min + landfill.wood.ccm.min.new * wood.product.degradable.landfill
        
        landfill.degradable.wood.max <- landfill.degradable.wood.max + landfill.wood.max.new * wood.product.degradable.landfill.c
        landfill.degradable.wood.ccm.max <- landfill.degradable.wood.ccm.max + landfill.wood.ccm.max.new * wood.product.degradable.landfill
        
        #landfill emissions
        landfill.wood.emissions.avg  <- landfill.degradable.wood.avg*wood.product.degradable.landfill.k
        landfill.wood.emissions.ccm.avg  <- landfill.degradable.wood.ccm.avg*wood.product.degradable.landfill.k
        
        landfill.wood.emissions.min  <- landfill.degradable.wood.min*wood.product.degradable.landfill.k
        landfill.wood.emissions.ccm.min  <- landfill.degradable.wood.ccm.min*wood.product.degradable.landfill.k
        
        landfill.wood.emissions.max  <- landfill.degradable.wood.max*wood.product.degradable.landfill.k
        landfill.wood.emissions.ccm.max  <- landfill.degradable.wood.ccm.max*wood.product.degradable.landfill.k
      }
      
      
      if (year >1960){
        landfill.wood.avg.new <- WD.2.wood.avg * wood.product.landfill.c # at max landfill rates
        landfill.wood.ccm.avg.new <- WD.2.wood.ccm.avg *wood.product.landfill.c
        landfill.wood.min.new <- WD.2.wood.min * wood.product.landfill.c
        landfill.wood.ccm.min.new <- WD.2.wood.ccm.min * wood.product.landfill.c
        landfill.wood.max.new <- WD.2.wood.max * wood.product.landfill.c
        landfill.wood.ccm.max.new<- WD.2.wood.ccm.max * wood.product.landfill.c
        
        landfill.permanent.wood.avg <- landfill.permanent.wood.avg + landfill.wood.avg.new * (1- wood.product.degradable.landfill.c)
        landfill.permanent.wood.ccm.avg <- landfill.permanent.wood.ccm.avg + landfill.wood.ccm.avg.new * (1- wood.product.degradable.landfill)
        
        landfill.permanent.wood.min <- landfill.permanent.wood.min + landfill.wood.min.new * (1- wood.product.degradable.landfill.c)
        landfill.permanent.wood.ccm.min <- landfill.permanent.wood.ccm.min + landfill.wood.ccm.min.new * (1- wood.product.degradable.landfill)
        
        landfill.permanent.wood.max <- landfill.permanent.wood.max + landfill.wood.max.new * (1- wood.product.degradable.landfill.c)
        landfill.permanent.wood.ccm.max <- landfill.permanent.wood.ccm.max + landfill.wood.ccm.max.new * (1- wood.product.degradable.landfill)
        
        landfill.degradable.wood.avg <- landfill.degradable.wood.avg + landfill.wood.avg.new * wood.product.degradable.landfill.c
        landfill.degradable.wood.ccm.avg <- landfill.degradable.wood.ccm.avg + landfill.wood.ccm.avg.new * wood.product.degradable.landfill
        
        landfill.degradable.wood.min <- landfill.degradable.wood.min + landfill.wood.min.new * wood.product.degradable.landfill.c
        landfill.degradable.wood.ccm.min <- landfill.degradable.wood.ccm.min + landfill.wood.ccm.min.new * wood.product.degradable.landfill
        
        landfill.degradable.wood.max <- landfill.degradable.wood.max + landfill.wood.max.new * wood.product.degradable.landfill.c
        landfill.degradable.wood.ccm.max <- landfill.degradable.wood.ccm.max + landfill.wood.ccm.max.new * wood.product.degradable.landfill
        
        #landfill emissions
        landfill.wood.emissions.avg  <- landfill.degradable.wood.avg*wood.product.degradable.landfill.k
        landfill.wood.emissions.ccm.avg  <- landfill.degradable.wood.ccm.avg*wood.product.degradable.landfill.k
        
        landfill.wood.emissions.min  <- landfill.degradable.wood.min*wood.product.degradable.landfill.k
        landfill.wood.emissions.ccm.min  <- landfill.degradable.wood.ccm.min*wood.product.degradable.landfill.k
        
        landfill.wood.emissions.max  <- landfill.degradable.wood.max*wood.product.degradable.landfill.k
        landfill.wood.emissions.ccm.max  <- landfill.degradable.wood.ccm.max*wood.product.degradable.landfill.k
      }
      #Decay paper
      WD.2.paper.avg <- TotWoodProd.paper.avg*Paper.k.c
      WD.2.paper.min <- TotWoodProd.paper.min*Paper.k.c.min
      WD.2.paper.max <- TotWoodProd.paper.max*Paper.k.c.max
      
     
       #landfill and recyle paper and decay it
      if (year > 1950) {
      paper.net.recycle.avg <- WD.2.paper.avg*paper.net.recycle.c
      paper.net.recycle.min <- WD.2.paper.min*paper.net.recycle.c
      paper.net.recycle.max <- WD.2.paper.max*paper.net.recycle.c
      
      WD.2.paper.avg <- WD.2.paper.avg - paper.net.recycle.avg
      WD.2.paper.min <- WD.2.paper.min - paper.net.recycle.min
      WD.2.paper.max <- WD.2.paper.max - paper.net.recycle.max
      
      landfill.paper.avg.new <- WD.2.paper.avg*paper.landfill.c*.5
      landfill.paper.min.new <- WD.2.paper.min*paper.landfill.c*.5
      landfill.paper.max.new <- WD.2.paper.max*paper.landfill.c*.5
      
      landfill.permanent.paper.avg <- landfill.permanent.paper.avg + (1- paper.degradable.landfill.c)* landfill.paper.avg.new
      landfill.permanent.paper.min <- landfill.permanent.paper.min + (1- paper.degradable.landfill.c)* landfill.paper.min.new
      landfill.permanent.paper.max <- landfill.permanent.paper.max + (1- paper.degradable.landfill.c)* landfill.paper.max.new
      
      landfill.degradable.paper.avg <- landfill.degradable.paper.avg + paper.degradable.landfill.c*landfill.paper.avg.new
      landfill.degradable.paper.min <- landfill.degradable.paper.min + paper.degradable.landfill.c*landfill.paper.min.new
      landfill.degradable.paper.max <- landfill.degradable.paper.max + paper.degradable.landfill.c*landfill.paper.max.new
      
      landfill.paper.emissions.avg  <- landfill.degradable.paper.avg*paper.degradable.landfill.k
      landfill.paper.emissions.min <- landfill.degradable.paper.min*paper.degradable.landfill.k
      landfill.paper.emissions.max  <- landfill.degradable.paper.max*paper.degradable.landfill.k
      }
      if (year > 1955) {
        paper.net.recycle.avg <- WD.2.paper.avg*paper.net.recycle.c
        paper.net.recycle.min <- WD.2.paper.min*paper.net.recycle.c
        paper.net.recycle.max <- WD.2.paper.max*paper.net.recycle.c
        
        WD.2.paper.avg <- WD.2.paper.avg - paper.net.recycle.avg
        WD.2.paper.min <- WD.2.paper.min - paper.net.recycle.min
        WD.2.paper.max <- WD.2.paper.max - paper.net.recycle.max
        
        landfill.paper.avg.new <- WD.2.paper.avg*paper.landfill.c*.75
        landfill.paper.min.new <- WD.2.paper.min*paper.landfill.c*.75
        landfill.paper.max.new <- WD.2.paper.max*paper.landfill.c*.75
        
        landfill.permanent.paper.avg <- landfill.permanent.paper.avg + (1- paper.degradable.landfill.c)* landfill.paper.avg.new
        landfill.permanent.paper.min <- landfill.permanent.paper.min + (1- paper.degradable.landfill.c)* landfill.paper.min.new
        landfill.permanent.paper.max <- landfill.permanent.paper.max + (1- paper.degradable.landfill.c)* landfill.paper.max.new
        
        landfill.degradable.paper.avg <- landfill.degradable.paper.avg + paper.degradable.landfill.c*landfill.paper.avg.new
        landfill.degradable.paper.min <- landfill.degradable.paper.min + paper.degradable.landfill.c*landfill.paper.min.new
        landfill.degradable.paper.max <- landfill.degradable.paper.max + paper.degradable.landfill.c*landfill.paper.max.new
        
        landfill.paper.emissions.avg  <- landfill.degradable.paper.avg*paper.degradable.landfill.k
        landfill.paper.emissions.min <- landfill.degradable.paper.min*paper.degradable.landfill.k
        landfill.paper.emissions.max  <- landfill.degradable.paper.max*paper.degradable.landfill.k
      }
      if (year > 1960) {
        paper.net.recycle.avg <- WD.2.paper.avg*paper.net.recycle.c
        paper.net.recycle.min <- WD.2.paper.min*paper.net.recycle.c
        paper.net.recycle.max <- WD.2.paper.max*paper.net.recycle.c
        
        WD.2.paper.avg <- WD.2.paper.avg - paper.net.recycle.avg
        WD.2.paper.min <- WD.2.paper.min - paper.net.recycle.min
        WD.2.paper.max <- WD.2.paper.max - paper.net.recycle.max
        
        landfill.paper.avg.new <- WD.2.paper.avg*paper.landfill.c
        landfill.paper.min.new <- WD.2.paper.min*paper.landfill.c
        landfill.paper.max.new <- WD.2.paper.max*paper.landfill.c
        
        landfill.permanent.paper.avg <- landfill.permanent.paper.avg + (1- paper.degradable.landfill.c)* landfill.paper.avg.new
        landfill.permanent.paper.min <- landfill.permanent.paper.min + (1- paper.degradable.landfill.c)* landfill.paper.min.new
        landfill.permanent.paper.max <- landfill.permanent.paper.max + (1- paper.degradable.landfill.c)* landfill.paper.max.new
        
        landfill.degradable.paper.avg <- landfill.degradable.paper.avg + paper.degradable.landfill.c*landfill.paper.avg.new
        landfill.degradable.paper.min <- landfill.degradable.paper.min + paper.degradable.landfill.c*landfill.paper.min.new
        landfill.degradable.paper.max <- landfill.degradable.paper.max + paper.degradable.landfill.c*landfill.paper.max.new
        
        landfill.paper.emissions.avg  <- landfill.degradable.paper.avg*paper.degradable.landfill.k
        landfill.paper.emissions.min <- landfill.degradable.paper.min*paper.degradable.landfill.k
        landfill.paper.emissions.max  <- landfill.degradable.paper.max*paper.degradable.landfill.k
      }
      
      # Total WD2s (annual)
      WD.2.avg <- WD.2.wood.avg + WD.2.paper.avg
      WD.2.ccm.avg <- WD.2.wood.ccm.avg + WD.2.paper.avg
      WD.2.min <- WD.2.wood.min + WD.2.paper.min
      WD.2.ccm.min <- WD.2.wood.ccm.min + WD.2.paper.min
      WD.2.max <- WD.2.wood.max + WD.2.paper.max
      WD.2.ccm.max <- WD.2.wood.ccm.max + WD.2.paper.max
      
      # redefine non ccm pools for after recycling (earlier in code); Do this before redefine wd2 for landfill pool (part of wd2 goes to the landfill)
      
      TotWoodProd.singlefamily.avg <- TotWoodProd.singlefamily.avg - WD.2.wood.singlefamily.avg
      TotWoodProd.multifamily.avg <- TotWoodProd.multifamily.avg - WD.2.wood.multifamily.avg
      TotWoodProd.mobilehomes.avg <- TotWoodProd.mobilehomes.avg - WD.2.wood.mobilehomes.avg
      TotWoodProd.nonresidential.avg <- TotWoodProd.nonresidential.avg - WD.2.wood.nonresidential.avg
      TotWoodProd.shipping.avg <- TotWoodProd.shipping.avg - WD.2.wood.shipping.avg
      TotWoodProd.furniture.manufactoring.avg <- TotWoodProd.furniture.manufactoring.avg - WD.2.wood.furniture.manufactoring.avg
      TotWoodProd.otherwood.avg <- TotWoodProd.otherwood.avg - WD.2.wood.otherwood.avg
      TotWoodProd.paper.avg <- TotWoodProd.paper.avg - WD.2.paper.avg
      TotWoodProd.wood.avg <- TotWoodProd.wood.avg - WD.2.wood.avg
      TotWoodProd.avg <- TotWoodProd.avg - WD.2.avg
      
      TotWoodProd.singlefamily.min <- TotWoodProd.singlefamily.min - WD.2.wood.singlefamily.min 
      TotWoodProd.multifamily.min <- TotWoodProd.multifamily.min - WD.2.wood.multifamily.min
      TotWoodProd.mobilehomes.min <- TotWoodProd.mobilehomes.min - WD.2.wood.mobilehomes.min
      TotWoodProd.nonresidential.min <- TotWoodProd.nonresidential.min - WD.2.wood.nonresidential.min
      TotWoodProd.shipping.min <- TotWoodProd.shipping.min - WD.2.wood.shipping.min
      TotWoodProd.furniture.manufactoring.min <- TotWoodProd.furniture.manufactoring.min - WD.2.wood.furniture.manufactoring.min
      TotWoodProd.otherwood.min <- TotWoodProd.otherwood.min - WD.2.wood.otherwood.min
      TotWoodProd.paper.min <- TotWoodProd.paper.min - WD.2.paper.min
      TotWoodProd.wood.min <- TotWoodProd.wood.min - WD.2.wood.min
      TotWoodProd.min <- TotWoodProd.min - WD.2.min 
      
      TotWoodProd.singlefamily.max <- TotWoodProd.singlefamily.max - WD.2.wood.singlefamily.max 
      TotWoodProd.multifamily.max <- TotWoodProd.multifamily.max - WD.2.wood.multifamily.max
      TotWoodProd.mobilehomes.max <- TotWoodProd.mobilehomes.max - WD.2.wood.mobilehomes.max
      TotWoodProd.nonresidential.max <- TotWoodProd.nonresidential.max - WD.2.wood.nonresidential.max
      TotWoodProd.shipping.max <- TotWoodProd.shipping.max - WD.2.wood.shipping.max
      TotWoodProd.furniture.manufactoring.max <- TotWoodProd.furniture.manufactoring.max - WD.2.wood.furniture.manufactoring.max
      TotWoodProd.otherwood.max <- TotWoodProd.otherwood.min - WD.2.wood.otherwood.max
      TotWoodProd.paper.max <- TotWoodProd.paper.max - WD.2.paper.max
      TotWoodProd.wood.max <- TotWoodProd.wood.max - WD.2.wood.max
      TotWoodProd.max <- TotWoodProd.max - WD.2.max
      
      #redefine all pools after subtracting decayed amout (add new inputs)
      TotWoodProd.singlefamily.avg <- TotWoodProd.singlefamily.avg +TotWoodProd.singlefamily.new # hold the old value before redefining on next line
      TotWoodProd.multifamily.avg <- TotWoodProd.multifamily.avg + TotWoodProd.multifamily.new
      TotWoodProd.mobilehomes.avg <- TotWoodProd.mobilehomes.avg + TotWoodProd.mobilehomes.new
      TotWoodProd.nonresidential.avg <- TotWoodProd.nonresidential.avg + TotWoodProd.nonresidential.new
      TotWoodProd.shipping.avg <- TotWoodProd.shipping.avg + TotWoodProd.shipping.new
      TotWoodProd.furniture.manufactoring.avg <- TotWoodProd.furniture.manufactoring.avg + TotWoodProd.furniture.manufactoring.new
      TotWoodProd.otherwood.avg <- TotWoodProd.otherwood.avg + TotWoodProd.otherwood.new
      TotWoodProd.wood.avg <- TotWoodProd.singlefamily.avg + TotWoodProd.multifamily.avg + TotWoodProd.mobilehomes.avg + TotWoodProd.furniture.manufactoring.avg + TotWoodProd.nonresidential.avg + TotWoodProd.shipping.avg + TotWoodProd.otherwood.avg
      TotWoodProd.wood.ccm.avg <- TotWoodProd.sf.short.avg.all+TotWoodProd.sf.long.avg.all + TotWoodProd.mf.short.avg.all+ TotWoodProd.mf.long.avg.all + TotWoodProd.mobilehomes.avg + TotWoodProd.furniture.manufactoring.avg + TotWoodProd.nr.short.avg.all+TotWoodProd.nr.long.avg.all + TotWoodProd.shipping.avg + TotWoodProd.otherwood.avg
      
      TotWoodProd.singlefamily.min <- TotWoodProd.singlefamily.min +TotWoodProd.singlefamily.new # hold the old value before redefining on next line
      TotWoodProd.multifamily.min <- TotWoodProd.multifamily.min + TotWoodProd.multifamily.new
      TotWoodProd.mobilehomes.min <- TotWoodProd.mobilehomes.min + TotWoodProd.mobilehomes.new
      TotWoodProd.nonresidential.min <- TotWoodProd.nonresidential.min + TotWoodProd.nonresidential.new
      TotWoodProd.shipping.min <- TotWoodProd.shipping.min + TotWoodProd.shipping.new
      TotWoodProd.furniture.manufactoring.min <- TotWoodProd.furniture.manufactoring.min + TotWoodProd.furniture.manufactoring.new
      TotWoodProd.otherwood.min <- TotWoodProd.otherwood.min + TotWoodProd.otherwood.new
      TotWoodProd.wood.min <- TotWoodProd.singlefamily.min + TotWoodProd.multifamily.min + TotWoodProd.mobilehomes.min + TotWoodProd.furniture.manufactoring.min + TotWoodProd.nonresidential.min + TotWoodProd.shipping.min + TotWoodProd.otherwood.min
      TotWoodProd.wood.ccm.min <- TotWoodProd.sf.short.min.all+TotWoodProd.sf.long.min.all + TotWoodProd.mf.short.min.all+ TotWoodProd.mf.long.min.all + TotWoodProd.mobilehomes.min + TotWoodProd.furniture.manufactoring.min + TotWoodProd.nr.short.min.all+TotWoodProd.nr.long.min.all + TotWoodProd.shipping.min + TotWoodProd.otherwood.min
      
      TotWoodProd.singlefamily.max <- TotWoodProd.singlefamily.max +TotWoodProd.singlefamily.new # hold the old value before redefining on next line
      TotWoodProd.multifamily.max <- TotWoodProd.multifamily.max + TotWoodProd.multifamily.new
      TotWoodProd.mobilehomes.max <- TotWoodProd.mobilehomes.max + TotWoodProd.mobilehomes.new
      TotWoodProd.nonresidential.max <- TotWoodProd.nonresidential.max + TotWoodProd.nonresidential.new
      TotWoodProd.shipping.max <- TotWoodProd.shipping.max + TotWoodProd.shipping.new
      TotWoodProd.furniture.manufactoring.max <- TotWoodProd.furniture.manufactoring.max + TotWoodProd.furniture.manufactoring.new
      TotWoodProd.otherwood.max <- TotWoodProd.otherwood.max + TotWoodProd.otherwood.new
      TotWoodProd.wood.max <- TotWoodProd.singlefamily.max + TotWoodProd.multifamily.max + TotWoodProd.mobilehomes.max + TotWoodProd.furniture.manufactoring.max + TotWoodProd.nonresidential.max + TotWoodProd.shipping.max + TotWoodProd.otherwood.max
      TotWoodProd.wood.ccm.max <- TotWoodProd.sf.short.max.all+TotWoodProd.sf.long.max.all + TotWoodProd.mf.short.max.all+ TotWoodProd.mf.long.max.all + TotWoodProd.mobilehomes.max + TotWoodProd.furniture.manufactoring.max + TotWoodProd.nr.short.max.all+TotWoodProd.nr.long.max.all + TotWoodProd.shipping.max + TotWoodProd.otherwood.max
      
      #all paper
      TotWoodProd.paper.avg <- TotWoodProd.paper.avg + TotWoodProd.paper.new
      TotWoodProd.paper.min <- TotWoodProd.paper.min + TotWoodProd.paper.new
      TotWoodProd.paper.max <- TotWoodProd.paper.max + TotWoodProd.paper.new
      #all products
      TotWoodProd.avg<- TotWoodProd.wood.avg + TotWoodProd.paper.avg
      TotWoodProd.min <- TotWoodProd.wood.min + TotWoodProd.paper.min
      TotWoodProd.max <- TotWoodProd.wood.max + TotWoodProd.paper.max
      
      #all products with ccm totals
      TotWoodProd.ccm.avg <- TotWoodProd.wood.ccm.avg + TotWoodProd.paper.avg
      TotWoodProd.ccm.min <- TotWoodProd.wood.ccm.min + TotWoodProd.paper.min
      TotWoodProd.ccm.max <- TotWoodProd.wood.ccm.max + TotWoodProd.paper.max
      
      #redefine wd2 so not double counted in landfill emissions
      if (year > 1950){
      WD.2.wood.avg <- WD.2.wood.avg - landfill.wood.avg.new
      WD.2.wood.ccm.avg <- WD.2.wood.ccm.avg - landfill.wood.ccm.avg.new
      WD.2.wood.min <- WD.2.wood.min - landfill.wood.min.new
      WD.2.wood.ccm.min <- WD.2.wood.ccm.min - landfill.wood.ccm.min.new
      WD.2.wood.max <- WD.2.wood.max - landfill.wood.max.new
      WD.2.wood.ccm.max <- WD.2.wood.ccm.max - landfill.wood.ccm.max.new
      WD.2.paper.avg <- WD.2.paper.avg - landfill.paper.avg.new
      WD.2.paper.min <- WD.2.paper.min - landfill.paper.min.new
      WD.2.paper.max <- WD.2.paper.max - landfill.paper.max.new
      }
      
      WD.2.avg <- WD.2.paper.avg + WD.2.wood.avg
      WD.2.ccm.avg <- WD.2.paper.avg + WD.2.wood.ccm.avg
      WD.2.min <- WD.2.paper.min + WD.2.wood.min
      WD.2.ccm.min <- WD.2.paper.min + WD.2.wood.ccm.min
      WD.2.max <- WD.2.paper.max + WD.2.wood.max
      WD.2.ccm.max <- WD.2.paper.max + WD.2.wood.ccm.max
      
       ###Wood product FFE emissions ####
     
      harv.E <- harv.E.c * (Merch.TgCyr)
      harv.tran.E <- tran.E.perkm.c * (Merch.TgCyr*1.5) * (distance.to.mill + distance.to.mill) #roundtrip distance to mill; moisture content weight of logs (50%)
      
      wood.manu.E <- wood.manu.E.c * fraction.wood.product
      paper.manu.E <- paper.manu.E.c* fraction.paper.product
      
      prod.tran.E <- tran.E.perkm.c * TotWoodProd.new * distance.to.use  #300km roundtrip; emissions do not accumulate nor decay each year, so just for this years wood
      
      # Emissions if doing bionenergy with residues THIS IS Oregon Boardman Plant specific
      Torr.Total.manu.E <- (Residues.TgCyr*0.5)*(torr.pelletizing.wc.E.c+chip.E.C+torr.E.c) #collect half
      Torr.trans.E <- (tran.E.perkm.c*1000*(Residues.TgCyr*.5)*1.03) + (tran.E.perkm.c*100*1.35) #distibuted torrefaction where 1000km is roundtrip distance and 100km is roundtrip distance of plants with full MC of wood chips
      WC.Total.manu.E <- (Residues.TgCyr*0.5)*(wc.pelletizing.E.c+chip.E.C+wc.drying.E.C)
      WC.trans.E <- tran.E.perkm.c*1000*((Residues.TgCyr*.5)*1.35) #distance to Boardman and mc of wood chips; chipped on site
      Torr.combust.E <- Residues.TgCyr*0.5
      WC.combust.E <- Residues.TgCyr*0.5
      
        ### Totals ##
      TotTransEmis.TgCyr <- harv.tran.E + prod.tran.E
      TotManuEmis.TgCyr <- wood.manu.E + paper.manu.E
      TotProdEmis.TgCyr <- TotTransEmis.TgCyr + TotManuEmis.TgCyr +harv.E
      Tot.WCEmis.TgCyr <- WC.Total.manu.E + WC.trans.E
      Tot.TorrEmis.TgCyr <- Torr.Total.manu.E + Torr.trans.E
      
      ### substitution and displacement credits
      
      wood.sub.25 <- wood.sub.c *fraction.wood.product*0.25
      wood.sub.50 <- wood.sub.c *fraction.wood.product*0.50
      wood.sub.75 <- wood.sub.c *fraction.wood.product*0.75
      wood.sub.100 <- wood.sub.c *fraction.wood.product*1.00
      
      mill.residues.energy.sub.50 <- chp.millresidue.sub.c * (WD.1*0.50) + energy.WTT.sub.c *(WD.1*0.5) #assume 50% of WD1 is recycled for energy at the plants or elsewhere
      mill.residues.energy.sub.75 <- chp.millresidue.sub.c * (WD.1*0.75) + energy.WTT.sub.c*(WD.1*0.75) #assume 75% of WD1 is recycled for energy at the plants or elsewhere
      
      #boardman
      wc.E.sub <- boardman.chip.ffe.e.sub*Residues.TgCyr*0.5
      wc.FFE.WTT.sub <- energy.WTT.sub.c*Residues.TgCyr*0.5
      torr.E.sub <- boardman.torr.ffe.e.sub*Residues.TgCyr*0.5
      torr.FFE.WTT.sub <- energy.WTT.sub.c*Residues.TgCyr*0.5
      boardman.wc.sub <- wc.E.sub + wc.FFE.WTT.sub
      boardman.torr.sub <- torr.E.sub+torr.FFE.WTT.sub
      
      ############format vars###########
      
      harv.E= round(harv.E,digits=6)
      TotTransEmis.TgCyr=round(TotTransEmis.TgCyr,digits=6)
      TotManuEmis.TgCyr = round(TotManuEmis.TgCyr,digits=8)
      TotProdEmis.TgCyr = round(TotProdEmis.TgCyr,digits=8)
      TotWoodProd.new = round(TotWoodProd.new,digits = 6)
      TotWoodProd.avg = round(TotWoodProd.avg,digits=6)
      TotWoodProd.min = round(TotWoodProd.min,digits=6)
      TotWoodProd.max = round(TotWoodProd.max,digits=6)
      
      WD.1 = round(WD.1, digits=6)
      WD.2.avg= round(WD.2.avg, digits=6)
      WD.2.min= round(WD.2.min, digits=6)
      WD.2.max=round(WD.2.max, digits=6)
       
      wood.sub.25 = round(wood.sub.25,digits=6)
      wood.sub.50 = round(wood.sub.50,digits=6)
      wood.sub.75 = round(wood.sub.75,digits=6)
      wood.sub.100 = round(wood.sub.100,digits=6)
      
      mill.residues.energy.sub.50 = round(mill.residues.energy.sub.50, digits=6)
      mill.residues.energy.sub.75 = round(mill.residues.energy.sub.75, digits=6)
      
      boardman.wc.sub = round(boardman.wc.sub, digits=6)
      boardman.torr.sub = round(boardman.torr.sub, digits=6)
      Tot.TorrEmis.TgCyr = round(Tot.TorrEmis.TgCyr, digits=6)
      Tot.WCEmis.TgCyr = round(Tot.WCEmis.TgCyr, digits= 6)
      Torr.combust.E = round(Torr.combust.E, digits =6)
      WC.combust.E = round(WC.combust.E, digits = 6)
      
      Residues.forest.total = round(Residues.forest.total, digits=6)
      Residues.forest.decay.emitted = round(Residues.forest.decay.emitted, digits = 6)
      Residues.forest.combust.emitted = round(Residues.forest.combust.emitted, digits = 6)
      
      Merch.TgCyr = round(Merch.TgCyr.sw,digits=6)
      
      ####write SCEN lines##############      
     
      LCA.Annual<-rbind(LCA.Annual,c("ID" = ID,"Region"=Region,"Year"= year,"HarvestTotal" = TotABC.TgCyr,"HarvestMerch" = Merch.TgCyr,"Residues" = Residues.TgCyr,
                                      "Residues.forest.total" = Residues.forest.total, "Residues.forest.decay.emitted" = Residues.forest.decay.emitted, "Residues.forest.combust.emitted" = Residues.forest.combust.emitted,
                                     "TotWoodProducts.new" = TotWoodProd.new, "TotWoodProd.avg"=TotWoodProd.avg,"TotWoodProd.min"=TotWoodProd.min, "TotWoodProd.max"=TotWoodProd.max,
                                     "TotWoodProducts.ccm.avg" = TotWoodProd.ccm.avg, "TotWoodProd.ccm.min" = TotWoodProd.ccm.min, "TotWoodProd.ccm.max" = TotWoodProd.ccm.max,
                                     "Harvest.FFE"= harv.E, "Trans.FFE"= TotTransEmis.TgCyr, "Manu.FFE"= TotManuEmis.TgCyr,"TotProdEmis.TgCyr"=TotProdEmis.TgCyr, 
                                     "WD1"=WD.1, "WD2.avg"=WD.2.avg,"WD2.min"=WD.2.min,"WD2.max"=WD.2.max,"WD.2.ccm.avg" = WD.2.ccm.avg, "WD.2.ccm.min" = WD.2.ccm.min, "WD.2.ccm.max" = WD.2.ccm.max,
                                     "Landfill.Perm.wood.avg" = landfill.permanent.wood.avg, "Landfill.perm.paper.avg"= landfill.permanent.paper.avg,"Landfill.perm.wood.min" = landfill.permanent.wood.min,"Landfill.perm.paper.min"=landfill.permanent.paper.avg, "Landfill.perm.wood.max"=landfill.permanent.wood.max,"landfill.perm.paper.max"=landfill.permanent.paper.max,
                                     "Landfill.emissions.avg" = landfill.wood.emissions.avg+landfill.paper.emissions.avg, "Landfill.emissions.min"=landfill.paper.emissions.min+landfill.wood.emissions.min, "Landfill.emissions.max"=landfill.paper.emissions.max+landfill.wood.emissions.max,
                                     "Landfill.perm.wood.ccm.avg"= landfill.permanent.wood.ccm.avg, "Landfill.perm.wood.ccm.min" = landfill.permanent.wood.ccm.min, "Landfill.perm.wood.ccm.max" = landfill.permanent.wood.ccm.max,
                                     "Landfill.emissions.ccm.avg" = landfill.wood.emissions.ccm.avg +landfill.paper.emissions.avg, "Landfill.emissions.ccm.min" = landfill.wood.emissions.ccm.min+landfill.paper.emissions.min, "Landfill.emissions.ccm.max" = landfill.wood.emissions.ccm.max+landfill.paper.emissions.max,
                                     "WoodProduct.Sub.25" = wood.sub.25, "WoodProduct.Sub.50" = wood.sub.50, "WoodProduct.Sub.75" = wood.sub.75, "WoodProduct.Sub.100" = wood.sub.100,
                                     "Mill.energy.sub.50" = mill.residues.energy.sub.50, "Mill.energy.sub.75"=mill.residues.energy.sub.75,
                                    "Tot.TorrEmis"=Tot.TorrEmis.TgCyr,"Tot.WCEmis"=Tot.WCEmis.TgCyr,"Torr.combust.E"=Torr.combust.E,"WC.combust.E"=WC.combust.E,
                                    "Boardman.wc.sub"=boardman.wc.sub, "Boardman.torr.sub"=boardman.torr.sub))
      LCA.Annual.2<-rbind(LCA.Annual.2,c("ID" = ID,"Region"=Region,"Year"= year,"HarvestTotal" = TotABC.TgCyr,"HarvestMerch" = Merch.TgCyr, "Totwoodprod.new"=TotWoodProd.new,
                                     "TotWoodProducts.sf.new" = TotWoodProd.singlefamily.new,"SingleFamily.avg"= TotWoodProd.singlefamily.avg, "SingleFamily.min"=TotWoodProd.singlefamily.min, "SingleFamily.max"=TotWoodProd.singlefamily.max,
                                     "WD1"=WD.1, "WD2.sf.old.avg"=WD.2.wood.singlefamily.avg ,"WD2.sf.old.min"=WD.2.wood.singlefamily.min,"WD2.sf.old.max"=WD.2.wood.singlefamily.max,
                                     "sf.short.avg" = TotWoodProd.sf.short.avg.all, "sf.long.avg" = TotWoodProd.sf.long.avg.all, "wd.sf.short.avg" = WD.2.wood.sf.short.avg, "wd.sf.long.avg" = WD.2.wood.sf.long.avg,
                                    "sf.short.min" = TotWoodProd.sf.short.min.all, "sf.long.min" = TotWoodProd.sf.long.min.all, "wd.sf.short.min" = WD.2.wood.sf.short.min, "wd.sf.long.min" = WD.2.wood.sf.long.min,
                                    "sf.short.max" = TotWoodProd.sf.short.max.all, "sf.long.max" = TotWoodProd.sf.long.max.all, "wd.sf.short.max" = WD.2.wood.sf.short.max, "wd.sf.long.max" = WD.2.wood.sf.long.max,
                                    "mf.short.avg" = TotWoodProd.mf.short.avg.all, "mf.long.avg" = TotWoodProd.mf.long.avg.all, "wd.mf.short.avg" = WD.2.wood.mf.short.avg, "wd.mf.long.avg" = WD.2.wood.mf.long.avg,
                                    "mf.short.min" = TotWoodProd.mf.short.min.all, "mf.long.min" = TotWoodProd.mf.long.min.all, "wd.mf.short.min" = WD.2.wood.mf.short.min, "wd.mf.long.min" = WD.2.wood.mf.long.min,
                                    "mf.short.max" = TotWoodProd.mf.short.max.all, "mf.long.max" = TotWoodProd.mf.long.max.all, "wd.mf.short.max" = WD.2.wood.mf.short.max, "wd.mf.long.max" = WD.2.wood.mf.long.max,
                                    "nr.short.avg" = TotWoodProd.nr.short.avg.all, "nr.long.avg" = TotWoodProd.nr.long.avg.all, "wd.nr.short.avg" = WD.2.wood.nr.short.avg, "wd.nr.long.avg" = WD.2.wood.nr.long.avg,
                                    "nr.short.min" = TotWoodProd.nr.short.min.all, "nr.long.min" = TotWoodProd.nr.long.min.all, "wd.nr.short.min" = WD.2.wood.nr.short.min, "wd.nr.long.min" = WD.2.wood.nr.long.min,
                                    "nr.short.max" = TotWoodProd.nr.short.max.all, "nr.long.max" = TotWoodProd.nr.long.max.all, "wd.nr.short.max" = WD.2.wood.nr.short.max, "wd.nr.long.max" = WD.2.wood.nr.long.max))
      
      
      x = x + 1 # next row in file
      
} # end rows


# Write file to c drive   ##!!
write.table(LCA.Annual,file =paste0(directory,"\\",file.name.out.1),sep = ",", quote = FALSE,col.names = TRUE, row.names = FALSE)
write.table(LCA.Annual.2,file =paste0(directory,"\\",file.name.out.2),sep = ",", quote = FALSE,col.names = TRUE, row.names = FALSE)


