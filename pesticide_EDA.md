Pesticides\_Data
================
Maya Spaur
11/14/2019

This document is to check to see if our pesticides of interest are in
the dataset.

\#FINDINGS: All 6 pesticides, “CHLOROTHALONIL”,“CHLORPYRIFOS”
“CLOTHIANIDIN”, “FIPRONIL”,“IMIDACLOPRID”,and “THIACLOPRID” have data
from 2004-2016. Only 4 pesticides, “CHLOROTHALONIL”,“CHLORPYRIFOS”,
“FIPRONIL”,“IMIDACLOPRID” had data from 2002-2003.

``` r
library(tidyverse)
```

    ## -- Attaching packages ----------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.1     v purrr   0.3.3
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   1.0.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts -------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(viridis)
```

    ## Loading required package: viridisLite

``` r
library(readxl)
```

copied from data\_processing document:

``` r
pest_2002 = read_excel("./data/pesticides_csv/EPest.county.estimates.2002.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate(
    state_fips = state_fips_code,
    county_fips = county_fips_code,
    state_county_fips = paste0(state_fips, county_fips),
    epest_low_kg = round(epest_low_kg),
    epest_high_kg = round(epest_high_kg)) %>%
  select(-state_fips_code, -county_fips_code)
    

unique(pull(pest_2002, compound))
```

    ##   [1] "2,4-D"                   "2,4-DB"                 
    ##   [3] "6-BENZYLADENINE"         "ABAMECTIN"              
    ##   [5] "ACEPHATE"                "ACETAMIPRID"            
    ##   [7] "ACETOCHLOR"              "ACIBENZOLAR"            
    ##   [9] "ACIFLUORFEN"             "ALACHLOR"               
    ##  [11] "ALDICARB"                "ALUMINUM PHOSPHIDE"     
    ##  [13] "AMETRYN"                 "AMITRAZ"                
    ##  [15] "AMPELOMYCES QUISQUALIS"  "ANILAZINE"              
    ##  [17] "ASULAM"                  "ATRAZINE"               
    ##  [19] "AVIGLYCINE"              "AZADIRACHTIN"           
    ##  [21] "AZINPHOS-METHYL"         "AZOXYSTROBIN"           
    ##  [23] "BACILLUS CEREUS"         "BACILLUS SUBTILIS"      
    ##  [25] "BACILLUS THURINGIENSIS"  "BENFLURALIN"            
    ##  [27] "BENOMYL"                 "BENSULFURON"            
    ##  [29] "BENSULIDE"               "BENTAZONE"              
    ##  [31] "BIFENAZATE"              "BIFENTHRIN"             
    ##  [33] "BISPYRIBAC"              "BORDEAUX MIXTURE"       
    ##  [35] "BROMACIL"                "BROMOXYNIL"             
    ##  [37] "BUPROFEZIN"              "BUTRALIN"               
    ##  [39] "BUTYLATE"                "CALCIUM CHLORIDE"       
    ##  [41] "CALCIUM POLYSULFIDE"     "CAPTAN"                 
    ##  [43] "CARBARYL"                "CARBOFURAN"             
    ##  [45] "CARBOXIN"                "CARFENTRAZONE-ETHYL"    
    ##  [47] "CHINOMETHIONAT"          "CHLORETHOXYFOS"         
    ##  [49] "CHLORFENAPYR"            "CHLORIDAZON"            
    ##  [51] "CHLORIMURON"             "CHLORMEQUAT"            
    ##  [53] "CHLOROPICRIN"            "CHLOROTHALONIL"         
    ##  [55] "CHLOROXURON"             "CHLORPROPHAM"           
    ##  [57] "CHLORPYRIFOS"            "CHLORSULFURON"          
    ##  [59] "CINNAMALDEHYDE"          "CLETHODIM"              
    ##  [61] "CLODINAFOP"              "CLOFENTEZINE"           
    ##  [63] "CLOMAZONE"               "CLOPYRALID"             
    ##  [65] "CLORANSULAM-METHYL"      "COPPER"                 
    ##  [67] "COPPER HYDROXIDE"        "COPPER OXYCHLORIDE"     
    ##  [69] "COPPER OXYCHLORIDE S"    "COPPER SULF TRIBASIC"   
    ##  [71] "COPPER SULFATE"          "CPPU"                   
    ##  [73] "CRYOLITE"                "CUPROUS OXIDE"          
    ##  [75] "CYANAMIDE"               "CYANAZINE"              
    ##  [77] "CYCLANILIDE"             "CYCLOATE"               
    ##  [79] "CYFLUTHRIN"              "CYHALOFOP"              
    ##  [81] "CYHALOTHRIN-LAMBDA"      "CYMOXANIL"              
    ##  [83] "CYPERMETHRIN"            "CYPRODINIL"             
    ##  [85] "CYROMAZINE"              "CYTOKININ"              
    ##  [87] "DAMINOZIDE"              "DAZOMET"                
    ##  [89] "DCPA"                    "DECAN-1-OL"             
    ##  [91] "DELTAMETHRIN"            "DESMEDIPHAM"            
    ##  [93] "DIAZINON"                "DICAMBA"                
    ##  [95] "DICHLOBENIL"             "DICHLOROPROPENE"        
    ##  [97] "DICHLORPROP"             "DICLOFOP"               
    ##  [99] "DICLORAN"                "DICLOSULAM"             
    ## [101] "DICOFOL"                 "DICROTOPHOS"            
    ## [103] "DIENOCHLOR"              "DIETHATYL"              
    ## [105] "DIFENZOQUAT"             "DIFLUBENZURON"          
    ## [107] "DIFLUFENZOPYR"           "DIMETHENAMID"           
    ## [109] "DIMETHENAMID-P"          "DIMETHIPIN"             
    ## [111] "DIMETHOATE"              "DIMETHOMORPH"           
    ## [113] "DIMETHYLARSINIC ACID"    "DINOSEB"                
    ## [115] "DIQUAT"                  "DISULFOTON"             
    ## [117] "DIURON"                  "DODINE"                 
    ## [119] "DSMA"                    "EMAMECTIN"              
    ## [121] "ENDOSULFAN"              "ENDOTHAL"               
    ## [123] "EPTC"                    "ESFENVALERATE"          
    ## [125] "ETHALFLURALIN"           "ETHEPHON"               
    ## [127] "ETHION"                  "ETHOFUMESATE"           
    ## [129] "ETHOPROPHOS"             "ETRIDIAZOLE"            
    ## [131] "FATTY ALCOHOLS"          "FENAMIPHOS"             
    ## [133] "FENARIMOL"               "FENBUCONAZOLE"          
    ## [135] "FENBUTATIN OXIDE"        "FENHEXAMID"             
    ## [137] "FENOXAPROP"              "FENOXYCARB"             
    ## [139] "FENPROPATHRIN"           "FENTIN"                 
    ## [141] "FENVALERATE"             "FERBAM"                 
    ## [143] "FIPRONIL"                "FLUAZIFOP"              
    ## [145] "FLUAZINAM"               "FLUCARBAZONE"           
    ## [147] "FLUDIOXONIL"             "FLUFENACET"             
    ## [149] "FLUMETRALIN"             "FLUMETSULAM"            
    ## [151] "FLUMICLORAC"             "FLUMIOXAZIN"            
    ## [153] "FLUOMETURON"             "FLUROXYPYR"             
    ## [155] "FLUTOLANIL"              "FLUVALINATE-TAU"        
    ## [157] "FOMESAFEN"               "FONOFOS"                
    ## [159] "FORAMSULFURON"           "FORMETANATE"            
    ## [161] "FOSAMINE"                "FOSETYL"                
    ## [163] "GAMMA AMINOBUTYRIC ACID" "GARLIC JUICE"           
    ## [165] "GIBBERELLIC ACID"        "GLUFOSINATE"            
    ## [167] "GLYPHOSATE"              "HALOSULFURON"           
    ## [169] "HARPIN PROTEIN"          "HEXAZINONE"             
    ## [171] "HEXYTHIAZOX"             "HYDRAMETHYLNON"         
    ## [173] "HYDRATED LIME"           "HYDROGEN PEROXIDE"      
    ## [175] "HYDROXYPROPANOIC ACID"   "IBA"                    
    ## [177] "IMAZAMETHABENZ"          "IMAZAMOX"               
    ## [179] "IMAZAPIC"                "IMAZAPYR"               
    ## [181] "IMAZAQUIN"               "IMAZETHAPYR"            
    ## [183] "IMIDACLOPRID"            "INDOLYL-BUTYRIC ACID"   
    ## [185] "INDOXACARB"              "IPRODIONE"              
    ## [187] "ISOXABEN"                "ISOXAFLUTOLE"           
    ## [189] "KAOLIN CLAY"             "KINOPRENE"              
    ## [191] "KRESOXIM-METHYL"         "L-GLUTAMIC ACID"        
    ## [193] "LACTOFEN"                "LINDANE"                
    ## [195] "LINURON"                 "MALATHION"              
    ## [197] "MALEIC HYDRAZIDE"        "MANCOZEB"               
    ## [199] "MANEB"                   "MCPA"                   
    ## [201] "MCPB"                    "MECOPROP"               
    ## [203] "MEFENOXAM"               "MEFLUIDIDE"             
    ## [205] "MEPIQUAT"                "MESOTRIONE"             
    ## [207] "METALAXYL"               "METALDEHYDE"            
    ## [209] "METAM"                   "METAM POTASSIUM"        
    ## [211] "METHAMIDOPHOS"           "METHIDATHION"           
    ## [213] "METHIOCARB"              "METHOMYL"               
    ## [215] "METHOXYCHLOR"            "METHOXYFENOZIDE"        
    ## [217] "METHYL BROMIDE"          "METHYL PARATHION"       
    ## [219] "METIRAM"                 "METOLACHLOR"            
    ## [221] "METOLACHLOR-S"           "METRIBUZIN"             
    ## [223] "METSULFURON"             "MEVINPHOS"              
    ## [225] "MOLINATE"                "MSMA"                   
    ## [227] "MYCLOBUTANIL"            "MYROTHECIUM VERRUCARIA" 
    ## [229] "NALED"                   "NAPHTHYLACETAMIDE"      
    ## [231] "NAPHTHYLACETIC ACID"     "NAPROPAMIDE"            
    ## [233] "NAPTALAM"                "NAPTHA"                 
    ## [235] "NEEM OIL"                "NICOSULFURON"           
    ## [237] "NORFLURAZON"             "NOVALURON"              
    ## [239] "ORYZALIN"                "OXADIAZON"              
    ## [241] "OXAMYL"                  "OXYDEMETON-METHYL"      
    ## [243] "OXYFLUORFEN"             "OXYTETRACYCLINE"        
    ## [245] "PACLOBUTRAZOL"           "PARAQUAT"               
    ## [247] "PARATHION"               "PEBULATE"               
    ## [249] "PELARGONIC ACID"         "PENDIMETHALIN"          
    ## [251] "PERMETHRIN"              "PETROLEUM DISTILLATE"   
    ## [253] "PETROLEUM OIL"           "PHENMEDIPHAM"           
    ## [255] "PHORATE"                 "PHOSMET"                
    ## [257] "PHOSPHORIC ACID"         "PICLORAM"               
    ## [259] "PINOLENE"                "PIPERONYL BUTOXIDE"     
    ## [261] "POTASSIUM BICARBONATE"   "POTASSIUM OLEATE"       
    ## [263] "PRIMISULFURON"           "PRODIAMINE"             
    ## [265] "PROFENOFOS"              "PROFLURALIN"            
    ## [267] "PROHEXADIONE"            "PROMETON"               
    ## [269] "PROMETRYN"               "PROPACHLOR"             
    ## [271] "PROPAMOCARB HCL"         "PROPANIL"               
    ## [273] "PROPARGITE"              "PROPICONAZOLE"          
    ## [275] "PROPYZAMIDE"             "PROSULFURON"            
    ## [277] "PSEUDOMONAS FLUORESCENS" "PYMETROZINE"            
    ## [279] "PYRACLOSTROBIN"          "PYRETHRINS"             
    ## [281] "PYRIDABEN"               "PYRIDATE"               
    ## [283] "PYRIPROXYFEN"            "PYRITHIOBAC-SODIUM"     
    ## [285] "QUINCLORAC"              "QUINTOZENE"             
    ## [287] "QUIZALOFOP"              "RIMSULFURON"            
    ## [289] "ROTENONE"                "SABADILLA"              
    ## [291] "SETHOXYDIM"              "SILICATES"              
    ## [293] "SIMAZINE"                "SODIUM CHLORATE"        
    ## [295] "SODIUM METABORATE"       "SPINOSYN"               
    ## [297] "STEINERNEMA CARPOCAP"    "STEINERNEMA RIOBRAVI"   
    ## [299] "STREPTOMYCIN"            "SULFCARBAMIDE"          
    ## [301] "SULFENTRAZONE"           "SULFOMETURON"           
    ## [303] "SULFOSATE"               "SULFOSULFURON"          
    ## [305] "SULFUR"                  "SULFURIC ACID"          
    ## [307] "TEBUCONAZOLE"            "TEBUFENOZIDE"           
    ## [309] "TEBUPIRIMPHOS"           "TEBUTHIURON"            
    ## [311] "TEFLUTHRIN"              "TERBACIL"               
    ## [313] "TERBUFOS"                "TETRABOROHYDRATE"       
    ## [315] "TETRACONAZOLE"           "TETRAOXOSULFATE"        
    ## [317] "TETRATHIOCARBONATE"      "THIABENDAZOLE"          
    ## [319] "THIAMETHOXAM"            "THIAZOPYR"              
    ## [321] "THIDIAZURON"             "THIFENSULFURON"         
    ## [323] "THIOBENCARB"             "THIODICARB"             
    ## [325] "THIOPHANATE-METHYL"      "THIRAM"                 
    ## [327] "TRALKOXYDIM"             "TRALOMETHRIN"           
    ## [329] "TRI-ALLATE"              "TRIADIMEFON"            
    ## [331] "TRIASULFURON"            "TRIBENURON METHYL"      
    ## [333] "TRIBUFOS"                "TRICLOPYR"              
    ## [335] "TRIFLOXYSTROBIN"         "TRIFLUMIZOLE"           
    ## [337] "TRIFLURALIN"             "TRIFLUSULFURON"         
    ## [339] "TRIFORINE"               "TRINEXAPAC"             
    ## [341] "VINCLOZOLIN"             "ZETA-CYPERMETHRIN"      
    ## [343] "ZINC"                    "ZINEB"                  
    ## [345] "ZIRAM"                   "ZOXAMIDE"

Identifying if pesticides are in the datasets:

``` r
pest_2002 = read_excel("./data/pesticides_csv/EPest.county.estimates.2003.xlsx") %>%
  janitor::clean_names() %>% 
  filter(compound == "CHLOROTHALONIL" | compound == "CLOTHIANIDIN"| compound == "CHLORPYRIFOS" | compound == "FIPRONIL" | compound == "IMIDACLOPRID"| compound ==  "THIACLOPRID")

unique(pull(pest_2002, compound))
```

    ## [1] "CHLOROTHALONIL" "CHLORPYRIFOS"   "FIPRONIL"       "IMIDACLOPRID"

``` r
pest_2003 = read_excel("./data/pesticides_csv/EPest.county.estimates.2003.xlsx") %>%
  janitor::clean_names() %>% 
  filter(compound == "CHLOROTHALONIL" | compound == "CLOTHIANIDIN"| compound == "CHLORPYRIFOS" | compound == "FIPRONIL" | compound == "IMIDACLOPRID"| compound ==  "THIACLOPRID")

unique(pull(pest_2003, compound))
```

    ## [1] "CHLOROTHALONIL" "CHLORPYRIFOS"   "FIPRONIL"       "IMIDACLOPRID"

\#just change the file name to change the year

``` r
pest_ = read_excel("./data/pesticides_csv/EPest.county.estimates.2015.xlsx") %>%
  janitor::clean_names() %>% 
  filter(compound == "CHLOROTHALONIL" | compound == "CLOTHIANIDIN"| compound == "CHLORPYRIFOS" | compound == "FIPRONIL" | compound == "IMIDACLOPRID"| compound ==  "THIACLOPRID")

unique(pull(pest_, compound))
```

    ## [1] "CHLOROTHALONIL" "CHLORPYRIFOS"   "CLOTHIANIDIN"   "FIPRONIL"      
    ## [5] "IMIDACLOPRID"   "THIACLOPRID"
