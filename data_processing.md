Data Processing
================

``` r
county_fips = read_csv("./data/all-geocodes-v2016.csv", skip = 4) %>%
  janitor::clean_names() %>%
  rename(county_name = area_name_including_legal_statistical_area_description) %>%
  filter(county_code_fips != "000") %>%
  mutate(state_county_fips = paste0(state_code_fips, county_code_fips)) %>%
  select (state_code_fips, county_code_fips, state_county_fips, county_name)
```

    ## Parsed with column specification:
    ## cols(
    ##   `Summary Level` = col_character(),
    ##   `State Code (FIPS)` = col_character(),
    ##   `County Code (FIPS)` = col_character(),
    ##   `County Subdivision Code (FIPS)` = col_character(),
    ##   `Place Code (FIPS)` = col_character(),
    ##   `Consolidtated City Code (FIPS)` = col_character(),
    ##   `Area Name (including legal/statistical area description)` = col_character()
    ## )

``` r
state_fips = read_csv("./data/state-geocodes-v2016.csv", skip = 5) %>%
  janitor::clean_names() %>%
  filter(state_fips != "00") %>%
  select(state_fips, name) %>%
  mutate(state_fips = as.numeric(state_fips)) %>%
  rename(state = name)
```

    ## Parsed with column specification:
    ## cols(
    ##   Region = col_double(),
    ##   Division = col_double(),
    ##   `State (FIPS)` = col_character(),
    ##   Name = col_character()
    ## )

``` r
bee_county = read_csv("./data/bee_county/Bee_Colony_Census_Data_by_County 2.csv") %>% 
  janitor::clean_names() %>% 
  select(year, 
         state, 
         state_ansi, 
         ag_district, 
         ag_district_code, 
         county, 
         county_ansi, 
         value, 
         cv_percent) 
```

    ## Parsed with column specification:
    ## cols(
    ##   Year = col_double(),
    ##   Period = col_character(),
    ##   State = col_character(),
    ##   `State ANSI` = col_double(),
    ##   `Ag District` = col_character(),
    ##   `Ag District Code` = col_double(),
    ##   County = col_character(),
    ##   `County ANSI` = col_double(),
    ##   Value = col_character(),
    ##   `CV (%)` = col_character()
    ## )

``` r
 bee_county$state_ansi = stringr::str_pad(bee_county$state_ansi,2 , pad = "0")
bee_county$county_ansi = stringr::str_pad(bee_county$county_ansi,3 , pad = "0")

bee_county = bee_county%>% 
  mutate(state_county_fips = paste0(state_ansi, county_ansi),
    value = replace(value, value == "(D)", "NA"),
    cv_percent = replace(cv_percent, cv_percent == "(D)", "NA"),
    colony_count = value)
```

``` r
#create function to import data
file_name <- list.files(path = "./data/bee_state_2/") 

df = read_csv(file = str_c("./data/bee_state_2/", file_name[1])) %>%
  mutate(file = file_name[1])
```

    ## Parsed with column specification:
    ## cols(
    ##   `2` = col_double(),
    ##   t = col_character(),
    ##   `Honey: Released February 28, 2002, by the National Agricultural Statistics Service (NASS), Agricultural Statistics Board, U.S. Department of Agriculture.` = col_character(),
    ##   X4 = col_character(),
    ##   X5 = col_character(),
    ##   X6 = col_character(),
    ##   X7 = col_character(),
    ##   X8 = col_character(),
    ##   X9 = col_character()
    ## )

``` r
  df
```

    ## # A tibble: 51 x 10
    ##      `2` t     `Honey: Released … X4    X5    X6    X7    X8    X9    file 
    ##    <dbl> <chr> <chr>              <chr> <chr> <chr> <chr> <chr> <chr> <chr>
    ##  1     2 t     Honey:  Number of… <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  2002…
    ##  2     2 t     and Value by Stat… <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  2002…
    ##  3     2 h     <NA>               <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  2002…
    ##  4     2 h     <NA>               Honey Yield <NA>  <NA>  Aver… Value 2002…
    ##  5     2 h     State              Prod… per   Prod… Stoc… Pric… of    2002…
    ##  6     2 h     <NA>               Colo… Colo… <NA>  Dec … Poun… Prod… 2002…
    ##  7     2 h     <NA>               <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  2002…
    ##  8     2 u     <NA>               1,000 Poun… 1,00… 1,00… Cents 1,00… 2002…
    ##  9     2 d     AL                 16    78    1248  187   59    736   2002…
    ## 10     2 d     AZ                 40    59    2360  1322  73    1723  2002…
    ## # … with 41 more rows

``` r
my_read_csv = function(x){
    df = read_csv(x, skip = 9, col_names  = F)
}

my_read_csv = function(x){
    df = read_csv(x, skip = 9, col_names  = F)
}
  
bee_data = 
  tibble(
    file_names = file_name,
    path = str_c("./data/bee_state_2/", file_names)
  ) %>% 
  mutate(data = map(path, my_read_csv))%>% 
  unnest()
```

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   X2 = col_character(),
    ##   X3 = col_character(),
    ##   X4 = col_double(),
    ##   X5 = col_double(),
    ##   X6 = col_double(),
    ##   X7 = col_double(),
    ##   X8 = col_double(),
    ##   X9 = col_double()
    ## )

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   X2 = col_character(),
    ##   X3 = col_character(),
    ##   X4 = col_double(),
    ##   X5 = col_double(),
    ##   X6 = col_double(),
    ##   X7 = col_double(),
    ##   X8 = col_double(),
    ##   X9 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   X2 = col_character(),
    ##   X3 = col_character(),
    ##   X4 = col_double(),
    ##   X5 = col_double(),
    ##   X6 = col_double(),
    ##   X7 = col_double(),
    ##   X8 = col_double(),
    ##   X9 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   X2 = col_character(),
    ##   X3 = col_character(),
    ##   X4 = col_double(),
    ##   X5 = col_double(),
    ##   X6 = col_double(),
    ##   X7 = col_double(),
    ##   X8 = col_double(),
    ##   X9 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   X2 = col_character(),
    ##   X3 = col_character(),
    ##   X4 = col_double(),
    ##   X5 = col_double(),
    ##   X6 = col_double(),
    ##   X7 = col_double(),
    ##   X8 = col_double(),
    ##   X9 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   X2 = col_character(),
    ##   X3 = col_character(),
    ##   X4 = col_double(),
    ##   X5 = col_double(),
    ##   X6 = col_double(),
    ##   X7 = col_double(),
    ##   X8 = col_double(),
    ##   X9 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   X2 = col_character(),
    ##   X3 = col_character(),
    ##   X4 = col_double(),
    ##   X5 = col_double(),
    ##   X6 = col_double(),
    ##   X7 = col_double(),
    ##   X8 = col_double(),
    ##   X9 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   X2 = col_character(),
    ##   X3 = col_character(),
    ##   X4 = col_double(),
    ##   X5 = col_double(),
    ##   X6 = col_double(),
    ##   X7 = col_double(),
    ##   X8 = col_double(),
    ##   X9 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   X2 = col_character(),
    ##   X3 = col_character(),
    ##   X4 = col_double(),
    ##   X5 = col_double(),
    ##   X6 = col_double(),
    ##   X7 = col_double(),
    ##   X8 = col_double(),
    ##   X9 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   X2 = col_character(),
    ##   X3 = col_character(),
    ##   X4 = col_double(),
    ##   X5 = col_double(),
    ##   X6 = col_double(),
    ##   X7 = col_double(),
    ##   X8 = col_double(),
    ##   X9 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   X2 = col_character(),
    ##   X3 = col_character(),
    ##   X4 = col_double(),
    ##   X5 = col_double(),
    ##   X6 = col_double(),
    ##   X7 = col_double(),
    ##   X8 = col_double(),
    ##   X9 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   X2 = col_character(),
    ##   X3 = col_character(),
    ##   X4 = col_double(),
    ##   X5 = col_double(),
    ##   X6 = col_double(),
    ##   X7 = col_double(),
    ##   X8 = col_double(),
    ##   X9 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   X2 = col_character(),
    ##   X3 = col_character(),
    ##   X4 = col_double(),
    ##   X5 = col_double(),
    ##   X6 = col_double(),
    ##   X7 = col_double(),
    ##   X8 = col_double(),
    ##   X9 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   X2 = col_character(),
    ##   X3 = col_character(),
    ##   X4 = col_double(),
    ##   X5 = col_double(),
    ##   X6 = col_double(),
    ##   X7 = col_double(),
    ##   X8 = col_double(),
    ##   X9 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   X2 = col_character(),
    ##   X3 = col_character(),
    ##   X4 = col_double(),
    ##   X5 = col_double(),
    ##   X6 = col_double(),
    ##   X7 = col_double(),
    ##   X8 = col_double(),
    ##   X9 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   X2 = col_character(),
    ##   X3 = col_character(),
    ##   X4 = col_double(),
    ##   X5 = col_double(),
    ##   X6 = col_double(),
    ##   X7 = col_double(),
    ##   X8 = col_double(),
    ##   X9 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   X2 = col_character(),
    ##   X3 = col_character(),
    ##   X4 = col_double(),
    ##   X5 = col_double(),
    ##   X6 = col_double(),
    ##   X7 = col_double(),
    ##   X8 = col_double(),
    ##   X9 = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   X2 = col_character(),
    ##   X3 = col_character(),
    ##   X4 = col_double(),
    ##   X5 = col_double(),
    ##   X6 = col_double(),
    ##   X7 = col_double(),
    ##   X8 = col_double(),
    ##   X9 = col_double()
    ## )

``` r
#clean data set
clean_bee_data =
  bee_data %>%
 separate(file_names, into = c("year", "remove"), sep = ".c") %>%
  select(-remove, -X1, -X2, -path) %>%
  rename(state = X3, honey_producing_colonies = X4, yield_per_colony = X5, production = X6, stocks = X7, price_per_pound = X8, production_value = X9) %>%
  drop_na(state) %>%
  mutate(state = recode(state, AL = "Alabama", AR = "Arkansas", AZ = "Arizona", CA = "California", CO = "Colorado", FL = "Florida", GA = "Georgia", HI = "Hawaii", IA = "Iowa", IL = "Illinois", ID = "Idaho", IN = "Indiana", KS = "Kansas", KY = "Kentucky", LA = "Louisiana", ME = "Maine", MD = "Maryland", MI = "Michigan", MN = "Minnesota", MO = "Missouri", MT = "Montana", MS = "Mississippi", NC = "North Carolina", ND = "North Dakota", NE = "Nebraska", NJ = "New Jersey", NM = "New Mexico", NV = "Nevada", NY = "New York", OH = "Ohio", OK = "Oklahoma", OR = "Oregon", PA = "Pennsylvania", SC = "South Carolina", SD = "South Dakota", TN = "Tennessee", TX = "Texas", UT = "Utah", VA = "Virginia", VT = "Vermont", WA = "Washington", WV = "West Virginia", WI = "Wisconsin", WY = "Wyoming"  ))

clean_bee_data %>%
  ggplot(aes(x = year, y = honey_producing_colonies, color = state)) +
  geom_point()
```

<img src="data_processing_files/figure-gfm/state bee colonies-1.png" width="90%" />

``` r
#Converting txt files to csv 

directory <- "./data/pesticides"
ndirectory <- "./data/pesticides_csv"

file_name <- list.files(directory, pattern = ".txt")

files.to.read <- paste(directory, file_name, sep="/")
files.to.write <- paste(ndirectory, paste0(sub(".txt","", file_name),".csv"), sep="/")

for (i in 1:length(files.to.read)) {
  temp <- (read.csv(files.to.read[i], header = TRUE))
  write.csv(temp, file = files.to.write[i])
}

#Only worked for 2002 to 2012 - need to check why I am getting an error message for 2013+
```

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
    
pest_2002 %>% 
  group_by(year, state_county_fips, compound) %>% 
  ggplot(aes(x = compound, y = epest_high_kg)) + 
  geom_col() 
```

<img src="data_processing_files/figure-gfm/pesticide data-1.png" width="90%" />

``` r
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

``` r
state_bee_fips = full_join(clean_bee_data, state_fips, by = "state")

top_pesticides = read_csv("./data/top_pesticides.csv") %>%
    mutate(state_fips = as.numeric(state_fips))
```

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   compound = col_character(),
    ##   year = col_double(),
    ##   epest_low_kg = col_double(),
    ##   epest_high_kg = col_double(),
    ##   state_fips = col_character(),
    ##   county_fips = col_character(),
    ##   state_county_fips = col_character()
    ## )

``` r
state_pest_data = 
  top_pesticides %>%
  group_by(state_fips, year, compound) %>%
  summarize(low = sum(epest_low_kg), high = sum(epest_high_kg)) 
```

``` r
state_bee_yrfips = 
  state_bee_fips %>%
  mutate(yearfips = paste0(year, state_fips))

state_pest_yrfips = 
  state_pest_data %>%
  mutate(yearfips = paste0(year, state_fips))

merged_state_data = full_join(state_bee_yrfips, state_pest_yrfips, by = "yearfips") %>%
  select(year.x, state, state_fips.x, honey_producing_colonies, yield_per_colony, production, compound, low, high) %>%
  drop_na(year.x, state, compound, high)
```

``` r
county_bee_yrfips = 
  bee_county %>%
  mutate(yearfips = paste0(year, state_county_fips))
         
county_pest_yrfips = 
  top_pesticides %>%
  mutate(state_county_fips = as.numeric(state_county_fips) ,
         yearfips = paste0(year, state_county_fips))

merged_county_data = full_join(county_bee_yrfips, county_pest_yrfips, by = "yearfips") %>%
  select(year.x, state, county, county_fips, colony_count, compound, epest_low_kg, epest_high_kg) %>%
  drop_na(year.x, state, county, colony_count, compound, epest_high_kg)
```
