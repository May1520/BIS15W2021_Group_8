---
title: "Prof ledford app"
author: "me"
date: "3/7/2021"
output: 
  html_document: 
    keep_md: yes
---





You should only load the libraries that you will use as part of your code. I removed a few and added lubridate.

```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.3.3     v purrr   0.3.4
## v tibble  3.0.5     v dplyr   1.0.3
## v tidyr   1.1.2     v stringr 1.4.0
## v readr   1.4.0     v forcats 0.5.0
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(janitor)
```

```
## 
## Attaching package: 'janitor'
```

```
## The following objects are masked from 'package:stats':
## 
##     chisq.test, fisher.test
```

```r
library(shiny)
library(shinydashboard)
```

```
## 
## Attaching package: 'shinydashboard'
```

```
## The following object is masked from 'package:graphics':
## 
##     box
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```


```r
options(scipen = 999)
```


```r
library(here)
```

```
## here() starts at C:/Users/manha/Desktop/BIS15W2021_Group_8
```

Let's use here to import the data. You can clean the names as part of the import.

```r
covid <- read.csv("Data/WHO-COVID-19-global-data.csv") %>% clean_names()
```

Take care of the date issue first.

```r
covid$i_date_reported <- lubridate::mdy(covid$i_date_reported)
```

Convert all of the `int` columns to `dbl`

```r
covid <- covid %>% mutate_if(is.integer,as.numeric)
```


```r
glimpse(covid)
```

```
## Rows: 96,222
## Columns: 8
## $ i_date_reported   <date> 2020-01-03, 2020-01-04, 2020-01-05, 2020-01-06, ...
## $ country_code      <chr> "AF", "AF", "AF", "AF", "AF", "AF", "AF", "AF", "...
## $ country           <chr> "Afghanistan", "Afghanistan", "Afghanistan", "Afg...
## $ who_region        <chr> "EMRO", "EMRO", "EMRO", "EMRO", "EMRO", "EMRO", "...
## $ new_cases         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
## $ cumulative_cases  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
## $ new_deaths        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
## $ cumulative_deaths <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
```

Based on the way the data are organized, I don't think a scatterplot best represents the data. I think it would be more interesting to make a barplot where the x-axis is date and the y-axis allows users to select between the continuous variables. Then, country can be a filter variable. Have a look at the app below. I will leave the aesthetics up to you.

```r
covid <- covid %>% 
  select(i_date_reported, country, cumulative_cases, cumulative_deaths)

ui <- dashboardPage(skin="purple",
  dashboardHeader(title = "COVID Cumulative Plot"),
  dashboardSidebar(disable = T),
  dashboardBody(
  fluidRow(
  box(title = "Plot Options", width = 3,
  selectInput("fill", "Select Country", choices =unique(covid$country)),
  radioButtons("x", "Select X Variable", choices = c("cumulative_cases", "cumulative_deaths"), selected = "cumulative_cases"),
  box(title = "Disease Abundance", width = 9,
  plotOutput("plot", width = "800px", height = "500px", click="plot_click"),
  verbatimTextOutput("info")
  ) 
  ) 
  )
  ) 
  )

server <- function(input, output, session) { 
  
  output$plot <- renderPlot({
    covid %>% 
    filter(country==input$fill) %>% 
    ggplot(aes_string(x = "i_date_reported", y = input$x, fill=input$x)) + 
    geom_col()
  })
}

shinyApp(ui, server)
```



****************************************




Experiment- gradient color filled

```r
covid <- covid %>% 
  select(i_date_reported, country, cumulative_cases, cumulative_deaths)

ui <- dashboardPage(skin="purple",
  dashboardHeader(title = "COVID Cumulative Plot"),
  dashboardSidebar(disable = T),
  dashboardBody(
  fluidRow(
  box(title = "Plot Options", width = 3,
  selectInput("fill", "Select Country", choices =unique(covid$country)),
  radioButtons("x", "Select X Variable", choices = c("cumulative_cases", "cumulative_deaths"), selected = "cumulative_cases"),
  box(title = "Disease Abundance", width = 9,
  plotOutput("plot", width = "800px", height = "500px", click="plot_click"),
  verbatimTextOutput("info")
  ) 
  ) 
  )
  ) 
  )

server <- function(input, output, session) { 
  
  output$plot <- renderPlot({
    covid %>% 
    filter(country==input$fill) %>% 
    ggplot(aes_string(x = "i_date_reported", y = input$x, fill=input$x)) +
      geom_col()
  })
}

shinyApp(ui, server)
```




###PLOTS


```r
covid%>%
  filter(i_date_reported=="2021-02-11")%>%
  arrange(desc(cumulative_deaths))
```

```
##     i_date_reported country_code
## 1        2021-02-11           US
## 2        2021-02-11           BR
## 3        2021-02-11           MX
## 4        2021-02-11           IN
## 5        2021-02-11           GB
## 6        2021-02-11           IT
## 7        2021-02-11           FR
## 8        2021-02-11           RU
## 9        2021-02-11           ES
## 10       2021-02-11           DE
## 11       2021-02-11           IR
## 12       2021-02-11           CO
## 13       2021-02-11           AR
## 14       2021-02-11           ZA
## 15       2021-02-11           PE
## 16       2021-02-11           PL
## 17       2021-02-11           ID
## 18       2021-02-11           TR
## 19       2021-02-11           UA
## 20       2021-02-11           BE
## 21       2021-02-11           CA
## 22       2021-02-11           RO
## 23       2021-02-11           CL
## 24       2021-02-11           CZ
## 25       2021-02-11           EC
## 26       2021-02-11           PT
## 27       2021-02-11           NL
## 28       2021-02-11           HU
## 29       2021-02-11           IQ
## 30       2021-02-11           SE
## 31       2021-02-11           PK
## 32       2021-02-11           PH
## 33       2021-02-11           BO
## 34       2021-02-11           EG
## 35       2021-02-11           BG
## 36       2021-02-11           CH
## 37       2021-02-11           MA
## 38       2021-02-11           BD
## 39       2021-02-11           AT
## 40       2021-02-11           TN
## 41       2021-02-11           JP
## 42       2021-02-11           SA
## 43       2021-02-11           GR
## 44       2021-02-11           GT
## 45       2021-02-11           PA
## 46       2021-02-11           SK
## 47       2021-02-11           HR
## 48       2021-02-11           IL
## 49       2021-02-11           BA
## 50       2021-02-11           CN
## 51       2021-02-11           JO
## 52       2021-02-11           RS
## 53       2021-02-11           SI
## 54       2021-02-11           LB
## 55       2021-02-11           IE
## 56       2021-02-11           HN
## 57       2021-02-11           MD
## 58       2021-02-11           GE
## 59       2021-02-11           KZ
## 60       2021-02-11           MM
## 61       2021-02-11           AZ
## 62       2021-02-11           AM
## 63       2021-02-11           LT
## 64       2021-02-11           MK
## 65       2021-02-11           DZ
## 66       2021-02-11           DO
## 67       2021-02-11           PY
## 68       2021-02-11           CR
## 69       2021-02-11           AF
## 70       2021-02-11           DK
## 71       2021-02-11           ET
## 72       2021-02-11           PS
## 73       2021-02-11           NP
## 74       2021-02-11           LY
## 75       2021-02-11           PR
## 76       2021-02-11           SD
## 77       2021-02-11           BY
## 78       2021-02-11           KE
## 79       2021-02-11           SV
## 80       2021-02-11           NG
## 81       2021-02-11           OM
## 82       2021-02-11           XK
## 83       2021-02-11           AL
## 84       2021-02-11           KR
## 85       2021-02-11           KG
## 86       2021-02-11           LV
## 87       2021-02-11           ZW
## 88       2021-02-11           VE
## 89       2021-02-11           KW
## 90       2021-02-11           SY
## 91       2021-02-11           AE
## 92       2021-02-11           MY
## 93       2021-02-11           AU
## 94       2021-02-11           ZM
## 95       2021-02-11           MW
## 96       2021-02-11           ME
## 97       2021-02-11           SN
## 98       2021-02-11           FI
## 99       2021-02-11           CD
## 100      2021-02-11           UZ
## 101      2021-02-11           YE
## 102      2021-02-11           SZ
## 103      2021-02-11           LU
## 104      2021-02-11           NO
## 105      2021-02-11           UY
## 106      2021-02-11           AO
## 107      2021-02-11           MZ
## 108      2021-02-11           GH
## 109      2021-02-11           EE
## 110      2021-02-11           CM
## 111      2021-02-11           MR
## 112      2021-02-11           BH
## 113      2021-02-11         <NA>
## 114      2021-02-11           LK
## 115      2021-02-11           JM
## 116      2021-02-11           ML
## 117      2021-02-11           UG
## 118      2021-02-11           BZ
## 119      2021-02-11           MT
## 120      2021-02-11           MG
## 121      2021-02-11           QA
## 122      2021-02-11           CU
## 123      2021-02-11           HT
## 124      2021-02-11           RW
## 125      2021-02-11           CY
## 126      2021-02-11           LS
## 127      2021-02-11           GY
## 128      2021-02-11           BW
## 129      2021-02-11           BS
## 130      2021-02-11           NI
## 131      2021-02-11           CI
## 132      2021-02-11           NE
## 133      2021-02-11           SR
## 134      2021-02-11           GP
## 135      2021-02-11           SO
## 136      2021-02-11           CV
## 137      2021-02-11           TT
## 138      2021-02-11           PF
## 139      2021-02-11           GM
## 140      2021-02-11           BF
## 141      2021-02-11           GU
## 142      2021-02-11           TD
## 143      2021-02-11           CG
## 144      2021-02-11           KM
## 145      2021-02-11           AD
## 146      2021-02-11           TJ
## 147      2021-02-11           GQ
## 148      2021-02-11           GN
## 149      2021-02-11           LR
## 150      2021-02-11           GI
## 151      2021-02-11           TH
## 152      2021-02-11           TG
## 153      2021-02-11           GF
## 154      2021-02-11           SL
## 155      2021-02-11           GA
## 156      2021-02-11           SM
## 157      2021-02-11           SS
## 158      2021-02-11           YT
## 159      2021-02-11           JE
## 160      2021-02-11           AW
## 161      2021-02-11           CF
## 162      2021-02-11           DJ
## 163      2021-02-11           BJ
## 164      2021-02-11           MV
## 165      2021-02-11           LI
## 166      2021-02-11           RE
## 167      2021-02-11           GW
## 168      2021-02-11           MQ
## 169      2021-02-11           VN
## 170      2021-02-11           IS
## 171      2021-02-11           SG
## 172      2021-02-11           SX
## 173      2021-02-11           IM
## 174      2021-02-11           NZ
## 175      2021-02-11           VI
## 176      2021-02-11           CW
## 177      2021-02-11           LC
## 178      2021-02-11           TZ
## 179      2021-02-11           BB
## 180      2021-02-11           MC
## 181      2021-02-11           ST
## 182      2021-02-11           GG
## 183      2021-02-11             
## 184      2021-02-11           BM
## 185      2021-02-11           MF
## 186      2021-02-11           MU
## 187      2021-02-11           AG
## 188      2021-02-11           PG
## 189      2021-02-11           TC
## 190      2021-02-11           ER
## 191      2021-02-11           SC
## 192      2021-02-11           VC
## 193      2021-02-11           XA
## 194      2021-02-11           BN
## 195      2021-02-11           BI
## 196      2021-02-11           KY
## 197      2021-02-11           FJ
## 198      2021-02-11           MN
## 199      2021-02-11           MP
## 200      2021-02-11           BT
## 201      2021-02-11           VG
## 202      2021-02-11           FO
## 203      2021-02-11           GD
## 204      2021-02-11           MS
## 205      2021-02-11           AS
## 206      2021-02-11           AI
## 207      2021-02-11           KH
## 208      2021-02-11           CK
## 209      2021-02-11           KP
## 210      2021-02-11           DM
## 211      2021-02-11           FK
## 212      2021-02-11           GL
## 213      2021-02-11           VA
## 214      2021-02-11           KI
## 215      2021-02-11           LA
## 216      2021-02-11           MH
## 217      2021-02-11           FM
## 218      2021-02-11           NR
## 219      2021-02-11           NC
## 220      2021-02-11           NU
## 221      2021-02-11           PW
## 222      2021-02-11           PN
## 223      2021-02-11           XC
## 224      2021-02-11           BL
## 225      2021-02-11           SH
## 226      2021-02-11           KN
## 227      2021-02-11           PM
## 228      2021-02-11           WS
## 229      2021-02-11           XB
## 230      2021-02-11           SB
## 231      2021-02-11           TL
## 232      2021-02-11           TK
## 233      2021-02-11           TO
## 234      2021-02-11           TM
## 235      2021-02-11           TV
## 236      2021-02-11           VU
## 237      2021-02-11           WF
##                                                      country who_region
## 1                                   United States of America       AMRO
## 2                                                     Brazil       AMRO
## 3                                                     Mexico       AMRO
## 4                                                      India      SEARO
## 5                                         The United Kingdom       EURO
## 6                                                      Italy       EURO
## 7                                                     France       EURO
## 8                                         Russian Federation       EURO
## 9                                                      Spain       EURO
## 10                                                   Germany       EURO
## 11                                Iran (Islamic Republic of)       EMRO
## 12                                                  Colombia       AMRO
## 13                                                 Argentina       AMRO
## 14                                              South Africa       AFRO
## 15                                                      Peru       AMRO
## 16                                                    Poland       EURO
## 17                                                 Indonesia      SEARO
## 18                                                    Turkey       EURO
## 19                                                   Ukraine       EURO
## 20                                                   Belgium       EURO
## 21                                                    Canada       AMRO
## 22                                                   Romania       EURO
## 23                                                     Chile       AMRO
## 24                                                   Czechia       EURO
## 25                                                   Ecuador       AMRO
## 26                                                  Portugal       EURO
## 27                                               Netherlands       EURO
## 28                                                   Hungary       EURO
## 29                                                      Iraq       EMRO
## 30                                                    Sweden       EURO
## 31                                                  Pakistan       EMRO
## 32                                               Philippines       WPRO
## 33                          Bolivia (Plurinational State of)       AMRO
## 34                                                     Egypt       EMRO
## 35                                                  Bulgaria       EURO
## 36                                               Switzerland       EURO
## 37                                                   Morocco       EMRO
## 38                                                Bangladesh      SEARO
## 39                                                   Austria       EURO
## 40                                                   Tunisia       EMRO
## 41                                                     Japan       WPRO
## 42                                              Saudi Arabia       EMRO
## 43                                                    Greece       EURO
## 44                                                 Guatemala       AMRO
## 45                                                    Panama       AMRO
## 46                                                  Slovakia       EURO
## 47                                                   Croatia       EURO
## 48                                                    Israel       EURO
## 49                                    Bosnia and Herzegovina       EURO
## 50                                                     China       WPRO
## 51                                                    Jordan       EMRO
## 52                                                    Serbia       EURO
## 53                                                  Slovenia       EURO
## 54                                                   Lebanon       EMRO
## 55                                                   Ireland       EURO
## 56                                                  Honduras       AMRO
## 57                                       Republic of Moldova       EURO
## 58                                                   Georgia       EURO
## 59                                                Kazakhstan       EURO
## 60                                                   Myanmar      SEARO
## 61                                                Azerbaijan       EURO
## 62                                                   Armenia       EURO
## 63                                                 Lithuania       EURO
## 64                                           North Macedonia       EURO
## 65                                                   Algeria       AFRO
## 66                                        Dominican Republic       AMRO
## 67                                                  Paraguay       AMRO
## 68                                                Costa Rica       AMRO
## 69                                               Afghanistan       EMRO
## 70                                                   Denmark       EURO
## 71                                                  Ethiopia       AFRO
## 72  occupied Palestinian territory, including east Jerusalem       EMRO
## 73                                                     Nepal      SEARO
## 74                                                     Libya       EMRO
## 75                                               Puerto Rico       AMRO
## 76                                                     Sudan       EMRO
## 77                                                   Belarus       EURO
## 78                                                     Kenya       AFRO
## 79                                               El Salvador       AMRO
## 80                                                   Nigeria       AFRO
## 81                                                      Oman       EMRO
## 82                                                 Kosovo[1]       EURO
## 83                                                   Albania       EURO
## 84                                         Republic of Korea       WPRO
## 85                                                Kyrgyzstan       EURO
## 86                                                    Latvia       EURO
## 87                                                  Zimbabwe       AFRO
## 88                        Venezuela (Bolivarian Republic of)       AMRO
## 89                                                    Kuwait       EMRO
## 90                                      Syrian Arab Republic       EMRO
## 91                                      United Arab Emirates       EMRO
## 92                                                  Malaysia       WPRO
## 93                                                 Australia       WPRO
## 94                                                    Zambia       AFRO
## 95                                                    Malawi       AFRO
## 96                                                Montenegro       EURO
## 97                                                   Senegal       AFRO
## 98                                                   Finland       EURO
## 99                          Democratic Republic of the Congo       AFRO
## 100                                               Uzbekistan       EURO
## 101                                                    Yemen       EMRO
## 102                                                 Eswatini       AFRO
## 103                                               Luxembourg       EURO
## 104                                                   Norway       EURO
## 105                                                  Uruguay       AMRO
## 106                                                   Angola       AFRO
## 107                                               Mozambique       AFRO
## 108                                                    Ghana       AFRO
## 109                                                  Estonia       EURO
## 110                                                 Cameroon       AFRO
## 111                                               Mauritania       AFRO
## 112                                                  Bahrain       EMRO
## 113                                                  Namibia       AFRO
## 114                                                Sri Lanka      SEARO
## 115                                                  Jamaica       AMRO
## 116                                                     Mali       AFRO
## 117                                                   Uganda       AFRO
## 118                                                   Belize       AMRO
## 119                                                    Malta       EURO
## 120                                               Madagascar       AFRO
## 121                                                    Qatar       EMRO
## 122                                                     Cuba       AMRO
## 123                                                    Haiti       AMRO
## 124                                                   Rwanda       AFRO
## 125                                                   Cyprus       EURO
## 126                                                  Lesotho       AFRO
## 127                                                   Guyana       AMRO
## 128                                                 Botswana       AFRO
## 129                                                  Bahamas       AMRO
## 130                                                Nicaragua       AMRO
## 131                                         CÃ´te dâ\200\231Ivoire       AFRO
## 132                                                    Niger       AFRO
## 133                                                 Suriname       AMRO
## 134                                               Guadeloupe       AMRO
## 135                                                  Somalia       EMRO
## 136                                               Cabo Verde       AFRO
## 137                                      Trinidad and Tobago       AMRO
## 138                                         French Polynesia       WPRO
## 139                                                   Gambia       AFRO
## 140                                             Burkina Faso       AFRO
## 141                                                     Guam       WPRO
## 142                                                     Chad       AFRO
## 143                                                    Congo       AFRO
## 144                                                  Comoros       AFRO
## 145                                                  Andorra       EURO
## 146                                               Tajikistan       EURO
## 147                                        Equatorial Guinea       AFRO
## 148                                                   Guinea       AFRO
## 149                                                  Liberia       AFRO
## 150                                                Gibraltar       EURO
## 151                                                 Thailand      SEARO
## 152                                                     Togo       AFRO
## 153                                            French Guiana       AMRO
## 154                                             Sierra Leone       AFRO
## 155                                                    Gabon       AFRO
## 156                                               San Marino       EURO
## 157                                              South Sudan       AFRO
## 158                                                  Mayotte       AFRO
## 159                                                   Jersey       EURO
## 160                                                    Aruba       AMRO
## 161                                 Central African Republic       AFRO
## 162                                                 Djibouti       EMRO
## 163                                                    Benin       AFRO
## 164                                                 Maldives      SEARO
## 165                                            Liechtenstein       EURO
## 166                                                 RÃ©union       AFRO
## 167                                            Guinea-Bissau       AFRO
## 168                                               Martinique       AMRO
## 169                                                 Viet Nam       WPRO
## 170                                                  Iceland       EURO
## 171                                                Singapore       WPRO
## 172                                             Sint Maarten       AMRO
## 173                                              Isle of Man       EURO
## 174                                              New Zealand       WPRO
## 175                             United States Virgin Islands       AMRO
## 176                                                 CuraÃ§ao       AMRO
## 177                                              Saint Lucia       AMRO
## 178                              United Republic of Tanzania       AFRO
## 179                                                 Barbados       AMRO
## 180                                                   Monaco       EURO
## 181                                    Sao Tome and Principe       AFRO
## 182                                                 Guernsey       EURO
## 183                                                    Other      Other
## 184                                                  Bermuda       AMRO
## 185                                             Saint Martin       AMRO
## 186                                                Mauritius       AFRO
## 187                                      Antigua and Barbuda       AMRO
## 188                                         Papua New Guinea       WPRO
## 189                                 Turks and Caicos Islands       AMRO
## 190                                                  Eritrea       AFRO
## 191                                               Seychelles       AFRO
## 192                         Saint Vincent and the Grenadines       AMRO
## 193                                                  Bonaire       AMRO
## 194                                        Brunei Darussalam       WPRO
## 195                                                  Burundi       AFRO
## 196                                           Cayman Islands       AMRO
## 197                                                     Fiji       WPRO
## 198                                                 Mongolia       WPRO
## 199           Northern Mariana Islands (Commonwealth of the)       WPRO
## 200                                                   Bhutan      SEARO
## 201                                   British Virgin Islands       AMRO
## 202                                            Faroe Islands       EURO
## 203                                                  Grenada       AMRO
## 204                                               Montserrat       AMRO
## 205                                           American Samoa       WPRO
## 206                                                 Anguilla       AMRO
## 207                                                 Cambodia       WPRO
## 208                                             Cook Islands       WPRO
## 209                    Democratic People's Republic of Korea      SEARO
## 210                                                 Dominica       AMRO
## 211                              Falkland Islands (Malvinas)       AMRO
## 212                                                Greenland       EURO
## 213                                                 Holy See       EURO
## 214                                                 Kiribati       WPRO
## 215                         Lao People's Democratic Republic       WPRO
## 216                                         Marshall Islands       WPRO
## 217                         Micronesia (Federated States of)       WPRO
## 218                                                    Nauru       WPRO
## 219                                            New Caledonia       WPRO
## 220                                                     Niue       WPRO
## 221                                                    Palau       WPRO
## 222                                         Pitcairn Islands       WPRO
## 223                                                     Saba       AMRO
## 224                                        Saint BarthÃ©lemy       AMRO
## 225                                             Saint Helena       AFRO
## 226                                    Saint Kitts and Nevis       AMRO
## 227                                Saint Pierre and Miquelon       AMRO
## 228                                                    Samoa       WPRO
## 229                                           Sint Eustatius       AMRO
## 230                                          Solomon Islands       WPRO
## 231                                              Timor-Leste      SEARO
## 232                                                  Tokelau       WPRO
## 233                                                    Tonga       WPRO
## 234                                             Turkmenistan       EURO
## 235                                                   Tuvalu       WPRO
## 236                                                  Vanuatu       WPRO
## 237                                        Wallis and Futuna       WPRO
##     new_cases cumulative_cases new_deaths cumulative_deaths
## 1       90930         26923756       2802            464412
## 2       51486          9599565       1350            233520
## 3       10738          1946751       1701            168432
## 4       12923         10871294        108            155360
## 5       13013          3985165       1001            114851
## 6       12947          2668266        336             92338
## 7       24631          3328987        295             80032
## 8       15038          4027748        553             78687
## 9        8298          3023601         86             63704
## 10      10237          2310233        666             63635
## 11       7585          1488981         61             58686
## 12       5442          2166904        217             56507
## 13       7794          1993295        158             49556
## 14       3159          1482412        276             47145
## 15       5557          1196778        159             42626
## 16       7013          1570658        456             40177
## 17       8776          1183555        191             32167
## 18       8642          2556837         95             27093
## 19       5039          1258094        124             24058
## 20          0           730951          0             21512
## 21       2677           810797         74             20909
## 22       3048           752482         79             19135
## 23       2387           760576         21             19105
## 24       9537          1064952        130             17772
## 25          0           259783          0             15086
## 26       4387           774889        161             14718
## 27       3226          1012908         78             14589
## 28       1862           381875         97             13444
## 29       2282           634539          6             13140
## 30       4070           600244          3             12326
## 31       1072           557591         62             12128
## 32       1333           541560        105             11401
## 33       1544           230731         65             10929
## 34        610           171390         53              9804
## 35        913           226974         45              9527
## 36       1598           535511         17              8931
## 37        564           476689         12              8436
## 38        388           539153         10              8239
## 39       1433           424005         29              8013
## 40       1086           219650         46              7378
## 41       1826           410012        121              6678
## 42        369           371356          5              6415
## 43       1482           167549         17              6034
## 44        753           164746         34              5989
## 45        822           328476         25              5531
## 46       3179           268986        120              5502
## 47        376           236709         25              5263
## 48       6075           705092         39              5238
## 49        253           124696         19              4853
## 50         21           101429          1              4834
## 51       1855           340177         16              4411
## 52       2088           413943         14              4168
## 53       1431           175795          1              3895
## 54       3157           328016         66              3803
## 55        999           205939         42              3794
## 56        871           156606         47              3789
## 57        890           166553         16              3589
## 58        557           264158         10              3321
## 59          0           247533          0              3185
## 60         39           141487          1              3181
## 61        147           231509          4              3167
## 62        180           168676          5              3135
## 63       1067           189058         25              3013
## 64        389            95736          4              2959
## 65        223           109782          2              2926
## 66        934           225472         19              2883
## 67        874           139819         16              2862
## 68        417           197852          6              2698
## 69         18            55420          1              2419
## 70        470           202887         11              2255
## 71        683           144249          9              2167
## 72        761           185760         12              2100
## 73        134           272349          0              2047
## 74        467           126028          7              1989
## 75         92            96494          4              1897
## 76         32            29933          6              1849
## 77       1799           261859         10              1811
## 78        173           102221          2              1791
## 79          0            56653          8              1709
## 80       1131           142578          8              1702
## 81        190           136377          1              1537
## 82        307            63046          4              1522
## 83       1143            88671         15              1503
## 84        504            82434         10              1496
## 85         75            85328          1              1437
## 86        990            73859         32              1395
## 87         79            34864         11              1364
## 88        500           131096          7              1247
## 89        987           173983          5               980
## 90         57            14668          4               965
## 91       3539           336142          9               956
## 92       3288           251604         14               923
## 93         11            28871          0               909
## 94        963            65573         20               901
## 95        328            28050         17               900
## 96          0            66853          0               852
## 97        276            29521         12               712
## 98        400            48807          3               706
## 99        119            23889          2               685
## 100        66            79303          1               622
## 101         2             2137          1               617
## 102        53            16341          6               616
## 103       225            52247          2               602
## 104       222            65338          9               592
## 105       503            46153          9               506
## 106        47            20210          9               487
## 107       951            46736          6               486
## 108         0            73003          0               482
## 109       675            50280          3               480
## 110         0            31394          0               474
## 111         0            16777          0               425
## 112       797           109604          4               391
## 113        41            35242          1               378
## 114       963            72174          5               375
## 115       207            17908          3               362
## 116        11             8203          0               339
## 117        28            39911          1               328
## 118         9            12079          3               310
## 119       167            19182          2               286
## 120         0            19360          0               285
## 121       451           155453          0               253
## 122       858            34922          5               249
## 123        22            11991          0               246
## 124       130            16941          3               229
## 125       113            32072          3               217
## 126        86             9804          3               210
## 127        18             8041          0               181
## 128         0            24435          0               179
## 129         0             8289          0               176
## 130         0             5064          0               171
## 131       166            30240          4               169
## 132         0             4643          0               167
## 133        20             8710          1               163
## 134         0             9302          0               159
## 135        73             4935          5               139
## 136        64            14543          1               138
## 137         1             7617          1               136
## 138        22            18244          0               135
## 139         0             4302          0               135
## 140        59            11426          0               134
## 141        10             7476          0               130
## 142        29             3568          1               126
## 143         0             8354          0               122
## 144        30             3209          2               120
## 145        40            10352          0               106
## 146         0            13714          0                91
## 147         0             5614          0                87
## 148        27            14791          0                84
## 149         0             1956          0                84
## 150         3             4190          0                83
## 151       201            24104          0                80
## 152        30             5606          0                80
## 153         0            16296          0                79
## 154        14             3803          0                79
## 155       335            12171          0                71
## 156        26             3213          0                71
## 157       351             4960          6                71
## 158       336            11783          1                69
## 159        10             3187          0                67
## 160        48             7286          2                66
## 161         7             4996          0                63
## 162         3             5962          0                63
## 163         0             4560          0                56
## 164       100            17201          0                56
## 165         4             2607          1                47
## 166       420            10907          0                47
## 167        16             2826          0                46
## 168         0             6521          0                45
## 169        39             2109          0                35
## 170         1             6026          0                29
## 171        15            59747          0                29
## 172        14             1952          0                27
## 173         1              436          0                25
## 174         1             1968          0                25
## 175        12             2485          0                24
## 176         7             4620          1                22
## 177       110             2137          3                22
## 178         0              509          0                21
## 179        82             1814          2                20
## 180        22             1717          0                20
## 181        45             1430          0                18
## 182        11              752          0                13
## 183         0              745          0                13
## 184         0              694          0                12
## 185         0             1377          0                12
## 186        10              594          0                10
## 187        34              350          1                 9
## 188        14              914          0                 9
## 189        43             1738          0                 9
## 190        17             2418          0                 7
## 191       120             1695          0                 7
## 192        16             1340          2                 6
## 193         0              368          1                 4
## 194         1              183          0                 3
## 195        20             1771          0                 3
## 196         0              408          0                 2
## 197         0               56          0                 2
## 198        54             2174          0                 2
## 199         0              133          0                 2
## 200         0              861          0                 1
## 201        10              151          0                 1
## 202         0              657          0                 1
## 203         0              148          0                 1
## 204         0               18          0                 1
## 205         0                0          0                 0
## 206         0               18          0                 0
## 207         0              478          0                 0
## 208         0                0          0                 0
## 209         0                0          0                 0
## 210         0              121          0                 0
## 211         0               49          0                 0
## 212         0               30          0                 0
## 213         0               26          0                 0
## 214         0                0          0                 0
## 215         0               45          0                 0
## 216         0                4          0                 0
## 217         0                0          0                 0
## 218         0                0          0                 0
## 219         0               50          0                 0
## 220         0                0          0                 0
## 221         0                0          0                 0
## 222         0                0          0                 0
## 223         0                6          0                 0
## 224         0              425          0                 0
## 225         0                0          0                 0
## 226         0               40          0                 0
## 227         0               24          0                 0
## 228         0                3          0                 0
## 229         0               20          0                 0
## 230         0               18          0                 0
## 231        14              100          0                 0
## 232         0                0          0                 0
## 233         0                0          0                 0
## 234         0                0          0                 0
## 235         0                0          0                 0
## 236         0                1          0                 0
## 237         0                9          0                 0
```


```r
### 5 highest cummulative cases on feb 11 2021: United States of America, India, Brazil, Russian Federation, The United Kingdom.
###5 highest cummulative deaths on feb 11 2021: United States of America, Brazil, Mexico, India, The United Kingdom.
```


```r
covid%>%
  filter(i_date_reported=="2021-02-11")%>%
  filter(country=="United States of America", country=="India", country=="Brazil", country=="Russian Federation", country=="The United Kingdom")
```

```
## [1] i_date_reported   country_code      country           who_region       
## [5] new_cases         cumulative_cases  new_deaths        cumulative_deaths
## <0 rows> (or 0-length row.names)
```


