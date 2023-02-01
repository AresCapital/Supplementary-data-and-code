

## Data

###  01. data.csv

Daily number of influenza cases and the related information of meterology and air pollutants. 

- **citycode** represents the specific city

* **date** represents the date  "*yyyy-mm-dd*".

* **mon** represents the month "*yyyy-mm*".

* **day** reprents the day in each year "*mm-yy*".

* **dow** represents the day of week, from *Sunday* (1) to *Saturday* (7).

  Daily recorded data of influenza cases  were collected and extracted from the  the *Chinese Information System for Disease Control and Prevention*. Here we selected the 82 cities with over 500 cases during each year of 2015~2019 for study.

* **total** represents the daily reported influenza case number in this city. 

* **child** represents the daily reported influenza case number of people aged 0~14 years . 

* **adult** represents the daily reported influenza case number of people aged 15~  years. 

  Daily temperature and relative humidity  were collected from from China Meteorological Data Service Center (http://data.cma.cn). 

* **tem** represents the daily average temperature. ($^\circ C$)

* **rhu** represents the daily average relative humidity. ($\%RH$)

  Daily air pollutants data of PM2.5, PM10, NO2, SO2, CO and O3 during 2015-2019 were collected from the China National Environmental Monitoring Centre (http://www.cnemc.cn/sssj). Thank you for *Dr. Wang Xiaolei*'s work in data collection of air pollutants (https://quotsoft.net/air/). Kalman smoothing implemented in the R package "imputeTS" was used to impute the few missing values of air pollutants. (*Hadeed et al. 2020; Moritz and Bartz-Beielstein 2017*). 

* **AQI**  represents daily *air quality index*, calculated by the 6 major air pollutants ($PM_{2.5}$, $PM_{10}$, $SO_2$, $NO_2$, $O_3$, $CO$), according to the *Technical Regulation on Ambient Air Quality Index (HJ 633—2012)* (https://www.mee.gov.cn/ywgz/fgbz/bz/bzwb/jcffbz/201203/t20120302_224166.shtml)
* **PM2.5** represents the 24-hour average level of particles with aerodynamic diameters of less than or equal to 2.5 $\mu m$. ($\mu g/m^3$)
* **PM10** represents the 24-hour average level of particles with aerodynamic diameters of less than or equal to 10 $\mu m$.  ($\mu g/m^3$)
* **SO2** represents the 24-hour average level of sulfur dioxide.  ($\mu g/m^3$)
* **NO2** represents the 24-hour average level of nitrogen dioxide.  ($\mu g/m^3$)
* **O3** represents the 24-hour maximum of  8-hour moving average level of ozone.  ($\mu g/m^3$)
* **CO** represents the 24-hour average level of carbon monoxide.  ($mg/m^3$)



### 02. city.csv

Information of the selected 82 cities

- **citycode** represents the specific city.

- **city_name** represents the name of city in Chinese.

- **lon** represents longitude of the city's centroid. 

- **lat**  represents latitude of the city's centroid.

- **pop** represents city-specifc population.

- **childprop** represents proportion of children<15years old in specific city.

- **climate** represents climate region of that the city belong to.

- **pwd_g** represents the geometric mean of population weighted density, calculated based on the 2020 1km raster data of Asia population counts offered by WorldPop. (https://www.worldpop.org/doi/10.5258/SOTON/WP00013; https://www.worldpop.org/methods/pwd)

  





