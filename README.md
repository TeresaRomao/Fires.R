# Fires

Description
Forest fires are a very important issue that negatively affects climate change. Typically, the causes of forest fires are those oversights, accidents and negligence committed by individuals, intentional acts and natural causes. The latter is the root cause for only a minority of the fires.
Their harmful impacts and effects on ecosystems can be major ones. Among them, we can mention the disappearance of native species,  the increase in levels of carbon dioxide in the atmosphere, the earth’s nutrients destroyed by the ashes, and the massive loss of wildlife. 
Data mining techniques can help in the prediction of the cause of the fire and, thus, better support the decision of taking preventive measures in order to avoid tragedy. In effect, this can play a major role in resource allocation, mitigation and recovery efforts. 
The ICFN - Nature and Forest Conservation Institute has the record of the list of forest fires that occurred in Portugal for several years.  For each fire, there is information such as the site, the alert date/hour, the extinction date/hour, the affected area and the cause type (intentional, natural, negligent, rekindling or unknown).

In the file fires_train.csv, you have data on reported forest fires during 2014 and 2015, for which the cause is known.  The attributes have information regarding the forest fire’s alarm point and the total affected area:

id, 
region, 
district, 
municipality, 
parish, 
lat, 
lon, 
origin, 
alert_date, 
alert_hour, 
extinction_date, 
extinction_hour, 
firstInterv_date, 
firstInterv_hour, 
alert_source, 
village_area, 
vegetation_area, 
farming_area, 
village_veget_area, 
total_area, 
intentional_cause.

The goal of this practical assignment is to build a machine learning model to predict the cause type of a forest fire: intentional or non-intentional.

As additional information, you can choose to use weather data. The script getTemperatureNOAA.R helps you with that. It allows you to get data from the National Oceanic and Atmospheric Administration (NOAA) Climate Data Sources. Namely, you can have access to the archive of global historical weather and climate data in addition to station history information. These data include quality controlled daily, monthly, seasonal, and yearly measurements of temperature, precipitation, wind, and degree days as well as radar data and 30-year Climate Normals. In the station_data.RData file you have the data collected from all the stations and from which you can fetch weather information. You can also fetch this information from other sources.

Tasks
Using the above data set, you have a set of main tasks to accomplish as described next. Still, you are free to include other tasks to increase the value of your assignment.  

Task 1: Data importation, clean-up and pre-processing
In this task, you should focus on importing the provided data into an appropriate R format so that your posterior analysis is made simpler.  You should also check if it is necessary to carry out any data clean-up and/or pre-processing steps.


Task 2: Data exploratory analysis
This task involves summarising and visualising the data to provide useful insights.  Think about questions that could be interesting to check with the available data, and provide answers either using textual summaries or data visualisation.


Task 3: Predictive modelling
From the available data, you should define the data set that will be used for the classification task at hand.  Different models should be considered and the choice of the final model should be justified. 
