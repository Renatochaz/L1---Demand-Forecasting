   *** This is a private work, data isn't avaliable and identification of the client is omitted ***
 
 ﻿# Demand forecasting - details
  
  A local supermarket chain was interested in forecasting their produce section, they manually estimated when to buy and had a huge waste of food going bad before selling.
  
  Their database had over 20years of information from all stores, the data was unustructured when minned. We made a functional code to structure the data and propposed a SARIMA model with paramater variation for every item in the produce section.
  
 The model worked out in mixed ways for the items, some were analyzed to be totally random and the model coudn't predict accuratelly, while some had a accuracy of over 95%.

# Screenshots

Overview of raw unstructured data
![Screenshot](raw_data.jpg)

Overview of structure data ready for forecast analysis
![Screenshot](clean_data.jpg)

Overview of plots useds to analyse data: time series averages and plots of autocorrelation
![Screenshot](plot_time_data.jpeg)
![Screenshot](decomposition.jpeg)

Exemple of single item weekly forecast sells with 80% accuracy (blue line is predicted values and black line the actual values)

![Screenshot](week_forecast.jpeg)
