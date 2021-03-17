# The Effect of TV Commercials on Internet Traffic
Welcome to our Repository! We are four students of Business Analytics and Quantitative Marketing at the Erasmus University in Rotterdam. For two months, we have researched the effects of TV commercials on website traffic for the e-commerce company Coolblue. We want to make these results more accessible using this application. Thank you for expressing your interest in our research!

# List of code
* ```Explanatory Analysis```
* ```Data Preprocessing```

* ```Z Score Algorithm```
 
   The Z Score Algorithm finds peaks in the data for given values of the parameters __lag__, __influence__ and __threshold__. The algorithm can be performed for a single day, but also for all days concatenated. The best performing commercials are collected in a dataframe. The analysis for the Belgian commercials comes after the analysis for the Dutch commercials.
  
* ```Direct Effects - 5 minutes```

   For the Direct Effects model with a 5-minute interval, we calculate the amount of traffic in an interval before the broadcast of a commercial and after the broadcast. Results are stored in the columns _preVisitorsApp_, _preVisitorsWeb_, _postVisitorsApp_ and _postVisitorsWeb_ in the dataframe _broad_. Furthermore, the Direct Effects model with a 5-minute interval is constructed and tested.
   
* ```Direct Effects - 20 minutes```   

  We also created a Direct Effects model with a 20-minute interval. The set-up is the same as the intervalCode. However, this time we subset on commercials with a GRP higher than 0.5. In the model, we use the internet visits of Belgium during	 the same time frame as control variable.
  
* ```Artificial Neural Network```

   First, we build a data set that contains al the information about commercials and website traffic for every minute in the time interval. Then, we build an Artificial Neural Network (ANN) and compare with a linear model. Finally, the ANN is trained again and a variable effect analysis is done to extract the influence of, for example, channels on the amount of visitors. 
   
* ```Bayesian Structural Time Series model```

   In this file, we obtain the Google searches for MediaMarkt and BCC. The, we sum the visit densities for each day of the first half of 2019, and make four Bayesian Structural Time Series models for four distinct periods. For each of these periods, we also test for a trend and for seasonality.

* ```R Shiny```

   The tab 'Data Exploration' gives information on all commercials that were broadcast in the first half of 2019, given certain criteria. The tab 'Direct Effects' again takes certain criteria for a commercial. The application then tells you what the expected absolute increase in visitors would be. ![direc](https://user-images.githubusercontent.com/16563680/111459485-03a5db00-871b-11eb-949e-b1878a3664ec.png)
