# The Effect of TV Commercials on Internet Traffic
Welcome to our Repository! We are four students of Business Analytics and Quantitative Marketing at the Erasmus University in Rotterdam. For two months, we researched the effects of TV commercials on website traffic for the e-commerce company Coolblue. We want to make these results more accessible using this application. Thank you for expressing your interest in our research!

# List of code
* ```plotsOnly```
* ```Data Preprocessing```

* ```ZScoreAlgorithm```
 
   The Z Score Algorithm finds peaks in the data for given values of the parameters _lag_, _influence_ and _threshold_. The algorithm can be performed on a single day, but also on all days concetenated. The best performing commercials are collected in a dataframe. The analysis for Belgian commercials comes after the analysis for Dutch commercials.
  
* ```intervalCode```

   For the direct effects model we calculate the amount of traffic in an interval before the broadcast of a commercial and after the broadcast Results are stored in the column preVisitorsApp, preVisitorsWeb, postVisitorsApp and postVisitorsWeb in the dataframe broad.
   
   Further the direct effects models with a 5-minute interval are made and tested.
   
* ```intervalCodeDiD```   

  We also created a direct effects model with a 20-minute interval. The set-up is the same as the intervalCode, however, this time we subset on commercials with a GRP higher than 0.5. In the model, we use the internet visits of Belgium in the same time frame as control variable.
  
* ```neuralNetwork```

   To start, a dataset is built which containts al the information about commercials and website traffic for every minute in the time interval. A Artificial Neural Network (ANN) is build and compared with a linear model. Finally the ANN is trained again and a variable effect analysis is done to extract the influence of, for example, channels on the amount of visitors. 
   
* ```bsts```
* ```app``` (Shiny)

# Dashboard app
In the tab 'Data Exploration', you can find information on all commercials that were broadcast in the first half of 2019, given certain criteria.In the tab 'Direct Effects', you can again fill in certain criteria for a broadcast. The application then tells you what the expected absolute increase in visitors would be. ![direc](https://user-images.githubusercontent.com/16563680/111459485-03a5db00-871b-11eb-949e-b1878a3664ec.png)
