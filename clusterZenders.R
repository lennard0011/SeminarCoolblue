#program categories
cat_before = unique(broad$program_category_before)
cat_after = unique(broad$program_category_after)
categories = union(cat_before, cat_after)
rm(cat_before)
rm(cat_after)

#channel
channel = unique(broad$channel)
channel = as.data.frame(channel)
nChan = nrow(channel)

#majority product
majorityProduct = function(channelName){
  channelSubset = broad[broad$channel == channelName,]
  uniqueProduct = unique(channelSubset$product_category)
  tab = tabulate(match(channelSubset$product_category, uniqueProduct))
  majority_product = uniqueProduct[tab == max(tab)]
  return(majority_product)
}

#channel country
channel$country = 0
for (i in 1:nChan){
  print(i)
  for (j in 1:nBroad){
    if (channel[i, 1] == broad[j, ]$channel){
      if (broad[j, ]$country == "Netherlands"){
        channel$country[i] = 1
      }
      break
    }
  }
}

#majority for adverts
channel$washingmachine = 0
channel$television = 0
for (i in 1:nChan){
  print(i)
  if (majorityProduct(channel[i, 1]) == "wasmachines"){
    channel$washingmachine[i] = 1
  }
  if (majorityProduct(channel[i, 1]) == "televisies"){
    channel$television[i] = 1
  }
}

#public or private
channel$private = 0
channel$private[1:9] = 1
channel$private[13:51] = 1

#gender
channel$women = 0
channel$men = 0
channel$men[3] = 1
channel$women[7] = 1
channel$women[13] = 1
channel$women[15] = 1
channel$men[19] = 1
channel$women[20] = 1
channel$men[24] = 1
channel$men[25] = 1
channel$women[27] = 1
channel$women[33] = 1
channel$men[35] = 1

#music
channel$music = 0
channel$music[16] = 1
channel$music[26] = 1
channel$music[29] = 1
channel$music[38] = 1

#sport
channel$sport = 0
channel$sport[36] = 1
channel$sport[40] = 1
channel$sport[41] = 1
channel$sport[42] = 1

#age
channel$youth = 0
channel$youth[6] = 1
channel$youth[12] = 1
channel$youth[18] = 1

#kmeans
# Elbow method
scaled_channel = scale(channel[2:10])
elbow = fviz_nbclust(scaled_channel, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
plot(elbow)

set.seed(11)
kmean = kmeans(scaled_channel, 7)
cluster_1 = channel[,1][kmean$cluster == 1]
cluster_2 = channel[,1][kmean$cluster == 2]
cluster_3 = channel[,1][kmean$cluster == 3]
cluster_4 = channel[,1][kmean$cluster == 4]
cluster_5 = channel[,1][kmean$cluster == 5]
cluster_6 = channel[,1][kmean$cluster == 6]
cluster_7 = channel[,1][kmean$cluster == 7]


broad$cluster2 = 0
broad$cluster3 = 0
broad$cluster4 = 0
broad$cluster5 = 0
broad$cluster6 = 0
broad$cluster7 = 0
for (i in 1:nBroad){
  if (broad$channel[i] %in% cluster_2){
    broad$cluster2[i] = 1
  }
  if (broad$channel[i] %in% cluster_3){
    broad$cluster3[i] = 1
  }
  if (broad$channel[i] %in% cluster_4){
    broad$cluster4[i] = 1
  }
  if (broad$channel[i] %in% cluster_5){
    broad$cluster5[i] = 1
  }
  if (broad$channel[i] %in% cluster_6){
    broad$cluster6[i] = 1
  }
  if (broad$channel[i] %in% cluster_7){
    broad$cluster7[i] = 1
  }
}
