## K-MEANS dummies for different channel categories
channel = unique(broad$channel)
channel = as.data.frame(channel)
nChan = nrow(channel)
# channel country
channel$country = 0
for (i in 1:nChan){
  print(i)
  for (j in 1:nBroad){
    if (channel[i, 1] == broad[j, ]$channel){
      if (broad[j, ]$country == "Netherlands"){
        channel$country[i] = 1
      }
    }
    break
  }
}
# gender
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
# music
channel$music = 0
channel$music[16] = 1
channel$music[26] = 1
channel$music[29] = 1
channel$music[38] = 1
# sport
channel$sport = 0
channel$sport[36] = 1
channel$sport[40] = 1
channel$sport[41] = 1
channel$sport[42] = 1
# age
channel$youth = 0
channel$youth[6] = 1
channel$youth[12] = 1
channel$youth[18] = 1
channel$youth[30] = 1
# kmeans
scaled_channel = scale(channel[2:7])
set.seed(21)
kmean = kmeans(scaled_channel, 6)
cluster_1 = channel[,1][kmean$cluster == 1]
cluster_2 = channel[,1][kmean$cluster == 2]
cluster_3 = channel[,1][kmean$cluster == 3]
cluster_4 = channel[,1][kmean$cluster == 4]
cluster_5 = channel[,1][kmean$cluster == 5]
cluster_6 = channel[,1][kmean$cluster == 6]
broad$cluster = 0
for (i in 1:nBroad){
  if (broad$channel[i] %in% cluster_1){
    broad$cluster[i] = 1
  }
  if (broad$channel[i] %in% cluster_2){
    broad$cluster[i] = 2
  }
  if (broad$channel[i] %in% cluster_3){
    broad$cluster[i] = 3
  }
  if (broad$channel[i] %in% cluster_4){
    broad$cluster[i] = 4
  }
  if (broad$channel[i] %in% cluster_5){
    broad$cluster[i] = 5
  }
  if (broad$channel[i] %in% cluster_6){
    broad$cluster[i] = 6
  }
}

# add some manual program categories if not filled in
for (i in 1:nBroad){
  print(i)
  if (broad$program_before[i] == 'NOS FIFA WK VOETBAL (V) 2019, NEDERLAND'){
    broad$program_category_before[i] = 'voetbalreportage'
  }
  if (broad$program_after[i] == 'NOS FIFA WK VOETBAL (V) 2019, NEDERLAND'){
    broad$program_category_after[i] = 'voetbalreportage'
  }
  if (broad$program_before[i] == 'VOETBAL WK ITA CHN DAMES'){
    broad$program_category_before[i] = 'voetbalreportage'
  }
  if (broad$program_after[i] == 'VOETBAL WK ITA CHN DAMES'){
    broad$program_category_after[i] = 'voetbalreportage'
  }
  if (broad$program_before[i] == 'NOS FIFA WK VOETBAL (V) 2019, ZWEDEN - U'){
    broad$program_category_before[i] = 'voetbalreportage'
  }
  if (broad$program_after[i] == 'NOS FIFA WK VOETBAL (V) 2019, ZWEDEN - U'){
    broad$program_category_after[i] = 'voetbalreportage'
  }
  if (broad$program_before[i] == 'STUDIO FRANCE'){
    broad$program_category_before[i] = 'voetbalreportage'
  }
  if (broad$program_after[i] == 'STUDIO FRANCE'){
    broad$program_category_after[i] = 'voetbalreportage'
  }
  if (broad$program_before[i] == 'VOETBAL WK RUST'){
    broad$program_category_before[i] = 'voetbalreportage'
  }
  if (broad$program_after[i] == 'VOETBAL WK RUST'){
    broad$program_category_after[i] = 'voetbalreportage'
  }
  if (broad$program_before[i] == 'VOETBAL WK NLD JPN DAMES'){
    broad$program_category_before[i] = 'voetbalreportage'
  }
  if (broad$program_after[i] == 'VOETBAL WK NLD JPN DAMES'){
    broad$program_category_after[i] = 'voetbalreportage'
  }
  if (broad$program_before[i] == 'SLAM! 40'){
    broad$program_category_before[i] = 'overige muziek: overig'
  }
  if (broad$program_after[i] == 'SLAM! 40'){
    broad$program_category_after[i] = 'overige muziek: overig'
  }
  if (broad$program_before[i] == 'SLAM! Nonstop'){
    broad$program_category_before[i] = 'overige muziek: overig'
  }
  if (broad$program_after[i] == 'SLAM! Nonstop'){
    broad$program_category_after[i] = 'overige muziek: overig'
  }
  if (broad$program_before[i] == 'SLAM! Weekend'){
    broad$program_category_before[i] = 'overige muziek: overig'
  }
  if (broad$program_after[i] == 'SLAM! Weekend'){
    broad$program_category_after[i] = 'overige muziek: overig'
  }
  if (broad$program_before[i] == 'SLAM! NON STOP'){
    broad$program_category_before[i] = 'overige muziek: overig'
  }
  if (broad$program_after[i] == 'SLAM! NON STOP'){
    broad$program_category_after[i] = 'overige muziek: overig'
  }
  if (broad$program_before[i] == 'SLAM!NL'){
    broad$program_category_before[i] = 'overige muziek: overig'
  }
  if (broad$program_after[i] == 'SLAM!NL'){
    broad$program_category_after[i] = 'overige muziek: overig'
  } 
  if (broad$program_before[i] == 'SLAM! MixMarathon'){
    broad$program_category_before[i] = 'overige muziek: overig'
  }
  if (broad$program_after[i] == 'SLAM! MixMarathon'){
    broad$program_category_after[i] = 'overige muziek: overig'
  } 
}

broad$program_after[broad$program_category_after=='niet opgegeven']

# make new program categories
broad$program_category_before_2 = broad$program_category_before
broad$program_category_after_2 = broad$program_category_after
for (i in 1:nBroad){
  # spanning
  if (broad$program_category_before[i] %in% c('btl series: spanning', 'btl films: spanning', 'nld series: spanning', 'nld films: spanning')){
    broad$program_category_before_2[i] = 'spanning'
  }
  if (broad$program_category_after[i] %in% c('btl series: spanning', 'btl films: spanning', 'nld series: spanning', 'nld films: spanning')){
    broad$program_category_after_2[i] = 'spanning'
  }
  
  # series/films: overig
  if (broad$program_category_before[i] %in% c('btl series: overig', 'nld series: overig', 'btl films: overig', 'film', 'serie')){
    broad$program_category_before_2[i] = 'films_series_overig'
  }
  if (broad$program_category_after[i] %in% c('btl series: overig', 'nld series: overig', 'btl films: overig', 'film', 'serie')){
    broad$program_category_after_2[i] = 'films_series_overig'
  }
  
  # drama
  if (broad$program_category_before[i] %in% c('nld series: drama', 'btl series: drama', 'btl films: drama', 'nld films: drama')){
    broad$program_category_before_2[i] = 'drama'
  }
  if (broad$program_category_after[i] %in% c('nld series: drama', 'btl series: drama', 'btl films: overig', 'nld films: drama')){
    broad$program_category_after_2[i] = 'drama'
  }
  
  # talent show
  if (broad$program_category_before[i] %in% c('show', 'talentenjacht of auditieprogramma')){
    broad$program_category_before_2[i] = 'talent_show'
  }
  if (broad$program_category_after[i] %in% c('show', 'talentenjacht of auditieprogramma')){
    broad$program_category_after_2[i] = 'talent_show'
  }
  
  # reality
  if (broad$program_category_before[i] %in% c('reality structured', 'docusoap/reality serie', 'reality show')){
    broad$program_category_before_2[i] = 'reality'
  }
  if (broad$program_category_after[i] %in% c('reality structured', 'docusoap/reality serie', 'reality show')){
    broad$program_category_after_2[i] = 'reality'
  }
  
  # comedy
  if (broad$program_category_before[i] %in% c('on stage', 'nld series: (sit)comedy', 'btl series: (sit)comedy', 'nld films: comedy', 'btl films: comedy')){
    broad$program_category_before_2[i] = 'comedy'
  }
  if (broad$program_category_after[i] %in% c('on stage', 'nld series: (sit)comedy', 'btl series: (sit)comedy', 'nld films: comedy', 'btl films: comedy')){
    broad$program_category_after_2[i] = 'comedy'
  }
  
  # sport
  if (broad$program_category_before[i] %in% c('voetbalreportage', 'actuele sportinformatie', 'overige sportinformatie', 'overige sportreportage')){
    broad$program_category_before_2[i] = 'sport'
  }
  if (broad$program_category_after[i] %in% c('voetbalreportage', 'actuele sportinformatie', 'overige sportinformatie', 'overige sportreportage')){
    broad$program_category_after_2[i] = 'sport'
  }
  
  # music
  if (broad$program_category_before[i] %in% c('populaire muziek: videoclips', 'populaire muziek: programma', 'overige muziek: overig')){
    broad$program_category_before_2[i] = 'music'
  }
  if (broad$program_category_after[i] %in% c('populaire muziek: videoclips', 'populaire muziek: programma', 'overige muziek: overig')){
    broad$program_category_after_2[i] = 'music'
  }
  
  # quiz
  if (broad$program_category_before[i] %in% c('spel & quiz', 'game/quiz')){
    broad$program_category_before_2[i] = 'quiz'
  }
  if (broad$program_category_after[i] %in% c('spel & quiz', 'game/quiz')){
    broad$program_category_after_2[i] = 'quiz'
  }
  
  # soap
  if (broad$program_category_before[i] %in% c('btl series: soap', 'nld series: soap')){
    broad$program_category_before_2[i] = 'soap'
  }
  if (broad$program_category_after[i] %in% c('btl series: soap', 'nld series: soap')){
    broad$program_category_after_2[i] = 'soap'
  }
  
  # news
  if (broad$program_category_before[i] %in% c('weerbericht', 'actualiteiten', 'nieuws', 'news/flash')){
    broad$program_category_before_2[i] = 'news'
  }
  if (broad$program_category_after[i] %in% c('weerbericht', 'actualiteiten', 'nieuws', 'news/flash')){
    broad$program_category_after_2[i] = 'news'
  }
  
  # children
  if (broad$program_category_before[i] %in% c('animation serie/cartoon', 'animation film', 'kinderen: non fictie', 'kinderfilms: tekenfilm/animatie/poppen', 'kinderen: amusement')){
    broad$program_category_before_2[i] = 'children'
  }
  if (broad$program_category_after[i] %in% c('animation serie/cartoon', 'animation film', 'kinderen: non fictie', 'kinderfilms: tekenfilm/animatie/poppen', 'kinderen: amusement')){
    broad$program_category_after_2[i] = 'children'
  }
  
  # documentary
  if (broad$program_category_before[i] %in% c('documentary film', 'documentary series')){
    broad$program_category_before_2[i] = 'documentary'
  }
  if (broad$program_category_after[i] %in% c('documentary film', 'documentary series')){
    broad$program_category_after_2[i] = 'documentary'
  }
  
  # not enough observations
  if (broad$program_category_before[i] %in% c('niet opgegeven', 'debate/talk show', 'other studio/structured/show', 'tekstuele informatie', 'cabaret/kleinkunst', 'godsdienst/verkondiging', 'kunst', 'satirisch programma', 'reizen/vakantie/toerisme', 'storing', 'programme trailer', 'other advertising', 'algemene consumenten informatie')){
    broad$program_category_before_2[i] = 'small'
  }
  if (broad$program_category_after[i] %in% c('niet opgegeven', 'debate/talk show', 'other studio/structured/show', 'tekstuele informatie', 'cabaret/kleinkunst', 'godsdienst/verkondiging', 'kunst', 'satirisch programma', 'reizen/vakantie/toerisme', 'storing', 'programme trailer', 'other advertising', 'algemene consumenten informatie')){
    broad$program_category_after_2[i] = 'small'
  }
}
