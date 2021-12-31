###Network Analysis Script: 2020 Election Study#######

#To build data set, need to follow 3 step process
###3 scripts: This one; then map to main data set w/variable creator; then macro vars 

#WD and load data
setwd("C:/Users/diehl/Desktop/Research/Papers/CommDetection/data")
txt = read.csv('news_master.csv', header=T, na.strings=c("","NA"))
txt = txt[4:6]
colnames(txt) = c("n1", "n2", "n3")


#get the audience sample sizes
#txt$text = paste(txt$n1, txt$n2, txt$n3)
#sample = na.omit(txt)#1264 respondents answered all three
#sample = txt
#sample$text = gsub("NA NA NA", "remove", sample$text)
#sample = subset(sample, text!="remove")#1654 answered at least 1 
#rm(sample)
#txt$text = NULL



#Collapse categories 

#local paper
txt$n1 = gsub('advocate|ajc|akronbeaconjournal|americanjournal|annistonstar|arizonadailystar|arizonarepublic|atmoreadvance|bostonglobe|baltimoresun|birminghamnewspaper|bluefielddailytelegraph|castrovalleyforum|charlotteobserver|chattogatimesfreepress|chicago_dailyherald|chicagosuntimes|cincinnatienquire|clarionledger|columbiatimes|columbiatribune|coshoctonbeacon|courierjournal|dailybulletin|dailypantagraph|dailypress|dallasmorning|democratandchronicle|detroit_freepress|duluthtrib|eastbaytimes|evansvillecourierandpress|fayettevilleobserver|gfherald|gilroydispatch|grandrapidspress|hartfordcourant|hawaiitribune|heraldgazette|houstonchronicle|huntsvilletimes|idahostatesman|indianapolisstar|^inquirer$|jacksonsun|kalamazoogazette|kxville|longbeachpresstelegram|lubbockavalanchejournal|lvrj|mercialappeal|messengerinquirer|mewsjournal|miamiherald|milwaukeejournal|minn_star|newarkstar-ledger|ny_daily|omahaworldherald|opelika_auburn|oregonian|orlandosentinel|oxfordeagle|palatkany_daily|patriot|rogersvillereview|salisburypost|sanantonioexpress|sandiegouniontribune|sanfranchronicle|sanjosemercury|saukvalleygazette|seattletimes|skagit|southfloridasunsentinel|staradvertiser|statesman|stlouispost-dispatch|tampabaytimes|times_union|toledoblade|tribtoday|tucsonsentinel|venturacountystar|virginianpilot|westhawaiitoday|wichitaeagle', "local_paper", txt$n1)
txt$n2 = gsub('advocate|ajc|akronbeaconjournal|americanjournal|annistonstar|arizonadailystar|arizonarepublic|atmoreadvance|bostonglobe|baltimoresun|birminghamnewspaper|bluefielddailytelegraph|castrovalleyforum|charlotteobserver|chattogatimesfreepress|chicago_dailyherald|chicagosuntimes|cincinnatienquire|clarionledger|columbiatimes|columbiatribune|coshoctonbeacon|courierjournal|dailybulletin|dailypantagraph|dailypress|dallasmorning|democratandchronicle|detroit_freepress|duluthtrib|eastbaytimes|evansvillecourierandpress|fayettevilleobserver|gfherald|gilroydispatch|grandrapidspress|hartfordcourant|hawaiitribune|heraldgazette|houstonchronicle|huntsvilletimes|idahostatesman|indianapolisstar|^inquirer$|jacksonsun|kalamazoogazette|kxville|longbeachpresstelegram|lubbockavalanchejournal|lvrj|mercialappeal|messengerinquirer|mewsjournal|miamiherald|milwaukeejournal|minn_star|newarkstar-ledger|ny_daily|omahaworldherald|opelika_auburn|oregonian|orlandosentinel|oxfordeagle|palatkany_daily|patriot|rogersvillereview|salisburypost|sanantonioexpress|sandiegouniontribune|sanfranchronicle|sanjosemercury|saukvalleygazette|seattletimes|skagit|southfloridasunsentinel|staradvertiser|statesman|stlouispost-dispatch|tampabaytimes|times_union|toledoblade|tribtoday|tucsonsentinel|venturacountystar|virginianpilot|westhawaiitoday|wichitaeagle', "local_paper", txt$n2)
txt$n3 = gsub('advocate|ajc|akronbeaconjournal|americanjournal|annistonstar|arizonadailystar|arizonarepublic|atmoreadvance|bostonglobe|baltimoresun|birminghamnewspaper|bluefielddailytelegraph|castrovalleyforum|charlotteobserver|chattogatimesfreepress|chicago_dailyherald|chicagosuntimes|cincinnatienquire|clarionledger|columbiatimes|columbiatribune|coshoctonbeacon|courierjournal|dailybulletin|dailypantagraph|dailypress|dallasmorning|democratandchronicle|detroit_freepress|duluthtrib|eastbaytimes|evansvillecourierandpress|fayettevilleobserver|gfherald|gilroydispatch|grandrapidspress|hartfordcourant|hawaiitribune|heraldgazette|houstonchronicle|huntsvilletimes|idahostatesman|indianapolisstar|^inquirer$|jacksonsun|kalamazoogazette|kxville|longbeachpresstelegram|lubbockavalanchejournal|lvrj|mercialappeal|messengerinquirer|mewsjournal|miamiherald|milwaukeejournal|minn_star|newarkstar-ledger|ny_daily|omahaworldherald|opelika_auburn|oregonian|orlandosentinel|oxfordeagle|palatkany_daily|patriot|rogersvillereview|salisburypost|sanantonioexpress|sandiegouniontribune|sanfranchronicle|sanjosemercury|saukvalleygazette|seattletimes|skagit|southfloridasunsentinel|staradvertiser|statesman|stlouispost-dispatch|tampabaytimes|times_union|toledoblade|tribtoday|tucsonsentinel|venturacountystar|virginianpilot|westhawaiitoday|wichitaeagle', "local_paper", txt$n3)

txt$n1 = gsub('^al$|clarkcounty|cleveland|floridatoday|maui|mlive|newlexpatch|njspotlight|ocregister|sfgate|tcpalm|yellowhammer', "local_web", txt$n1)
txt$n2 = gsub('^al$|clarkcounty|cleveland|floridatoday|maui|mlive|newlexpatch|njspotlight|ocregister|sfgate|tcpalm|yellowhammer', "local_web", txt$n2)
txt$n3 = gsub('^al$|clarkcounty|cleveland|floridatoday|maui|mlive|newlexpatch|njspotlight|ocregister|sfgate|tcpalm|yellowhammer', "local_web", txt$n3)


#international media (papers)
txt$n1 = gsub('dailymail|nikkei|eltiempo|chinatimes|independent|ltn.tw|japantoday|hindu|indianexpress|leparisien|peoplesdaily|sunuk', "int_media", txt$n1)
txt$n2 = gsub('dailymail|nikkei|eltiempo|chinatimes|independent|ltn.tw|japantoday|hindu|indianexpress|leparisien|peoplesdaily|sunuk', "int_media", txt$n2)
txt$n3 = gsub('dailymail|nikkei|eltiempo|chinatimes|independent|ltn.tw|japantoday|hindu|indianexpress|leparisien|peoplesdaily|sunuk', "int_media", txt$n3)

#international media (TV and web)
txt$n1 = gsub('telemundo|economist|chinatimes|dailymail|eltiempo|hindu|independent|indianexpress|japantoday|leparisien|ltn.tw|nikkei|peoplesdaily|nta|tvc|aljazeera|cbc|france24|germantagesschau|sky|dailyedge|dailyhunt|guardian', "int_media", txt$n1)
txt$n2 = gsub('telemundo|economist|chinatimes|dailymail|eltiempo|hindu|independent|indianexpress|japantoday|leparisien|ltn.tw|nikkei|peoplesdaily|nta|tvc|aljazeera|cbc|france24|germantagesschau|sky|dailyedge|dailyhunt|guardian', "int_media", txt$n2)
txt$n3 = gsub('telemundo|economist|chinatimes|dailymail|eltiempo|hindu|independent|indianexpress|japantoday|leparisien|ltn.tw|nikkei|peoplesdaily|nta|tvc|aljazeera|cbc|france24|germantagesschau|sky|dailyedge|dailyhunt|guardian', "int_media", txt$n3)


#news magazine
txt$n1 = gsub('atlantic|newyorker|newsweek|forbes|^nation$|^time$|nationalreview|us_news|globalmagazine|rollingstone|fortune|people|wired|vogue|barrons|discover|christianmagazine|gothamist|esquire|^magazine$', "news_mag", txt$n1)
txt$n2 = gsub('atlantic|newyorker|newsweek|forbes|^nation$|^time$|nationalreview|us_news|globalmagazine|rollingstone|fortune|people|wired|vogue|barrons|discover|christianmagazine|gothamist|esquire|^magazine$', "news_mag", txt$n2)
txt$n3 = gsub('atlantic|newyorker|newsweek|forbes|^nation$|^time$|nationalreview|us_news|globalmagazine|rollingstone|fortune|people|wired|vogue|barrons|discover|christianmagazine|gothamist|esquire|^magazine$', "news_mag", txt$n3)

#right-wing echo system 
txt$n1 = gsub('babalyonbee|billoreilly|blaze|^bongireport$|cbn|citizenfreepress|dailycaller|dailyedge|dailywire|daverubenpodcast|drudge|epochtimes|examiner|federalist|jewishworldreview|nationalreview|redstate|rsbn|rush|talkradio|washingtontimes|wendybell|danbongi|infowars|judicialwatch|libertydaily|newsbuz|redpilltv|timesofamerica|truthisscary|wnd|yournation|realclearpolitics|zerohedge|yournation', "right_sphere", txt$n1)
txt$n2 = gsub('babalyonbee|billoreilly|blaze|^bongireport$|cbn|citizenfreepress|dailycaller|dailyedge|dailywire|daverubenpodcast|drudge|epochtimes|examiner|federalist|jewishworldreview|nationalreview|redstate|rsbn|rush|talkradio|washingtontimes|wendybell|danbongi|infowars|judicialwatch|libertydaily|newsbuz|redpilltv|timesofamerica|truthisscary|wnd|yournation|realclearpolitics|zerohedge|yournation', "right_sphere", txt$n2)
txt$n3 = gsub('babalyonbee|billoreilly|blaze|^bongireport$|cbn|citizenfreepress|dailycaller|dailyedge|dailywire|daverubenpodcast|drudge|epochtimes|examiner|federalist|jewishworldreview|nationalreview|redstate|rsbn|rush|talkradio|washingtontimes|wendybell|danbongi|infowars|judicialwatch|libertydaily|newsbuz|redpilltv|timesofamerica|truthisscary|wnd|yournation|realclearpolitics|zerohedge|yournation', "right_sphere", txt$n3)

#fold in fox business
txt$n1 = gsub('fox_business', "fox", txt$n1)
txt$n2 = gsub('fox_business', "fox", txt$n2)
txt$n3 = gsub('fox_business', "fox", txt$n3)

#left-wing echo system
txt$n1 = gsub('bulwark|afa|crookedmedia|dailybeast|dailykos|motherjones|^nation$|occupieddemocrats|peoplesdaily|rawstory|root|slate|townhall|propublica|vox|axios|vice|skim|bet', "left_sphere", txt$n1)
txt$n2 = gsub('bulwark|afa|crookedmedia|dailybeast|dailykos|motherjones|^nation$|occupieddemocrats|peoplesdaily|rawstory|root|slate|townhall|propublica|vox|axios|vice|skim|bet', "left_sphere", txt$n2)
txt$n3 = gsub('bulwark|afa|crookedmedia|dailybeast|dailykos|motherjones|^nation$|occupieddemocrats|peoplesdaily|rawstory|root|slate|townhall|propublica|vox|axios|vice|skim|bet', "left_sphere", txt$n3)

#neutral sphere
txt$n1 = gsub('aol|bloomberg|cdc|cnet|congress.gov|cspan|dailybulletin|dailyhunt|dailypnut|fark|firstamerican|hill|newlexpatch|news_wire|newsday|newser|newsnet|newsy|nta|ny1|podcasts|smart|tcpalm|tvc|us_news|webmd|whitehouse.gov', "neutral_sphere", txt$n1)
txt$n2 = gsub('aol|bloomberg|cdc|cnet|congress.gov|cspan|dailybulletin|dailyhunt|dailypnut|fark|firstamerican|hill|newlexpatch|news_wire|newsday|newser|newsnet|newsy|nta|ny1|podcasts|smart|tcpalm|tvc|us_news|webmd|whitehouse.gov', "neutral_sphere", txt$n2)
txt$n3 = gsub('aol|bloomberg|cdc|cnet|congress.gov|cspan|dailybulletin|dailyhunt|dailypnut|fark|firstamerican|hill|newlexpatch|news_wire|newsday|newser|newsnet|newsy|nta|ny1|podcasts|smart|tcpalm|tvc|us_news|webmd|whitehouse.gov', "neutral_sphere", txt$n3)

#remove entertainment
txt$n1 = gsub('tmz|entertainment|streaming', "", txt$n1)
txt$n2 = gsub('tmz|entertainment|streaming', "", txt$n2)
txt$n3 = gsub('tmz|entertainment|streaming', "", txt$n3)

#Text Analysis + Network Structure
library(quanteda)
library(quanteda.textstats)
library(igraph)
#drop other vars and merge fields
txt$text = paste(txt$n1, txt$n2, txt$n3)
txt$text = gsub("NA", '', txt$text)

#create document matrix 
news_dfm <- dfm(txt$text)
#inspect the dfm
head(news_dfm) 
textstat_frequency(news_dfm)


#list = textstat_frequency(news_dfm)
#write.csv(list, "test_list.csv")

#Network analysis
#create the co-occurrence matrix
tag_fcm <- fcm(news_dfm, count = c("frequency"))
head(tag_fcm)

#pull top organizations Total (in this case we have less than 100 so it takes them all)
toptag <- names(topfeatures(news_dfm, 100))
toptag 

#pull from the co-occurrence matrix based on the filter
toptag_fcm <- fcm_select(tag_fcm, pattern = toptag)
toptag_fcm

#convert the co-occurrence to a matrix for export to igraph
df = as.matrix(toptag_fcm)
cor(df)

##########################
#Create the graph
library(igraph)
net <- graph_from_adjacency_matrix(df, mode = "undirected", diag = F, weighted = T) #diag = self loops (T/F)
class(net)
net

#Drop edges based on weight
net <- delete.edges(net, E(net)[weight <= 1]) #drop edges of specified weight
net = delete.vertices(net, degree(net)==0) #drop vertices w/degree of 0
net

#net = delete.vertices(net, c("streaming", "entertainment"))

plot(net)

#Exploratory Plots#
#Adjust graphs
plot(net, layout=layout_with_fr, vertex.size=2,
     vertex.label.dist=.8, vertex.color="red", edge.arrow.size=0.1)

#check the attributes
E(net)#Edges
V(net)#Vertices
E(net)$weight#number of ties for each link
strength(net)#weighted sum of ties
edge_density(net)
farthest_vertices(net)
get_diameter(net)
mean(degree(net))
sort(degree(net))

#Cluster analysis
#Communities
#https://igraph.org/r/doc/communities.html
#Create/annotate graph object separately then run analysis
net1 = net

#Various clustering algorithms, used louvian

# cluster walktrap (more precision but more groups)
wc <- cluster_walktrap(net1)
#Attributes
modularity(wc)
membership(wc)
length(wc)
sizes(wc)
algorithm(wc)
merges(wc)
crossing(wc, net1)
code_len(wc)

plot(wc, net1, vertex.label.dist=0.4)

#2 louvain (reliable, but less precision than walktrap)
wc <- cluster_louvain(net1)
modularity(wc)
membership(wc)
length(wc)
sizes(wc)
crossing(wc, net1)
plot(wc, net1, vertex.label.dist=0.4)

#3 spinglass (least stable, but more precision than louvian)
wc <- spinglass.community(net1)
modularity(wc)
length(wc)
membership(wc)
sizes(wc)
crossing(wc, net1)
plot(wc, net1, vertex.label.dist=0.4)

#Map clusters to data############
#################################
#map to text data then save file and run in the variable creator
#need to transform results from cluster analysis into df
d2 = as.list(membership(wc)) #need to convert to list first
d2 = as.data.frame(do.call(rbind, d2))#create the df
d2$labels = rownames(d2) #assign labels
#clean df
d2$comm = d2$V1
d2$V1 = NULL
rownames(d2) = NULL
d2


map = setNames(d2$comm, d2$labels)
txt$cluster.1 = as.numeric(map[txt$n1])
txt$cluster.2 = as.numeric(map[txt$n2])
txt$cluster.3 = as.numeric(map[txt$n3])

#save to same folder as variable creator/data & run
#setwd("C:/Users/diehl/Desktop/Research/Survey F20/EMCP20/EMCP20_/Data")
#save(txt, file = 'cluster.analysis.Rdata')
