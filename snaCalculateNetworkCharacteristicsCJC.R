##calculate the network measures of the graphs created earlier
t2<-networkMeasures(iGraphList_t2,26)
t2_egoRemoved<-networkMeasures(iGraphList_egoRemoved,26)
##corrrect for the fact that removing the egoID will mess up the engagement column, I just copy
#the previous data into this data
t2_egoRemoved$Engagement=t2$Engagement
egoNetwork<-networkMeasures(iGraphList_egoID,26)
egoNetworkEgoRemoved<-networkMeasures(iGraphList_egoNetwork_egoRemovedID,26)
personalNetwork<-networkMeasuresPersonal(iGraphList_personalNets,26)


avet2<-networkMeasuresAverage(t2,10)
avet2_egoRemoved<-networkMeasuresAverage(t2_egoRemoved,10)
avet2_egoNetwork<-networkMeasuresAverage(egoNetwork,10)
avet2_egoNetworkEgoRemoved<-networkMeasuresAverage(egoNetworkEgoRemoved,10)
avet2_personalNetwork<-networkMeasuresAverage(personalNetwork,10)

#split the network by engagement
split_t2t<-split(t2,t2$Engagement=='engaged')
split_egoRemoved<-split(t2_egoRemoved,t2_egoRemoved$Engagement=='engaged')
split_egoNetworkegoRemoved<-split(egoNetworkEgoRemoved,egoNetworkEgoRemoved$Engagement=='engaged')
split_egoNetwork<-split(egoNetwork,egoNetwork$Engagement=='engaged')
split_personalNetwork<-split(personalNetwork,personalNetwork$Engagement=='engaged')



avet2Engaged<-networkMeasuresAverage(split_t2t$'TRUE',10)
avet2Instrumental<-networkMeasuresAverage(split_t2t$'FALSE',10)
avet2_egoRemovedEngaged<-networkMeasuresAverage(split_egoRemoved$'TRUE',10)
avet2_egoRemovedInstrumental<-networkMeasuresAverage(split_egoRemoved$'FALSE',10)
avet2_egoNetworkEngaged<-networkMeasuresAverage(split_egoNetwork$'TRUE',10)
avet2_egoNetworkInstrumental<-networkMeasuresAverage(split_egoNetwork$'FALSE',10)
avet2_egoNetworkEgoRemovedEngaged<-networkMeasuresAverage(split_egoNetworkegoRemoved$'TRUE',10)
avet2_egoNetworkEgoRemovedInstrumental<-networkMeasuresAverage(split_egoNetworkegoRemoved$'FALSE',10)
avet2_personalNetworkEngaged<-networkMeasuresAverage(split_personalNetwork$'TRUE',10)
avet2_personalNetworkInstrumental<-networkMeasuresAverage(split_personalNetwork$'FALSE',10)





write.xlsx(t2, "FullNetwork_t2.xlsx")
write.xlsx(t2_egoRemoved, "FullNetwork_t2_egoRemoved.xlsx")
write.xlsx(egoNetworkEgoRemoved, "egoNetwork_egoRemoved_t2.xlsx")
write.xlsx(egoNetwork, "egoNetwork_t2.xlsx")
write.xlsx(personalNetwork, "personalNetwork_t2.xlsx")

write.xlsx(avet2, "average_t2_FullNetwork.xlsx")
write.xlsx(avet2_egoRemoved, "average_t2_FullNetwork_egoRemoved.xlsx")
write.xlsx(avet2_egoNetworkEgoRemoved, "average_t2_egoNetwork_egoRemoved.xlsx")
write.xlsx(avet2_egoNetwork, "average_t2_egoNetwork.xlsx")
write.xlsx(avet2_personalNetwork, "average_t2_personalNetwork.xlsx")

write.xlsx(avet2Engaged, "average_t2_FullNetwork_Engaged.xlsx")
write.xlsx(avet2Instrumental, "average_t2_FullNetwork_Instrumental.xlsx")
write.xlsx(avet2_egoRemovedEngaged, "average_t2_FullNetwork_egoRemoved_Engaged.xlsx")
write.xlsx(avet2_egoRemovedInstrumental, "average_t2_FullNetwork_egoRemoved_Instrumental.xlsx")

write.xlsx(avet2_egoNetworkEgoRemovedEngaged, "average_t2_egoNetwork_egoRemoved_Engaged.xlsx")
write.xlsx(avet2_egoNetworkEgoRemovedInstrumental, "average_t2_egoNetwork_egoRemoved_Instrumental.xlsx")

write.xlsx(avet2_egoNetworkEngaged, "average_t2_egoNetwork_Engaged.xlsx")
write.xlsx(avet2_egoNetworkInstrumental, "average_t2_egoNetwork_Instrumental.xlsx")
write.xlsx(avet2_personalNetworkEngaged, "average_t2_personalNetwork_Engaged.xlsx")
write.xlsx(avet2_personalNetworkInstrumental, "average_t2_personalNetwork_Instrumental.xlsx")




