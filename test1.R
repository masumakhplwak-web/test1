dfitc255=read.csv("itc255.csv")
View(dfitc255)
names(dfitc255)

View(dfitc255)
#Variable 1 Skill Level
AbsFreq=table(dfitc255$SkillLevel)
AbsFreq
prop.table(AbsFreq)
RelFreq=round(prop.table(AbsFreq), 2)
RelFreq
CumFreq=cumsum(RelFreq)
CumFreq
FDTSkillLevel=cbind(AbsFreq,RelFreq,CumFreq)

FDTQL=function(x) {
  AbsFreq=table(x)
  RelFreq=round(prop.table(AbsFreq), 2)
  CumFreq=cumsum(RelFreq)
  FDTx= cbind(AbsFreq,RelFreq,CumFreq)
  return(FDTx)
  
}
FDTQL(dfitc255$SkillLevel)

#Variable 2 Gender
AbsFreq=table(dfitc255$Gender)
AbsFreq
prop.table(AbsFreq)
RelFreq=round(prop.table(AbsFreq), 2)
RelFreq
CumFreq=cumsum(RelFreq)
CumFreq
FDTSGender=cbind(AbsFreq,RelFreq,CumFreq)

FDTQL=function(x) {
  AbsFreq=table(x)
  RelFreq=round(prop.table(AbsFreq), 2)
  CumFreq=cumsum(RelFreq)
  FDTx= cbind(AbsFreq,RelFreq,CumFreq)
  return(FDTx)
  
}
FDTQL(dfitc255$Gender)
