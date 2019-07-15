

#https://www.youtube.com/watch?v=I_2BBUqa9cY
#https://blog.dominodatalab.com/item-response-theory-r-survey-analysis/
#https://www.youtube.com/watch?v=LiIx_iqd5qU
#https://www.youtube.com/watch?v=1gglVK3a5MA
#https://www.youtube.com/watch?v=SbuQi2xi0os
#https://www.youtube.com/watch?v=N1gSyEB3eYo
#https://www.youtube.com/watch?v=tpU9mKo2AaU&t=2618s



library(ltm)
library(mirt)

df <- read.csv('ICTCourseEvaluation.csv')
df <- df[, -1]

legend <- legend(1, 95, legend=c("P1", "P2", "P3", "P4", "P5"), col=c("red", "blue", "green", "orange", "black"))


polyModel <- mirt(data = df, model = 1, itemtype = "gpcm")
plot(polyModel, type = 'trace', par.settings = simpleTheme(col=c('red', 'blue', 'orange', 'green', 'black')))

itemplot(polyModel, 4, type='info')
plot(polyModel, type = 'info')





#############

grmModel <- grm(df)
plot(grmModel, lwd=2, type='ICC')
plot(grmModel, lwd=2, type='OCCu')

##########
























# # prepare the data
# df$LearningObjectivesMet[which(df$LearningObjectivesMet < 4)] <- 0
# df$LearningObjectivesMet[which(df$LearningObjectivesMet > 3)] <- 1
# 
# df$CourseMaterialsUseful[which(df$CourseMaterialsUseful < 4)] <- 0
# df$CourseMaterialsUseful[which(df$CourseMaterialsUseful > 3)] <- 1
# 
# df$RateQualityOfMaterials[which(df$RateQualityOfMaterials < 4)] <- 0
# df$RateQualityOfMaterials[which(df$RateQualityOfMaterials > 3)] <- 1
# 
# df$RateInstructorKnowledge[which(df$RateInstructorKnowledge < 4)] <- 0
# df$RateInstructorKnowledge[which(df$RateInstructorKnowledge > 3)] <- 1
# 
# df$ContentPresentationClear[which(df$ContentPresentationClear < 4)] <- 0
# df$ContentPresentationClear[which(df$ContentPresentationClear > 3)] <- 1
# 
# df$TimeUsedEffectively[which(df$TimeUsedEffectively < 4)] <- 0
# df$TimeUsedEffectively[which(df$TimeUsedEffectively > 3)] <- 1
# 
# df$AbleToUseKnowledge[which(df$AbleToUseKnowledge < 4)] <- 0
# df$AbleToUseKnowledge[which(df$AbleToUseKnowledge > 3)] <- 1
# 
# df$OverallReaction[which(df$OverallReaction < 4)] <- 0
# df$OverallReaction[which(df$OverallReaction > 3)] <- 1

#create the model
#IRTmodel <- ltm(df ~ z1, IRT.param = TRUE)
#coef(IRTmodel)

df <- df[, -1]

grmModel <- grm(df)

# Test goodness of fit (following standard cutoff of 3.5 for residuals)
margins(grmModel, rule=1)

grmModel$coefficients
print(paste("Model log likelihood:", grmModel$log.Lik))

# Plot item characteristic curves
plot(grmModel, xlab="Theta", cex.main=0.85)

GrmScores <- factor.scores(grmModel)
print(GrmScores)

ThetaTable <- factor.scores(grmModel, resp.patterns = data.frame(lapply(df, as.numeric)))[["score.dat"]]
ThetaTable[["id"]] <- RawSurvey[["id"]]


