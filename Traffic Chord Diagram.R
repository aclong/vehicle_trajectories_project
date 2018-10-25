#################################################################################################################################################################################################################################

#Chord Diagram Visualisation 

library(circlize)

Cars <- read.csv("NGSIM_Peachtree_Vehicle_Trajectories.csv")

ODFlow <- subset(Cars, select =c('O_Zone', 'D_Zone', 'Vehicle_ID'))
ODFlow <- subset(ODFlow,!duplicated(ODFlow$Vehicle_ID))
ODFlow$one <- 1
ODFlow <- aggregate(ODFlow$one, by=list(ODFlow$O_Zone, ODFlow$D_Zone), FUN = sum)
names(ODFlow) <- c("Origin", "Destination", "Frequency") 

write.csv(ODFlow, 'ODFlow.csv', row.names = F)


ODFlowReduced <- subset(ODFlow, Frequency > 1)
ODFlowReduced$Origin <- paste("O", ODFlowReduced$Origin, sep = "")
ODFlowReduced$Destination <- paste("D", ODFlowReduced$Destination, sep = "")


#################################################################################################################################################################################################################################
ODR <- c("D201", "O101", "D202", "O102", "D203", "O103", "D204",  "O104", "D205",  "O105", "D206", "O106", "D207", "O107", "D208",  "O108", "D209", "O109", "D210",  "O110",
  "D211", "O111", "D212", "O112", "D213", "O113", "D214",  "O114", "D215",  "O115", "D216", "O116", "D217", "O117", "D218",  "O118", "D219", "O119", "D220",  "O120", 
  "D221", "O121", "D222", "O122", "D223", "O123")
chordDiagram(ODFlowReduced, order= ODR, 
             grid.col = grid.col, transparency = 0.4, link.border = "black", link.lwd = 0.1, link.sort = T, directional = 1, diffHeight = -uh(2, "mm"), direction.type = c("diffHeight", "arrows"), link.arr.type = "big.arrow",
             annotationTrack = "grid", 
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(ODFlowReduced))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)


grid.col = c(D201 = "#FF0000", D202 = "#3cb44b", D203 ="#ffe119", D204 = "#4363d8", D205 = "#f58231", D206 = "#911eb4", D207 = "#46f0f0", D208 = "#f032e6", D209 = "#bcf60c", D210 = "#fabebe", 
            D211 = "#008080", D212 = "#e6beff", D213 ="#0000FF", D214 = "#ff00ce", D215 = "#fffac8", D216 = "#aaffc3", D217 = "#ffd8b1", D218 = "#808000", D219 = "#000075", D220 = "#808080", 
            D221 = "#00ffdf", D222 = "#000000", D223 ="#4b1b5c",
            O101 = "#FF0000", O102 = "#3cb44b", O103 ="#ffe119", O104 = "#4363d8", O105 = "#f58231", O106 = "#911eb4", O107 = "#46f0f0", O108 = "#f032e6", O109 = "#bcf60c", O110 = "#fabebe", 
            O111 = "#008080", O112 = "#e6beff", O113 ="#0000FF", O114 = "#ff00ce", O115 = "#fffac8", O116 = "#aaffc3", O117 = "#ffd8b1", O118 = "#808000", O119 = "#000075", O120 = "#808080", 
            O121 = "#00ffdf", O122 = "#000000", O123 ="#4b1b5c")
#e6194b
#9a6324

direction.type = "arrows"
circos.par(start.degree  = -90, clock.wise = FALSE)

gap.after = c(rep(5, length(unique(ODFlowReduced[[1]]))-1), 12, 
              rep(5, length(unique(ODFlowReduced[[2]]))-1), 12)                                  
                                  
circos.info()
circos.clear()
#################################################################################################################################################################################################################################

