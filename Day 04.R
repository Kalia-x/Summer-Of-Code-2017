
library(dplyr)
library(gtools)

maze = (readLines("04-lines.txt"))
maze = gsub("[[:punct:]]", "", maze)
mazeEdit = lapply(maze, strsplit, split=" ")

final = vector()

for (position in 0:25) {
    for(x in 1:length(mazeEdit)) {
        if(sum(mazeEdit[[x]][[1]] == as.character(position)) == 1) {
            position = grep(paste("^", position, "$", sep=""), mazeEdit[[x]][[1]], invert=TRUE, value=TRUE)
        }
    }
    final = c(final, position)
}

names(final) = letters[1:26]
final = mixedsort(final)
paste0(names(final), sep="", collapse="")

#################

tempstep = "test"
step = 0

for(x in 1:length(mazeEdit)) {
    tempStep = c(tempStep, mazeEdit[[x]][[1]])
    if (!(length(unique(tempStep)) == length(tempStep))) {
        step = step + 1
    }
}

print(step-1)
