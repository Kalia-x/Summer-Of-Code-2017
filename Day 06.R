
data = as.list((readLines("06-tours.txt")))
dataX = sapply(data, strsplit, split="")
dirList = c("north", "east", "south", "west")
tourSteps = data.frame()
totalLength = vector()
invalidTours = vector()

for (x in 1:length(dataX)) {    #for each of the tours in the list # length(dataX)

    start = c(0, 0)
    direction = "north"
    tourSteps = data.frame()

    for (y in 1:length(dataX[[x]])) { #for each of the letters in the tour

        if (dataX[[x]][y] == "R") {
            tempDir = grep(direction, dirList) + 1
            if (tempDir > 4) {
                tempDir = tempDir - 4
            }
            direction = dirList[tempDir]
        }

        else if (dataX[[x]][y] == "L") {
            tempDir = grep(direction, dirList) - 1
            if (tempDir < 1) {
                tempDir = tempDir + 4
            }
            direction = dirList[tempDir]
        }

        if (direction == "north") {
            start[2] = start[2] + 1
        } else if (direction == "east") {
            start[1] = start[1] + 1
        } else if (direction == "south") {
            start[2] = start[2] - 1
        } else { #if west
            start[1] = start[1] - 1
        }

        tourSteps[y, 1] = start[1]
        tourSteps[y, 2] = start[2]
    }

    if((sum(duplicated(tourSteps)) == 0) && (tourSteps[nrow(tourSteps), 1] == 0) && (tourSteps[nrow(tourSteps), 2] == 0)) {
        totalLength = c(totalLength, nrow(tourSteps))

    } else if (sum(duplicated(tourSteps)) == 0) {
        invalidTours = c(invalidTours, x)
    }

}

sum(totalLength)
initialLength = totalLength

pairs = data.frame()
for (x in 1:length(invalidTours)) {     #for each value in invalid tours
    for (y in 1:length(invalidTours)) { #matching up against each value in invalid tours
      pairs = rbind(pairs,  c(invalidTours[x], invalidTours[y]))
    }
}
colnames(pairs) = c("x", "y")
pairs = pairs[!(pairs$x == pairs$y), ]

newData = list()
for (x in 1:nrow(pairs)) {
    one = pairs[x, 1]
    two = pairs[x, 2]
    newData[[x]] = c(dataX[one][[1]], dataX[two][[1]])
}

totalLength = vector()
invalidTours = vector()

for (x in 1:length(newData)) {    #for each of the tours in the list # length(newData)

    start = c(0, 0)
    direction = "north"
    tourSteps = data.frame()

    for (y in 1:length(newData[[x]])) { #for each of the letters in the tour

        if (newData[[x]][y] == "R") {
            tempDir = grep(direction, dirList) + 1
            if (tempDir > 4) {
                tempDir = tempDir - 4
            }
            direction = dirList[tempDir]
        }

        else if (newData[[x]][y] == "L") {
            tempDir = grep(direction, dirList) - 1
            if (tempDir < 1) {
                tempDir = tempDir + 4
            }
            direction = dirList[tempDir]
        }

        if (direction == "north") {
            start[2] = start[2] + 1
        } else if (direction == "east") {
            start[1] = start[1] + 1
        } else if (direction == "south") {
            start[2] = start[2] - 1
        } else { #if west
            start[1] = start[1] - 1
        }

        tourSteps[y, 1] = start[1]
        tourSteps[y, 2] = start[2]
    }

    if((sum(duplicated(tourSteps)) == 0) && (tourSteps[nrow(tourSteps), 1] == 0) && (tourSteps[nrow(tourSteps), 2] == 0)) {
        totalLength = c(totalLength, nrow(tourSteps))

    }
}

sum(totalLength)
sum(totalLength) + sum(initialLength)

