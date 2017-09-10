
data = readLines("10-wordsearch.txt")
searchWords = data[102:length(data)]
foundWords = ""
horiLetters = list()
horiReverse = list()
vertLetters = list()
vertReverse = list()
horiLetters = strsplit(data[2:101], split="")
gridLetters = horiLetters
grid = as.data.frame(matrix(nrow=100, ncol=100))
gridRemove = grid

for(x in 1:100) {
    grid[x, ] = horiLetters[[x]]
}
for (x in 1:length(horiLetters)) {
    horiLetters[[100 + x]] = rev(horiLetters[[x]])
}
horiRemove = horiLetters
for (x in 1:200) {
    horiLetters[[x]] = paste(horiLetters[[x]], collapse="")
}

for(y in 1:length(searchWords)) {
    for(x in 1:length(horiLetters)) {
        if (sum(gregexpr(searchWords[y], horiLetters[[x]])[[1]]) != -1) {
            foundWords = c(foundWords, searchWords[y])
            horiRemove[[x]][(sum(gregexpr(searchWords[y], horiLetters[[x]])[[1]])):(sum(gregexpr(searchWords[y], horiLetters[[x]])[[1]])+(nchar(searchWords[y])-1))] = NA
        }
    }
}

for (x in 1:ncol(grid)) {
    vertLetters[[x]] = grid[ , x]
}
for (x in 1:length(vertLetters)) {
    vertLetters[[100 + x]] = rev(vertLetters[[x]])
}
vertRemove = vertLetters
for (x in 1:200) {
    vertLetters[[x]] = paste(vertLetters[[x]], collapse="")
}

for(y in 1:length(searchWords)) {
    for(x in 1:length(vertLetters)) {
        if (sum(gregexpr(searchWords[y], vertLetters[[x]])[[1]]) != -1) {
            foundWords = c(foundWords, searchWords[y])
            vertRemove[[x]][(sum(gregexpr(searchWords[y], vertLetters[[x]])[[1]])):(sum(gregexpr(searchWords[y], vertLetters[[x]])[[1]])+(nchar(searchWords[y])-1))] = NA

        }
    }
}

numMatrix = matrix(1:10000, 100, byrow=TRUE)
gridLetters = unlist(gridLetters)
diagOne = row(numMatrix) - col(numMatrix)
diagOne = split(numMatrix, diagOne)
diagTwo =  row(numMatrix) + col(numMatrix)
diagTwo = split(numMatrix, diagTwo)

for (x in 1:length(diagOne)) {
    for(y in 1:length(diagOne[[x]])) {
        diagOne[[x]][y] = gridLetters[as.numeric(diagOne[[x]][y])]
    }
}
for (x in 1:length(diagTwo)) {
    for(y in 1:length(diagTwo[[x]])) {
        diagTwo[[x]][y] = gridLetters[as.numeric(diagTwo[[x]][y])]
    }
}

for (x in 1:length(diagOne)) {
    diagOne[[199 + x]] = rev(diagOne[[x]])
}
diagOneRemove = diagOne
for (x in 1:length(diagOne)) {
    diagOne[[x]] = paste(diagOne[[x]], collapse="")
}
for (x in 1:length(diagTwo)) {
    diagTwo[[199 + x]] = rev(diagTwo[[x]])
}
diagTwoRemove = diagTwo
for (x in 1:length(diagTwo)) {
    diagTwo[[x]] = paste(diagTwo[[x]], collapse="")
}

for(y in 1:length(searchWords)) {
    for(x in 1:length(diagOne)) {
        if (sum(gregexpr(searchWords[y], diagOne[[x]])[[1]]) != -1) {
            foundWords = c(foundWords, searchWords[y])
            diagOneRemove[[x]][(sum(gregexpr(searchWords[y], diagOne[[x]])[[1]])):(sum(gregexpr(searchWords[y], diagOne[[x]])[[1]])+(nchar(searchWords[y])-1))] = NA
        }
    }
}
for(y in 1:length(searchWords)) {
    for(x in 1:length(diagTwo)) {
        if (sum(gregexpr(searchWords[y], diagTwo[[x]])[[1]]) != -1) {
            foundWords = c(foundWords, searchWords[y])
            diagTwoRemove[[x]][(sum(gregexpr(searchWords[y], diagTwo[[x]])[[1]])):(sum(gregexpr(searchWords[y], diagTwo[[x]])[[1]])+(nchar(searchWords[y])-1))] = NA

        }
    }
}

lengthWords = paste(foundWords[2:length(foundWords)], collapse="")
nchar(lengthWords)

for(x in 1:100) {
    gridRemove[x, ] = horiRemove[[x]]
}
horiForwardNAs = is.na(gridRemove)

for (x in 1:100) {
    gridRemove[, x] = vertRemove[[x]]
}
vertForwardNAs = is.na(gridRemove)

for (x in 101:length(horiRemove)) {
    horiRemove[[x]] = rev(horiRemove[[x]])
}
for(x in 101:length(horiRemove)) {
    gridRemove[x-100, ] = horiRemove[[x]]
}
horiBackwardNAs = is.na(gridRemove)

for (x in 101:length(vertRemove)) {
    vertRemove[[x]] = rev(vertRemove[[x]])
}
for(x in 101:length(vertRemove)) {
    gridRemove[, x-100] = vertRemove[[x]]
}
vertBackwardNAs = is.na(gridRemove)

diagonalOne = row(numMatrix) - col(numMatrix)
diagonalOne = split(numMatrix, diagonalOne)
for (x in 1:199) {
    for (y in 1:length(diagonalOne[[x]])) {
        gridRemove[numMatrix==diagonalOne[[x]][y]] = diagOneRemove[[x]][y]
    }
}
diagOneForwardNas = is.na(gridRemove)

diagonalTwo =  row(numMatrix) + col(numMatrix)
diagonalTwo = split(numMatrix, diagonalTwo)
for (x in 1:199) {
    for (y in 1:length(diagonalTwo[[x]])) {
        gridRemove[numMatrix==diagonalTwo[[x]][y]] = diagTwoRemove[[x]][y]
    }
}
diagTwoForwardNas = is.na(gridRemove)

for (x in 200:length(diagOneRemove)) {
    diagOneRemove[[x]] = rev(diagOneRemove[[x]])
}

diagOneRemoveBack = diagOneRemove[200:length(diagOneRemove)]

for(x in 1:length(diagOneRemoveBack)) {
    for (y in 1:length(diagOneRemoveBack[[x]])) {
        gridRemove[(numMatrix==diagonalOne[[x]][y])] = diagOneRemoveBack[[x]][y]
    }
}
diagOneBackwardNAs = is.na(gridRemove)

for (x in 200:length(diagTwoRemove)) {
    diagTwoRemove[[x]] = rev(diagTwoRemove[[x]])
}

diagTwoRemoveBack = diagTwoRemove[200:length(diagTwoRemove)]

for(x in 1:length(diagTwoRemoveBack)) {
    for (y in 1:length(diagTwoRemoveBack[[x]])) {
        gridRemove[(numMatrix==diagonalTwo[[x]][y])] = diagTwoRemoveBack[[x]][y]
    }
}
diagTwoBackwardNAs = is.na(gridRemove)

wordsGone = vertForwardNAs + vertBackwardNAs + horiForwardNAs + horiBackwardNAs + diagTwoForwardNas + diagTwoBackwardNAs + diagOneForwardNas + diagOneBackwardNAs
finalLetters = grid[wordsGone==0]
finalLetters = table(finalLetters)
finalLetters = finalLetters[names(finalLetters) == "a" | names(finalLetters) == "e" | names(finalLetters) == "i" | names(finalLetters) == "o" | names(finalLetters) == "u"]

sum(finalLetters)
