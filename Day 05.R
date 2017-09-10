
board = data.frame(matrix(data=FALSE, nrow=8, ncol=80))
data = (readLines("05-pixels.txt"))

for (x in 1:length(data)) {

    tempNos = strsplit(gsub("[[:alpha:]]", "", data[x]), " ")[[1]]
    tempCols = (tempNos[tempNos != ""])[2]:(tempNos[tempNos != ""])[1]
    tempRotate = c((tempNos[tempNos != ""])[2], (tempNos[tempNos != ""])[1])

    if (grepl("^(top)", data[x])) {
        board[1, tempCols] = !(board[1, tempCols])

    } else if (grepl("^(rotate column)", data[x])) {
        tempBoard = board[, as.integer(tempRotate[2])]
        for (y in 1:8) {
            if(y + as.integer(tempRotate[1]) > 8) {
                z = (y + as.integer(tempRotate[1]) - 8)
            } else {
                z = y + as.integer(tempRotate[1])
            }
            board[z, as.integer(tempRotate[2])] = tempBoard[y]
        }

    } else if (grepl("^(rotate row)", data[x])) {
        tempBoard = board[as.integer(tempRotate[2]), ]
        for (y in 1:80) {
            if(y + as.integer(tempRotate[1]) > 80) {
                z = (y + as.integer(tempRotate[1]) - 80)
            } else {
                z = y + as.integer(tempRotate[1])
            }
            board[as.integer(tempRotate[2]), z] = tempBoard[y]
        }

    } else {    #left
        board[tempCols, 1] = !(board[tempCols, 1])
    }

}

print(sum(colSums(board)))
