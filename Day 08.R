
import = readLines("08-rooms.txt")
wordsList = list()
wordsList[[1]] = "coax"
inc = 1

# for (a in 1:10) {
repeat {

    words = vector()

        for (x in 1:length(wordsList[[inc]])) {  #for each word in the previous words list

            for (y in 1:length(import)) { #compare to each word in import

                tempOne = strsplit(wordsList[[inc]][x], "")[[1]]
                tempTwo = strsplit(import[y], "")[[1]]
                temporary = 0

                for (z in 1:4) {
                    if(tempOne[z] == tempTwo[z]) {
                        temporary = temporary + 1
                    }
                }
                if (temporary == 3 && length(intersect(import[y], wordsList[[inc]])) == 0 && import[y] != wordsList[[1]]) {
                    words = c(words, import[y])
                }
            }
        }

    wordsList[[inc+1]] = unique(words)

    if (sum(grepl("stay", wordsList[[inc+1]])) == 1) {
        print(inc+1)
        break
    }
    inc = inc + 1
}

# final = c(wordsList[[2]], wordsList[[3]], wordsList[[4]], wordsList[[5]], wordsList[[6]], wordsList[[7]], wordsList[[8]], wordsList[[9]], wordsList[[10]], wordsList[[11]])
# print(length(unique(final)))
