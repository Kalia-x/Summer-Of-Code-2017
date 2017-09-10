
pass = "the traveller in the grey riding-coat, who called himself mr. melville, was contemplating the malice of which the gods are capable."

pass = gsub(" ", "", pass)
pass = gsub("[[:punct:]]", "", pass)
pass = strsplit(pass, "")[[1]]

currentCode = c(grep(pass[1], letters), grep(pass[2], letters))


for(x in 3:length(pass)) {

    working = currentCode[1] + currentCode[2]
    if(working > 26){
        partOne = letters[working-26]
    } else {
        partOne = letters[working]
    }
    tempCode = partOne

    workingToo = grep(pass[x], letters) + currentCode[2]
    if(workingToo > 26){
        partTwo = letters[workingToo-26]
    } else {
        partTwo = letters[workingToo]
    }
    tempCode = c(tempCode, partTwo)

    currentCode = c(grep(tempCode[1], letters), grep(tempCode[2], letters))
}

print(c(letters[currentCode[1]], letters[currentCode[2]]))

#######################

pass = "the traveller in the grey riding-coat, who called himself mr. melville, was contemplating the malice of which the gods are capable."

pass = gsub(" ", "", pass)
pass = gsub("[[:punct:]]", "", pass)
pass = strsplit(pass, "")[[1]]

currentCode = c(18, 9)

for(x in 1:length(pass)) {

    working = currentCode[2] * 5 + currentCode[1]
    while (working > 26) {
        working = working - 26
    }
    tempCode = letters[working]

    workingTwo = grep(pass[x], letters) * 11 + currentCode[2]
    while (workingTwo > 26) {
        workingTwo = workingTwo - 26
    }

    tempCode = c(tempCode, letters[workingTwo])
    currentCode = c(grep(tempCode[1], letters), grep(tempCode[2], letters))
}

print(c(letters[currentCode[1]], letters[currentCode[2]]))
