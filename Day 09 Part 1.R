
bills = readLines("09-bills.txt")
match = 0

for (x in 1:length(bills)) {
    bills[x] = gsub(".*: ", "", bills[x])
}

myBill = strsplit(bills[1], split="")[[1]]
otherBills = strsplit(bills[2:length(bills)], split="")

for(x in 1:length(otherBills)) {

    if (length(otherBills[[x]]) < length(myBill)) {
        next
    }
    myNo = 1
    othNo = 1
    repeat {
        if(myBill[myNo] == otherBills[[x]][othNo]) {
            myNo = myNo + 1
        }
        othNo = othNo + 1
        if(othNo > length(otherBills[[x]]) | myNo > length(myBill)) {
            break
        }
    }
    if(myNo == 1 + length(myBill)) {
        match = match + 1
    }
}

print(paste(match, "matched bills"))
