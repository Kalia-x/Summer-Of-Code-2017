
floor = 0
maxFloor = 0

text = readLines("02-lifts.txt")
text = strsplit(text, "")[[1]]

for(x in 1:length(text)) {
    if(text[x] == "^") {
        floor = floor + 1
    } else if (text[x] == "v") {
        floor = floor - 1
    } else if (text[x] == "=") {
        if (floor > maxFloor) {
            maxFloor = floor
        }
    }
}

print(maxFloor)
print(floor)
