
import = (readLines("07-program.txt"))
import = strsplit(import, " ")

reg = data.frame(7, 0, 0, 0)
colnames(reg) = c("a", "b", "c", "d")
pc = 1

repeat {

    if (pc > length(import)) {
        break
    }

    tempCmd = import[[pc]]

    if(tempCmd[1] == "inc") {
        reg[, tempCmd[2]] = reg[ , tempCmd[2]] + 1
        pc = pc + 1

    } else if(tempCmd[1] == "dec") {
        reg[, tempCmd[2]] = reg[ , tempCmd[2]] - 1
        pc = pc + 1

    } else if(tempCmd[1] == "set") {
        reg[, tempCmd[2]] = as.integer(tempCmd[3])
        pc = pc + 1

    } else if(tempCmd[1] == "cpy") {
        reg[, tempCmd[3]] = reg[ , tempCmd[2]]
        pc = pc + 1

    } else if(tempCmd[1] == "jmp") {
        pc = pc + as.integer(tempCmd[2])

    } else { #jpz
        if (reg[, tempCmd[2]] == 0) {
            pc = pc + as.integer(tempCmd[3])
        } else {
            pc = pc + 1
        }

    }
}

print(reg$a)

