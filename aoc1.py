WORDS = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
DIGIT_WORDS = {
    w: str(i + 1) for i, w in enumerate(WORDS)
}

def get_number_or_none(line, i):
    if line[i].isdigit():
        return line[i]
    for w in WORDS:
        word_len = len(w)
        offset = (i + word_len)
        if i < 0:
            offset = (len(line) + i) + word_len
        if line[i:offset] == w:
            return DIGIT_WORDS[w]
            print(">> %s" % w)

    return None

with open("1.txt") as f:
    input = f.read()

    sum = 0
    for line in input.split("\n"):
        if len(line) == 0:
            continue
        first_n = None
        last_n = None

        i = 0
        while i < len(line):
            first_n = get_number_or_none(line, i)
            if first_n:
                break
            i += 1
        i = -1
        while i >= -len(line):
            last_n = get_number_or_none(line, i)
            if last_n:
                break
            i -= 1

        print(">> %s %s | %s" % (first_n, last_n, line))
        sum += int(first_n + last_n)
    print(sum)
