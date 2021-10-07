import re

grammer = [{"name": "Ident", "re": "[_a-zA-Z][_0-9a-zA-Z]*"}, {"name": "Number", "re": "[0-9]+"}]
reserve_word = {}
with open("grammer_input") as file:
    for line in file.readlines():
        x = line.split()
        grammer.append({"name": x[1], "re": re.escape(x[0])})
with open("reserve_word") as file:
    for line in file:
        reserve_word[x[1]] = x[0]


with open("program") as file:
    program_input = file.read()

start_idx = 0
while start_idx < len(program_input):
    # remove space
    if program_input[start_idx].isspace():
        start_idx += 1
    else:
    # check
        for token in grammer:
            match = re.search(r"^" + token["re"], program_input[start_idx:])   
            if match != None:
                ans = match.group()
                if ans in reserve_word.keys():
                    print(reserve_word[ans]) 
                else:
                    print(
                        token["name"],
                        "(" + ans + ")"
                        if token["name"] == "Ident" or token["name"] == "Number"
                        else "",
                        sep="",
                    )
                    start_idx += len(ans)
                    break
        if match == None:
            print("Err")
            exit(0)