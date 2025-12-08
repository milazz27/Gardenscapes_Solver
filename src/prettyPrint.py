import json

def printGrid(grid):
    for row in grid:
        for cell in row:
            if cell == 1:
                print(" 🍀 ", end = "")
            elif cell == 2:
                print(" 🌸 ", end = "")
            elif cell == 3:
                print(" 🍎 ", end = "")
            elif cell == 4:
                print(" 🫐 ", end = "")
            elif cell == 5:
                print(" 💧 ", end = "")
            elif cell == 6:
                print(" 🍹 ", end = "")
            else:
                print(" • ", end = "")
        print("\n")



with open("out.json") as json_file:
    level = json.load(json_file)

width = level["width"]
height = level["height"]
pieces = level["pieces"]

grid = [[0 for _ in range(width)] for _ in range(height)]

for piece in pieces:
    y = piece["col"]
    x = piece["row"]
    type = piece["type"]

    if type == "leaf":
        grid[x][y] = 1
    elif type == "flower":
        grid[x][y] = 2
    elif type == "apple":
        grid[x][y] = 3
    elif type == "berry":
        grid[x][y] = 4
    elif type == "water":
        grid[x][y] = 5
    elif type == "lemonade":
        grid[x][y] = 6
    else:
        grid[x][y] = 0

printGrid(grid)
