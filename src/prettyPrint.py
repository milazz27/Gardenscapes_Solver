import json

# ---------------------------------------------------------
# SYMBOL MAP
# ---------------------------------------------------------
SYMBOLS = {
    0: "🔳",
    1: "🍀",
    2: "🌸",
    3: "🍎",
    4: "🫐",
    5: "💧",
    6: "🍹",
}

CELL_WIDTH = 5   # adjust this to make spacing wider or tighter


# ---------------------------------------------------------
# Print a single grid with centered symbols + borders
# ---------------------------------------------------------
def print_grid(grid, title=None):
    rows = len(grid)
    cols = len(grid[0]) if rows > 0 else 0

    if title:
        print(f"\n=== {title} ===")

    # Top border
    print("\'" + (" " * (CELL_WIDTH + 2)) * cols + "\'", end="\n\n")

    for row in grid:
        for cell in row:
            symbol = SYMBOLS.get(cell, "+")
            print(" ", end="")
            print(symbol.center(CELL_WIDTH), end="")
        print("\n")
    
    # Bottom border
    print("\'" + (" " * (CELL_WIDTH + 2)) * cols + "\'\n")


# ---------------------------------------------------------
# Build a grid from a level JSON object
# ---------------------------------------------------------
def build_grid(level):
    width = level["width"]
    height = level["height"]
    pieces = level["pieces"]

    grid = [[0 for _ in range(width)] for _ in range(height)]

    type_map = {
        "leaf": 1,
        "flower": 2,
        "apple": 3,
        "berry": 4,
        "water": 5,
        "lemonade": 6,
    }

    for piece in pieces:
        x = piece["row"]
        y = piece["col"]
        grid[x][y] = type_map.get(piece["type"], 0)

    return grid


# ---------------------------------------------------------
# Example usage
# ---------------------------------------------------------
if __name__ == "__main__":
    with open("../test/p1.json") as json_file:
        level = json.load(json_file)

    grid = build_grid(level)
    print_grid(grid, title="Level p3")