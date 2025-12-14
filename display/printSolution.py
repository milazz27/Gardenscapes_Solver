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

CELL_WIDTH = 5


# ---------------------------------------------------------
# Print a single grid with centered symbols + borders
# ---------------------------------------------------------
def print_grid(grid, title=None):
    rows = len(grid)
    cols = len(grid[0]) if rows > 0 else 0

    if title:
        print(f"\n=== {title} ===")

    # Top border
    print("•" + (" " * (CELL_WIDTH + 2)) * cols + "•\n")

    for row in grid:
        for cell in row:
            symbol = SYMBOLS.get(cell, "+")
            print(" " + symbol.center(CELL_WIDTH), end="")
        print("\n")

    # Bottom border
    print("•" + (" " * (CELL_WIDTH + 2)) * cols + "•\n")


# ---------------------------------------------------------
# Build a grid from ONE state object
# ---------------------------------------------------------
def build_grid(state, width, height):
    pieces = state["pieces"]

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
        r = piece["row"]
        c = piece["col"]
        grid[r][c] = type_map.get(piece["type"], 0)

    return grid


# ---------------------------------------------------------
# Example usage: replay all states
# ---------------------------------------------------------
if __name__ == "__main__":
    with open("solution.json") as json_file:
        data = json.load(json_file)

    width = data["width"]
    height = data["height"]
    states = data["states"]

    for state in states:
        step = state.get("step", "?")
        grid = build_grid(state, width, height)
        print_grid(grid, title=f"Step {step}")
