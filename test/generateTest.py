# ---------------------------------------------------------
# Mapping from text input -> Prolog type
# ---------------------------------------------------------
TYPE_MAP = {
    "leaf":      "obstacle(leaf)",
    "flower":    "obstacle(flower)",
    "apple":     "obstacle(apple)",
    "berry":     "obstacle(berry)",
    "water":     "obstacle(water)",
    "lemonade":  "object(lemonade)",
    "empty":     "none(empty)"
}


# ---------------------------------------------------------
# Read cell types from a file (one per line)
# ---------------------------------------------------------
def read_cell_types(filename):
    with open(filename, "r") as f:
        lines = [line.strip() for line in f.readlines() if line.strip()]
    return lines


# ---------------------------------------------------------
# Generate rows and cols as strings
# ---------------------------------------------------------
def generate_rows_and_cols_text(height, width):
    out = []

    # rows
    out.append("rows([")
    for r in range(height):
        row = [f"pos({r},{c})" for c in range(width)]
        comma = "," if r < height - 1 else ""
        out.append(f"    [{', '.join(row)}]{comma}")
    out.append("]).\n")

    # cols
    out.append("cols([")
    for c in range(width):
        col = [f"pos({r},{c})" for r in range(height)]
        comma = "," if c < width - 1 else ""
        out.append(f"    [{', '.join(col)}]{comma}")
    out.append("]).\n")

    return "\n".join(out)


# ---------------------------------------------------------
# Format Prolog cell declarations as a string
# ---------------------------------------------------------
def format_state_text(name, height, width, cell_types):
    out = [f"{name}(["]

    idx = 0
    total = height * width

    for r in range(height):
        for c in range(width):
            cell_type = cell_types[idx]
            idx += 1

            prolog_term = TYPE_MAP.get(cell_type, f"obstacle({cell_type})")
            comma = "," if idx < total else ""
            out.append(f"    cell({prolog_term}, pos({r},{c})){comma}")
        out.append("")  # blank line between rows

    out.append("]).\n")
    return "\n".join(out)


# ---------------------------------------------------------
# Count lemonade objects
# ---------------------------------------------------------
def count_lemonades(cell_types):
    return sum(1 for t in cell_types if t == "lemonade")


# ---------------------------------------------------------
# Write output to a .pl file
# ---------------------------------------------------------
def write_to_file(filename, text):
    with open(filename, "w") as f:
        f.write(text)
    print(f"\n✅ Successfully wrote Prolog file: {filename}\n")


# ---------------------------------------------------------
# Main interactive program
# ---------------------------------------------------------
if __name__ == "__main__":
    print("Enter grid dimensions:")
    height = int(input("Height: "))
    width = int(input("Width: "))

    filename = input("Input file with cell types (one per line): ")
    cell_types = read_cell_types(filename)

    if len(cell_types) != height * width:
        print("ERROR: Number of lines in file does not match height*width")
        exit(1)

    # Ask for output .pl filename
    output_file = input("Output Prolog filename (e.g., level3.pl): ")

    # Build Prolog text
    lemonade_count = count_lemonades(cell_types)

    prolog_text = []
    prolog_text.append(f"width({width}).")
    prolog_text.append(f"height({height}).\n")
    prolog_text.append(f"total_objects({lemonade_count}).\n")

    prolog_text.append(generate_rows_and_cols_text(height, width))

    state_name = input("State name (e.g., state3): ")
    prolog_text.append(format_state_text(state_name, height, width, cell_types))

    final_output = "\n".join(prolog_text)

    # Print to console
    print("\nGenerated Prolog Output:\n")
    print(final_output)

    # Write to file
    write_to_file(output_file, final_output)