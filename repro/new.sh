
#!/bin/bash
# Path to the counter file
COUNTER_FILE="counter"

# Initialize the counter file if it does not exist
if [ ! -f "$COUNTER_FILE" ]; then
  echo 0 > "$COUNTER_FILE"
fi

# Read the current counter value
ID=$(cat "$COUNTER_FILE")

# Increment the counter
NEW_ID=$((ID + 1))

# Write the new counter value back to the counter file
echo $NEW_ID > "$COUNTER_FILE"

# Create a new file with the incremented ID
NEW_FILE="${NEW_ID}.ela"

echo "main :: fn() { }" >> "$NEW_FILE"

echo "Created new file: $NEW_FILE"