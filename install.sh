
#!/bin/bash

make clean

make

# Create the destination directory if it doesn't exist
mkdir -p /usr/local/lib/ela

# Move the contents of ./lib to /usr/local/lib/ela
sudo cp ./lib/* /usr/local/lib/ela/

sudo cp ./source/boilerplate.hpp /usr/local/lib/ela/boilerplate.hpp

sudo ln -sf $(pwd)/bin/ela /usr/local/bin/ela