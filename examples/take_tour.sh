echo starting 'target practice'...
cd target_practice > /dev/null
./run.sh

echo done. starting 'sequencer'...
cd ../sequencer > /dev/null
./run.sh

echo done. starting 'dynamic library', a hot reload-able test using raylib.
cd ../dynamic_library  > /dev/null
./run.sh