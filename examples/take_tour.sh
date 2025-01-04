echo starting 'target practice'...
cd target_practice > /dev/null
ela run

echo done. starting 'sequencer'...
cd ../sequencer > /dev/null
ela run

echo done. starting 'dynamic library', a hot reload-able test using raylib.
cd ../dynamic_library  > /dev/null
ela run