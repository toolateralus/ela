@echo off

REM some windows user fix this idk how that shtuff works.

REM Create the destination directory if it doesn't exist
if not exist "C:\usr\local\lib\ela" (
  mkdir "C:\usr\local\lib\ela"
)

REM Move the contents of .\lib to C:\usr\local\lib\ela
move .\lib\* "C:\usr\local\lib\ela\"