@echo off
setlocal

REM Set the working directory to the script's location
cd /d "%~dp0"

REM Check for administrative privileges
net session >nul 2>&1
if %errorLevel% neq 0 (
    echo Requesting administrative permissions...
    powershell -Command "Start-Process '%~f0' -Verb runAs"
    exit /b
)

REM Define source and destination paths
set "source_path=.\x64\Debug\ela.exe"
set "destination_path=C:\Program Files\ela"

REM Verify source file exists
if not exist "%source_path%" (
    echo Source file does not exist: %source_path%
    pause
    exit /b
)

REM Create the destination directory if it doesn't exist
if not exist "%destination_path%" (
    mkdir "%destination_path%"
    if %errorLevel% neq 0 (
        echo Failed to create destination directory: %destination_path%
        pause
        exit /b
    )
)

REM Move the file
move "%source_path%" "%destination_path%"
if %errorLevel% neq 0 (
    echo Failed to move file to: %destination_path%
    pause
    exit /b
)

echo File moved successfully to: %destination_path%
pause

endlocal