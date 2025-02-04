setlocal

:: Check for administrative permissions
net session >nul 2>&1
if %errorLevel% neq 0 (
    echo Requesting administrative permissions...
    powershell -Command "Start-Process '%~f0' -Verb runAs"
    exit /b
)

:: Set the source and target directories
set "SOURCE_LIB=%~dp0lib"
set "SOURCE_ELA_SOURCE=%~dp0ela_source"
set "TARGET_DIR=C:\Program Files\ela"

:: Create the target directory if it doesn't exist
if not exist "%TARGET_DIR%" (
    mkdir "%TARGET_DIR%"
)

:: Copy files to the target directory
echo Copying files...
xcopy /y /e "%SOURCE_LIB%\*" "%TARGET_DIR%\"

echo Files copied successfully.

:: Uncomment to pause to see copied files.
:: pause

endlocal