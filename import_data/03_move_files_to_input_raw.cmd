
echo %DATE%

SET destination_folder="..\input\data_raw"

xcopy .\output\*.* %destination_folder% /s /i

PAUSE


