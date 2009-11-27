@echo off
rem couchapp script for windows

setlocal

%~dp0..\python "%~dp0bin\couchapp" %*
endlocal
