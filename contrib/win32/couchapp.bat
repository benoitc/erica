@echo off
rem couchapp script for windows

setlocal

%~dp0..\python "%~dp0couchapp" %*
endlocal
