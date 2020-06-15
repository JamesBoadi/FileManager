On windows:

To test the file using emacs, place all the files in the same directory
as erl.exe, or specify the directory on emacs first before compiling.

The submission contains 4 files:

dropbox.erl
tree.erl
mutex.erl
test.erl

Upon successfully compiling all the files, the function 
test:start(List, File) is called, this accepts two parameters in the following format

Example: test:start([1, 1, "John", 1, "Brock"], "FileName.txt").

1 - read file
String - write file 

1's in the list allow you to read the file
Strings in the list allows you to write the file

This solution can be run on multiple nodes, but the erlang cookie will be required on each
system.

http://erlang.org/doc/getting_started/conc_prog.html

Upon running, the last process will not be executed or deleted (in this case, "brock"), this is to demonstrate
cocurrency on multiple nodes and prevent the termination of the program






