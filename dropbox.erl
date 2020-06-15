
-module(dropbox).

-export([writeFile/2, getFileContents/2, readFile/1]).

writeFile(File, Content)-> % Write file function
    open_file(File, Content).

readFile(File)
-> {ok, Device} = (file:open(File, read)), % Open the file in read mode
  readFile_internal(Device).

open_file(File, Content) ->
  {ok, Device} = (file:open(File, [append])), % Open the file in write mode
  file:write_file(File, [Content++"\n"], [append]),
  writeFile_internal(Device, Content). % pass to update buffer
  
writeFile_internal(Device, Contents)->
  setFile(Device, Contents). 

getFileContents(Content, Line) -> % get value and send to above
     receive
        {file, Device, Contents} ->
         NewContent = Content ++ " " ++ Contents, % concatenate and set
         mutex:buffer(NewContent), % update, lock and send to buffer
         %io:format(File, "~s~n", NewContent),
      
        % io:format("The current file contents: ~p~n", [NewContent]), % testing only
         getFileContents(NewContent, Line + 1) 
        end.
      
setFile(Device, Contents) ->
    
    get_file ! {file, Device, Contents}.
    
readFile_internal(Device) -> 
case file:read_line(Device) of
    {ok, Line} ->
        ListOfWords = re:split(Line, ""), % split the line in words
        mutex:buffer(ListOfWords);
      %  addtoList([ListOfWords], Line), % Store words as list
    
    eof ->
        io:fwrite("Buffer has been populated ~n") %send a message if true and user closes file (print newline)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%% ADDITIONAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%
addtoList([]) -> ok;

addtoList([H|T])->   
mutex:buffer(H),
io:format("New pop: ~p~n", [H]),
addtoList(T).


createNewFilea(newFile) -> % Should be cocurrent with write file (if new file was just created)
    Data = ["1","2","3"],
    LineSep = io_lib:nl(),
    Print = [string:join(Data, LineSep), LineSep],
   % {sendfile, newFile} -> Pid ! sendfile,
    file:write_file(newFile, Print).

addtoList2([], Line)->
    Line.
    createNewFile(newFile) -> % Should be cocurrent with write file (if new file was just created)
    Data = ["1","2","3"],
    LineSep = io_lib:nl(),
    Print = [string:join(Data, LineSep), LineSep],
   % {sendfile, newFile} -> Pid ! sendfile,
    file:write_file(newFile, Print).

