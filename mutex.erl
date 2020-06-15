-module(mutex).

%% API

-export([ process_signal/1, getTime/0, reader_Buffer/1, buffer/1
, read_writeLock/1, executeFunction/2, handleProcess/0, process_queue/1, 
timeElapsed/0]). 

% Tasks -> 

% Read: 1
% Write 2 


-spec get_timestamp() -> integer().

getTime() ->
     receive
        {time, Time} ->
         io:format("Time in seconds: ~p~n", [Time]),
         getTime()
        end.

setTime(Time) ->
    get_time ! {time, Time}.
    
get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
 % Stamp = (Mega*1000000 + Sec)*1000 + round(Micro/1000),
   Sec.

timeElapsed() -> 
    ElapsedTime = (get_timestamp() - 0) / 1000,
    receive
        set_time ->
        setTime(ElapsedTime)
    end.
  
% Spawned functions must be exported

executeFunction([H|T], File) -> % execute the function
io:format("File: ~p~n", [File]),
Tree = get_tree(),
Queue = get_queue(),
Resource = "",
Content = "", 
ListOfWords = "",
timer(), % Start the timer

register(get_file, spawn(dropbox, getFileContents, [Content, 1])), % Return the get_file
register(queue, spawn(mutex, process_queue, [Queue])), % Start queue
register(lock, spawn(mutex, read_writeLock, [Tree])), % Handle to lock 
register(process_signal, spawn(mutex, process_signal, [File])), % Process signal
register(reader_Buffer, spawn(mutex, reader_Buffer, [ListOfWords])), % Initialize the buffer
%dropbox:readFile(File), % Populate the buffer (incomplete)
handle_signal([H|T], 1).

timer() ->
    Timer = spawn(mutex, timeElapsed, []),
    GetTime = spawn(mutex, getTime, []),
    register(get_time, GetTime),
    register(time_elapsed, Timer).

% Read File
%----------------------------------------------------------------- 
processloop(0) ->
    io:fwrite("No more processes to handle at this time ~n");
   
processloop(Counter) -> % Send a signal to each process
    handleProcess(),
    processloop(Counter - 1).

handle_signal([], Counter) ->
     processloop(Counter);

handle_signal([H|T], Counter)  -> % Will input each process 
    case H of
            1 -> 
            Value = Counter, 
            io:fwrite("Read process added ~n"),
          %  io:format("Read process added ~p~n",[H]),
            lock ! {request_readlock, Value, self()}, % request a lock
            handle_signal(T, Counter + 1);
            
            Otherwise -> % Any other value is assumed to be a write operation
            Value = Counter,
            io:fwrite("Write process added ~n"),
            lock ! {request_writelock, Value, H, self()},
            handle_signal(T, Counter + 1)
    end.
     

process_signal(File) -> % Will send a signal to either read or update the buffer
    receive
       read -> reader_Buffer ! read, process_signal(File);
        
    {write, Contents} ->
        spawn(dropbox, writeFile, [File, Contents]),
        process_signal(File)
    end.

buffer(Contents) -> % reader buffer
    reader_Buffer ! {updateResource, Contents}.

reader_Buffer(Resource) -> % Store the previous state of resource (initially empty)
    receive
        {updateResource, Contents} -> % Update resource
            io:fwrite("Update resource ~n"),
            Buffer = Resource ++ Contents,
            io:format("The current file contents after updating the buffer: ~p~n", [Buffer]),
            reader_Buffer(Buffer);

        read -> % Read resource
             io:format("The current file contents in the buffer after reading: ~p~n", [Resource]),
             reader_Buffer(Resource)
        end.

handleProcess() -> % Send a signal to handle to the process
    lock ! read_save_File.



get_tree() -> Tree = tree:empty(), Tree. % Return an empty tree

get_queue() -> Queue = queue:new(), Queue. % Return an empty queue

process_queue(Queue) -> 
    receive
        {enqueue, Value, Content, Pid2, Mode} ->
        case Mode of 
            "empty" ->
                NewQueue = queue:in(Value, Queue), % Enqueue and update queue (FIFO)
                process_queue(NewQueue); 
            "non_empty_read" ->
                NewQueue  = queue:in(Value, Queue), % Enqueue and update queue (FIFO)
                Pid2 ! {searchTree, Value, Content, NewQueue, "read"}, % Send a request to search and add to the tree
                process_queue(NewQueue);
            "non_empty_write" ->
                NewQueue  = queue:in(Value, Queue), % Enqueue and update queue (FIFO)
                Pid2 ! {searchTree, Value, Content, NewQueue, "write"}, % Send a request to search and add to the tree
                process_queue(NewQueue)
            end;

         {dequeue, write, Contents} ->
             case queue:out(Queue) of 
                {empty, _} -> io:fwrite("queue is empty ~n");
           
                {{Head, Tail}, NewQueue} ->
                    io:fwrite("Pop ~n"),
                    process_signal ! {write, Contents}, % read or write
                    process_queue(NewQueue)
                end;
         {dequeue, read} ->
             case queue:out(Queue) of 
                {empty, _} -> io:fwrite("queue is empty ~n");
           
                {{Head_, Tail_}, NewQueue_} ->
                    process_signal ! read, % read or write
                    process_queue(NewQueue_)
                end
    end.

setQueue(Value, Content, Pid2, Mode) -> % set queue and pass values to queue -> lock
    queue ! {enqueue, Value, Content, Pid2, Mode}.
    
read_writeLock(Tree) -> % Tree acts as a buffer, Every other process (That has a read-lock) can only view the previous state of the file
TreeEmpty = tree:is_empty(Tree),
receive 
      {request_readlock, Value, Pid} -> 
            if
            TreeEmpty == true -> % If tree is empty add a process as normal and unlock it
                NewTree = tree:insert(Value, [Value, "reading"], Tree), 
                setQueue(Value, "", self(), "empty"),
                NewVal = tree:values(NewTree),
                io:format("First process added to tree: ~p~n", [NewVal]),
                read_writeLock(NewTree);% use newtree
               % handle_signal ! ok_to_write; % ------------- also add process (as normal) and notify handle_signal

            true -> % Else enqueue the process,
                setQueue(Value, "" ,self(), "non_empty_read"), % Do not set the queue before adding it to the tree
                read_writeLock(Tree)
            end; 

       {request_writelock, Value, Content, Pid} -> % Request a lock to write to the file
            if
            TreeEmpty == true -> % If tree is empty add a process as normal and unlock it
                NewTree = tree:insert(Value, [Content, "writing"], Tree), 
                setQueue(Value, Content, self(), "empty"),
                NewVal = tree:values(NewTree),
                io:format("First process added to tree: ~p~n", [NewVal]),
                read_writeLock(NewTree);% use newtree
     

            true -> % Else enqueue the process,
                setQueue(Value, Content, self(), "non_empty_write"), % We not set the queue before adding a key to the tree
                read_writeLock(Tree)
            end;
  
        {searchTree, Value, Content, NewQueue, Mode} -> % Any processes to be added to the tree are handled here
            Length = queue:len(NewQueue),
            if

            Mode == "write" ->
                if
                    Length >= 1 -> % The length of the queue must be greater than one
                        NewTree2 = tree:insert(Value, [Content, "write_lock"], Tree),  % lock the write proccess and add it to tree
                        NewVal = tree:values(NewTree2),      
                        io:format("This file is now locked, current tree ~p~n", [NewVal]),
                        read_writeLock(NewTree2);    
                    
                   true -> % The queue is now empty
                     io:fwrite("No more processes to add ~n")
                end;

            Mode == "read" ->
                   if
                    Length >= 1 -> 
                        NewTree3 = tree:insert(Value, [Value, "read_lock"], Tree),  % lock the write proccess and add it to tree
                        NewVal = tree:values(NewTree3),      
                        io:format("This file is now locked, current tree ~p~n", [NewVal]), 
                        process_signal ! read, % A request must be sent so that the current resource can be read 
                        read_writeLock(NewTree3);    
                    
                   true ->
                     io:fwrite("No more processes to add ~n")
                end
            end;

        read_save_File -> % The file is saved and the process is deleted, tree is updated  % Check if tree is empty to avoid errors  
            TreeSize = tree:size(Tree),
            if
                TreeSize > 1 ->
                   {Key_, [Value|[H|T]]} = tree:smallest(Tree), % Take the root of the tree and delete it
                   io:format("King ~p~n", [H]),
                    case H of
                       "writing" -> % update so that it can write
                            NewTree4 = tree:delete(Key_, Tree),  % Delete the current process
                            {Key_1, [NewValue|[H_|T_]]} = tree:smallest(NewTree4),
                            if 
                                H_ == "write_lock" ->
                                [A|B] = [NewValue],
                                NewRoot1 = tree:update(Key_1, [A, "writing"], NewTree4), % Update the next process (upon checking it)
                                Values_ = tree:values(NewRoot1),
                                io:format("----> ~p~n", [Values_]),
                                queue ! {dequeue, write, A},
                                read_writeLock(NewRoot1);

                                true ->
                                NewRoot1 = tree:update(Key_1, [Key_1, "reading"], NewTree4),
                                Values_ = tree:values(NewRoot1),
                                queue ! {dequeue, read},
                                io:format("----> ~p~n", [Values_]),
                                read_writeLock(NewRoot1)
                            end;  % else perform a read operation 
                                  
                    
                          "reading" -> % update so that it can write

                            NewTree5 = tree:delete(Key_, Tree),  % Delete the current process
                            {Key2, [Value2|[H2|T2]]} = tree:smallest(NewTree5),
                            if H2 == "read_lock" ->
                                NewRoot2 = tree:update(Key2, [Key2, "reading"], NewTree5), 
                                Values = tree:values(NewRoot2),
                                queue ! {dequeue, read},
                                io:format("----> ~p~n", [Values]),
                                read_writeLock(NewRoot2);

                                true ->
                                [Head | Tail] = [Value2],
                                NewRoot2 = tree:update(Key2, [Head, "writing"], NewTree5), % Update the next process (upon checking it)
                                Values = tree:values(NewRoot2),
                                io:format("----> ~p~n", [Values]),
                                queue ! {dequeue, write, Head},
                                read_writeLock(NewRoot2)
                            end;

                       Otherwise ->
                            io:fwrite("No more processes, no change of state ~n"), % Nothing to save, no change of state
                            read_writeLock(Tree)
                    end;

                TreeSize =< 1 ->  % If tree is less than or equal to 1, nothing to process
                         {Key3, [Value3|[H3|T3]]} = tree:smallest(Tree),
                            
                            if H3 == "reading" ->
                                NewRoot2 = tree:update(Key3, [Key3, "reading"], Tree), % Update the next process (upon checking it)
                                Values = tree:values(NewRoot2),
                                queue ! {dequeue, read},
                                io:format("Final process ~p~n", [Values]);
                               
                                true ->
                                [Head_ | Tail_] = [Value3],
                                NewRoot2 = tree:update(Key3, [Head_, "writing"], Tree), % Update the next process (upon checking it)
                                Values = tree:values(NewRoot2),
                                io:format("Final process ~p~n", [Values]),
                                queue ! {dequeue, write, Head_}
                            end,
                    %NewTree6 = tree:delete(Key3, Tree),  % Delete the current process
                    io:fwrite("No more processes, no change of state ~n")
                    
                end
        end.
     
