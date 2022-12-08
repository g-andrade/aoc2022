#!/usr/bin/env escript
-mode(compile).

main([]) ->
    Filesystem = parse(_Pwd = queue:from_list(["/"]), _Tree = #{}),
    io:format("filesystem: ~p~n", [Filesystem]),

    Part01 = part01(Filesystem),
    io:format("answer to part 1: ~p~n", [Part01]),

    Part02 = part02(Filesystem),
    io:format("answer to part 2: ~p~n", [Part02]),
    ok.

%% Parsing filesystem info
%%
parse(Pwd, Tree) ->
    LineParts = get_line_parts(),
    handle_cmd(LineParts, Pwd, Tree).

handle_cmd(LineParts, Pwd, Tree) ->
    case LineParts of
        ["$", "cd", DirName] ->
            NewPwd = new_pwd(Pwd, DirName),
            parse(NewPwd, Tree);
        ["$", "ls"] ->
            handle_ls_output(Pwd, Tree);
        done ->
            Tree
    end.

new_pwd(_Pwd, "/" = _DirName) ->
    queue:from_list(["/"]);
new_pwd(Pwd, ".." = _DirName) ->
    queue:drop_r(Pwd);
new_pwd(Pwd, DirName) ->
    new_path(Pwd, DirName).

new_path(Pwd, DirName) ->
    queue:in(DirName, Pwd).

handle_ls_output(Pwd, Tree) ->
    case get_line_parts() of
        ["$", _] = Cmd ->
            handle_cmd(Cmd, Pwd, Tree);
        ["dir", _DirName] ->
            % if we never go into it, we'll never know its size anyway
            handle_ls_output(Pwd, Tree);
        [FileSizeStr, FileName] ->
            FileSize = list_to_integer(FileSizeStr),
            UpdatedTree = place_entry_in_tree(Pwd, FileName, {file, FileSize}, Tree),
            handle_ls_output(Pwd, UpdatedTree);
        Other ->
            handle_cmd(Other, Pwd, Tree)
    end.

place_entry_in_tree(Pwd, EntryName, EntryValue, Tree) ->
    case queue:out(Pwd) of
        {{value, Name}, RemainingPwd} ->
            place_entry_in_subtree(Name, RemainingPwd, EntryName, EntryValue, Tree);
        {empty, _} ->
            maps:put(EntryName, EntryValue, Tree)
    end.

place_entry_in_subtree(Name, RemainingPwd, EntryName, EntryValue, Tree) ->
    case maps:find(Name, Tree) of
        {ok, {dir, SubTree}} ->
            UpdatedSubTree = place_entry_in_tree(RemainingPwd, EntryName, EntryValue, SubTree),
            maps:update(Name, {dir, UpdatedSubTree}, Tree);
        error ->
            NewSubTree = place_entry_in_tree(RemainingPwd, EntryName, EntryValue, #{}),
            maps:put(Name, {dir, NewSubTree}, Tree)
    end.

get_line_parts() ->
    case io:get_line(_Prompt = "") of
        eof ->
            done;
        Line ->
            re:split(Line, "[ \n\r]", [trim, {return, list}])
    end.

%% Filtering dirs
%%
part01(Tree) ->
    {_, SumSizeAccN} = part01(100000, Tree, _SumSizeAcc0 = 0),
    SumSizeAccN.

part01(MaxSize, Tree, SumSizeAcc0) ->
    {CurDirSize, SumSizeAccN}
        = maps:fold(
            fun (_EntryName, {dir, SubTree}, {CurDirSizeAcc, SumSizeAcc}) ->
                    {SubDirSizeAcc, UpdatedSumSizeAcc} = part01(MaxSize, SubTree, SumSizeAcc),
                    {CurDirSizeAcc + SubDirSizeAcc,
                     UpdatedSumSizeAcc};
                (_EntryName, {file, FileSize}, {CurDirSizeAcc, SumSizeAcc}) ->
                    {CurDirSizeAcc + FileSize, SumSizeAcc}
            end,
            {_CurDirSizeAcc0 = 0, SumSizeAcc0},
            Tree),

    UpdatedSumSizeAccN
        = case CurDirSize =< MaxSize of
              true -> SumSizeAccN + CurDirSize;
              false -> SumSizeAccN
          end,

    {CurDirSize, UpdatedSumSizeAccN}.

part02(Tree) ->
    {RootDirSize, AllSizes} = compute_all_sizes(Tree, _Acc = gb_sets:empty()),
    DiskSpaceAvailable = 70000000,
    MinSpaceRequired = 30000000,
    FreeSpaceAvailable = DiskSpaceAvailable - RootDirSize,
    MinSizeOfDirectoryToDelete = MinSpaceRequired - FreeSpaceAvailable,
    Iterator = gb_sets:iterator_from(MinSizeOfDirectoryToDelete, AllSizes),
    {SizeOfDirectoryToDelete, _} = gb_sets:next(Iterator),
    SizeOfDirectoryToDelete.

compute_all_sizes(Tree, Acc0) ->
    {CurDirSize, AccN}
        = maps:fold(
            fun (_EntryName, {dir, SubTree}, {CurDirSizeAcc, Acc1}) ->
                    {SubDirSize, Acc2} = compute_all_sizes(SubTree, Acc1),
                    {CurDirSizeAcc + SubDirSize, Acc2};
                (_EntryName, {file, FileSize}, {CurDirSizeAcc, Acc}) ->
                    {CurDirSizeAcc + FileSize, Acc}
            end,
            {_CurDirSizeAcc0 = 0, Acc0},
            Tree),

    UpdatedAccN = gb_sets:add_element(CurDirSize, AccN),
    {CurDirSize, UpdatedAccN}.
