#!/usr/bin/env escript
-mode(compile).

-record(label, {
    cell :: start | 'end' | 0..25,
    pos :: {pos_integer(), pos_integer()}
}).

main(["part1"]) ->
    do().

do() ->
    Matrix = parse_input(),
    log("Matrix: ~p", [Matrix]),

    Graph = digraph:new(),
    try build_graph(Graph, Matrix) of
        VertexPerPos ->
            log("Graph info: ~p", [digraph:info(Graph)]),

            #{start := StartPos, 'end' := EndPos} = get_start_and_end_positions(Matrix),
            StartVertex = maps:get(StartPos, VertexPerPos),
            EndVertex = maps:get(EndPos, VertexPerPos),
            [_|_] = ShortestPath = digraph:get_short_path(Graph, StartVertex, EndVertex),

            ShortestPathPositions
                = lists:map(
                    fun (Vertex) ->
                            {Vertex, #label{pos = Pos}} = digraph:vertex(Graph, Vertex),
                            Pos
                    end,
                    ShortestPath),

            log("Shorted path: ~p", [ShortestPathPositions]),
            log("Shorted path nr of steps: ~b", [length(ShortestPath) - 1])
    after
        digraph:delete(Graph)
    end.

parse_input() ->
    Rows = parse_input_recur(),
    list_to_tuple(Rows).

parse_input_recur() ->
    case io:get_line("") of
        Line when is_list(Line) ->
            Row = parse_line(Line),
            [Row | parse_input_recur()];
        eof ->
            []
    end.

parse_line(Line) ->
    [Chars | _] = string:split(Line, "\n", all),
    list_to_tuple(
      lists:map(
        fun ($S) -> start;
            ($E) -> 'end';
            (C) -> C - $a
        end,
        Chars)).

get_start_and_end_positions(Matrix) ->
    Ys = lists:seq(1, tuple_size(Matrix)),
    get_start_and_end_positions_(Matrix, Ys, #{}).

get_start_and_end_positions_(Matrix, [Y | NextYs], Acc) ->
    Row = element(Y, Matrix),
    Xs = lists:seq(1, tuple_size(Row)),
    UpdatedAcc = get_start_and_end_positions__(Row, Y, Xs, Acc),
    get_start_and_end_positions_(Matrix, NextYs, UpdatedAcc);
get_start_and_end_positions_(_Matrix, [], Acc) ->
    Acc.

get_start_and_end_positions__(Row, Y, [X | NextXs], Acc) ->
    case element(X, Row) of
        StartOrEnd when StartOrEnd =:= start; StartOrEnd =:= 'end' ->
            UpdatedAcc = Acc#{StartOrEnd => {X, Y}},
            get_start_and_end_positions__(Row, Y, NextXs, UpdatedAcc);
        _ ->
            get_start_and_end_positions__(Row, Y, NextXs, Acc)
    end;
get_start_and_end_positions__(_Row, _Y, [], Acc) ->
    Acc.

build_graph(Graph, Matrix) ->
    VertexPerPos = build_graph_vertices(Graph, Matrix),
    build_graph_edges(Graph, VertexPerPos),
    VertexPerPos.

build_graph_vertices(Graph, Matrix) ->
    Ys = lists:seq(1, tuple_size(Matrix)),
    build_graph_vertices_rows(Graph, Matrix, Ys, _Acc = []).

build_graph_vertices_rows(Graph, Matrix, [Y | NextYs], Acc) ->
    Row = element(Y, Matrix),
    Xs = lists:seq(1, tuple_size(Row)),
    UpdatedAcc = build_graph_vertices_row(Graph, Row, Y, Xs, Acc),
    build_graph_vertices_rows(Graph, Matrix, NextYs, UpdatedAcc);
build_graph_vertices_rows(_Graph, _Matrix, [], Acc) ->
    maps:from_list(Acc).

build_graph_vertices_row(Graph, Row, Y, [X | NextXs], Acc) ->
    Cell = element(X, Row),
    Vertex = digraph:add_vertex(Graph),
    Pos = {X, Y},
    Label = #label{cell = Cell, pos = Pos},
    Vertex = digraph:add_vertex(Graph, Vertex, Label),
    UpdatedAcc = [{Pos, Vertex} | Acc],
    build_graph_vertices_row(Graph, Row, Y, NextXs, UpdatedAcc);
build_graph_vertices_row(_Graph, _Matrix, _Y, [], Acc) ->
    Acc.

build_graph_edges(Graph, VertexPerPos) ->
    lists:foreach(
      fun ({Pos, Vertex}) ->
              build_graph_edges_for_vertex(Graph, VertexPerPos, Pos, Vertex)
      end,
      lists:sort(maps:to_list(VertexPerPos))). % sorting helps with debugging

build_graph_edges_for_vertex(Graph, VertexPerPos, Pos, Vertex) ->
    lists:foreach(
      fun (NeighVertex) ->
              digraph:add_edge(Graph, Vertex, NeighVertex)
      end,
      reachable_neighbours(Graph, VertexPerPos, Pos, Vertex)).

reachable_neighbours(Graph, VertexPerPos, Pos, Vertex) ->
    % log("--------", []),
    {Vertex, #label{cell = Cell}} = digraph:vertex(Graph, Vertex),
    lists:filtermap(
      fun (NeighPos) ->
              case maybe_reachable_neighbour(Graph, VertexPerPos, Cell, NeighPos) of
                  {true, _} = True ->
                      % log("~p -> ~p", [Pos, NeighPos]),
                      True;
                  false ->
                      false
              end
      end,
      neighbouring_positions(Pos)).

maybe_reachable_neighbour(Graph, VertexPerPos, Cell, NeighPos) ->
    try maps:get(NeighPos, VertexPerPos) of
        NeighVertex ->
            {NeighVertex, #label{cell = NeighCell}} = digraph:vertex(Graph, NeighVertex),
            (height_diff(NeighCell, Cell) =< 1) andalso {true, NeighVertex}
    catch
        error:{badkey, K} when K =:= NeighPos ->
            false
    end.

height_diff('end' = _NeighCell, Cell) ->
    height_diff(25, Cell);
height_diff(_NeighCell, 'end' = _Cell) ->
    infinity;
height_diff(start = _NeighCell, _Cell) ->
    infinity;
height_diff(NeighCell, start = _Cell) ->
    height_diff(NeighCell, 0);
height_diff(NeighCell, Cell) ->
    NeighCell - Cell.

neighbouring_positions({X, Y}) ->
    [{X + 1, Y},
     {X - 1, Y},
     {X, Y + 1},
     {X, Y - 1}].

log(Format, Args) ->
    io:format(standard_error, Format ++ "\n", Args).
