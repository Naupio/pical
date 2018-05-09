-module (dag_accumulator).
-compile(export_all).

num_acc() ->
    ADDFu = fun(X, Y) -> X + Y end,
    {0, ADDFu}.

list_acc() ->
    ADDFu = fun(X, Y) -> X ++ Y end,
    {[], ADDFu}.

map_acc() ->
    ADDFu = fun(X, Y) -> maps:merge(X, Y) end,
    {#{}, ADDFu}.

set_acc() ->
    ADDFu = fun(X, Y) ->
                Z = X ++ Y,
                Temp = lists:seq(1,length(Z)),
                ZipTemp = lists:zip(Z, Temp),
                MapTemp = maps:from_list(ZipTemp),
                maps:keys(MapTemp)
            end,
    {[], ADDFu}.

test_term_acc() ->
    {_, NumFn} = num_acc(),
    {_, ListFn} = list_acc(),
    {_, MapFn} = map_acc(),
    {_, SetFn} = set_acc(),
    1 = NumFn(0,1),
    [1,2] = ListFn([1],[2]),
    #{a := 2, b := 2, c := 3} = MapFn(#{a => 1, b => 2}, #{a => 2, c => 3}),
    [1,2,3] = SetFn([1,2],[2,3]),
    io:format("test num_acc, list_acc, map_acc, set_acc ok \n").

accumulator(InitialValue, TermAcc) ->
    Value = InitialValue,
    {Zero, TermAdd} = TermAcc,
    Add = fun(Self, V) ->
              #{value := X} = Self,
              %io:format("~w\n",[TermAdd(X, V)]),
              Self#{value => TermAdd(X, V)}
          end,
    Merge = fun(Self, Acc) ->
                #{value := X } = Acc,
                Add(Self, X)
            end,
    Reset = fun(Self) ->
                Self#{value=> Zero}
            end,
    #{value=> Value, add => Add, merge => Merge, reset => Reset}.

test_accumulator() ->
    One = accumulator(1, num_acc()),
    #{value := A} = One,
    A = 1,
    ADD = maps:get(add, One),
    Two = ADD(One, 1),
    #{value := X} = Two,
    X = 2,
    Reset = maps:get(reset, Two),
    #{value := Y} = Reset(Two),
    Y = 0,
    Merge = maps:get(merge, One),
    #{value := Z} = Merge(One,Two),
    Z = 3,
    io:format("test_accumulator ok \n").
