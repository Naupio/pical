-module (ta_rdd).
%-export().
-compile(export_all). % Just for test, remember to delete it when the project release.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_all() ->
    test_lazy_map(),
    test_lazy_filter(),
    test_lazy_lists(),
    test_lazy_flat(),
    test_lazy_flat_map(),
    test_lazy_map_Partition(), 
    test_lazy_map_value(),
    test_lazy_kv_lists(),
    test_lazy_flat_map_value(),
    test_merge_pair(),
    test_seq_take(),
    test_seq_del(),
    test_seq_slice(),
    io:format("TEST_ALL is ok\n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_lazy({none,_}) ->
    [];
eval_lazy({fn, Fn}) ->
    Fn();
eval_lazy({nil, Fn}) ->
    eval_lazy(Fn());
eval_lazy({{flat, LazyLists},Fn}) ->
    eval_lazy(LazyLists) ++ eval_lazy(Fn());
eval_lazy({A,Fn}) ->
    [A|eval_lazy(Fn())]. 

base_rdd() ->
    Partitions = #{} , % Partitions
    Compute = fun(Split) -> [] end,
    Dependencies = {none,[]} ,
    Partitioner = none,
    Lineage = [],

    #{partitions => Partitions, compute => Compute,
      dependencies => Dependencies, partitioner => Partitioner,
      lineage => Lineage}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lazy_map([], SomeFn) ->
    {none, fun() -> {none,fun() -> none end} end};
lazy_map([Ele], SomeFn)->
    {SomeFn(Ele),fun() -> lazy_map([], SomeFn) end};
lazy_map(Lists, SomeFn) ->
    [Head| Tail] = Lists,
    {SomeFn(Head), fun() -> lazy_map(Tail, SomeFn) end }.


test_lazy_map() ->
    Lists = [1,2,3],
    SomeFn = fun(Ele) -> Ele + 1 end,
    [2,3,4] = eval_lazy(lazy_map(Lists, SomeFn)),
    io:format("test lazy_map ok\n").

map_rdd(ParentRDD, SomeFn) ->
    #{partitions := ParentPartitions, compute := ParentCompute,
      dependencies := ParentDependencies, partitioner := ParentPartitioner,
      lineage := ParentLineage} = ParentRDD,

    Partitions = ParentPartitions,
    % Split is an iterator of Partition in Partitions
    Compute = fun(Split) -> lazy_map(Split, SomeFn) end,
    Dependencies = {one_to_one_dependency, [dependency:one_to_one_dependency(ParentRDD)]},
    Partitioner = ParentPartitioner,
    Lineage = [one_to_one_dependency | ParentLineage],

    #{partitions => Partitions, compute => Compute,
      dependencies => Dependencies, partitioner => Partitioner,
      linage => Lineage}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lazy_filter([],FilterFn) ->
    {none, fun() -> {none,fun() -> none end} end};
lazy_filter([Ele],FilterFn) ->
    case FilterFn(Ele) of
        true -> {Ele, fun() -> lazy_filter([], FilterFn) end};
        false -> {nil, fun() -> lazy_filter([], FilterFn) end}
    end;
lazy_filter(Lists, FilterFn) ->
    [Head| Tail] = Lists,
    case FilterFn(Head) of
        true -> {Head, fun() -> lazy_filter(Tail, FilterFn) end};
        false -> {nil, fun() -> lazy_filter(Tail, FilterFn) end}
    end.

test_lazy_filter() ->
    Lists = [1,2,3,4,5],
    Even = fun(A) -> A rem 2 == 0 end,

    [2,4] = eval_lazy(lazy_filter(Lists, Even)),
    % eval_lazy(lazy_filter(Lists, Even)),
    io:format("test lazy_filter ok\n").

filter_rdd(ParentRDD, SomeFn) ->
    #{partitions := ParentPartitions, compute := ParentCompute,
      dependencies := ParentDependencies, partitioner := ParentPartitioner,
      lineage := ParentLineage} = ParentRDD,

    Partitions = ParentPartitions,
    Compute = fun(Split) -> lazy_filter(Split, SomeFn) end,
    Dependencies = {one_to_one_dependency, [dependency:one_to_one_dependency(ParentRDD)]},
    Partitioner = ParentPartitioner,
    Lineage = [one_to_one_dependency | ParentLineage],

    #{partitions => Partitions, compute => Compute,
      dependencies => Dependencies, partitioner => Partitioner,
      linage => Lineage}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lazy_lists(none) ->
    {none, fun() -> none end};
lazy_lists([Ele]) ->
    {Ele, fun() -> lazy_lists(none) end};
lazy_lists(Lists) ->
    [Head| Tail] = Lists,
    {Head,fun() -> lazy_lists(Tail) end}.

test_lazy_lists() ->
    Lists = [[1,2],[3,4],[5,6]], 
    Result_Lists = lazy_lists(Lists),
    [[1,2],[3,4],[5,6]] = eval_lazy(Result_Lists),
    io:format("test lazy_lists ok\n").

lazy_flat([]) ->
    {none, fun() -> none end};
lazy_flat([Ele]) ->
    {{flat, lazy_lists(Ele)}, fun() -> lazy_flat([]) end};
lazy_flat(Lists) ->
    [Head| Tail] = Lists,
    {{flat, lazy_lists(Head)}, fun() -> lazy_flat(Tail) end}.

test_lazy_flat() ->
    Lists = [[1,2],[3,4],[5,6]],
    Result_Lists = lazy_flat(Lists),
    [1, 2, 3, 4, 5, 6] = eval_lazy(Result_Lists),
    io:format("test lazy_flat_map ok\n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lazy_flat_map([], ReturnListFn) ->
    {none, fun() -> none end};
lazy_flat_map([Ele], ReturnListFn) ->
    {{flat, lazy_lists(ReturnListFn(Ele))}, fun() -> lazy_flat_map([], ReturnListFn) end};
lazy_flat_map(Lists, ReturnListFn) ->
    [Head| Tail] = Lists,
    {{flat, lazy_lists(ReturnListFn(Head))}, fun() -> lazy_flat_map(Tail, ReturnListFn) end}.

test_lazy_flat_map() ->
    Lists = [1, 3, 5],
    ReturnListFn = fun(A) -> [A, A+1] end,
    Result_Lists = lazy_flat_map(Lists, ReturnListFn),
    [1, 2, 3, 4, 5, 6] = eval_lazy(Result_Lists),
    io:format("test lazy_flat ok\n").

flat_map_rdd(ParentRDD, ReturnListFn) ->
    #{partitions := ParentPartitions, compute := ParentCompute,
      dependencies := ParentDependencies, partitioner := ParentPartitioner,
      lineage := ParentLineage} = ParentRDD,

    Partitions = ParentPartitions,
    Compute = fun(Split) -> lazy_flat_map(Split, ReturnListFn) end,
    Dependencies = {one_to_one_dependency, [dependency:one_to_one_dependency(ParentRDD)]},
    Partitioner = ParentPartitioner,
    Lineage = [one_to_one_dependency | ParentLineage],

    #{partitions => Partitions, compute => Compute,
      dependencies => Dependencies, partitioner => Partitioner,
      linage => Lineage}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lazy_map_Partition(Splits, PartiotionFn) ->
    {fn, fun() -> PartiotionFn(Splits) end}.

test_lazy_map_Partition() ->
    Splits = [1,2,3],
    PartiotionFn = fun(Splits) -> [lists:sum(Splits)] end,
    Result = lazy_map_Partition(Splits, PartiotionFn),
    [6] = eval_lazy(Result),
    io:format("test lazy_map_Partition ok \n").

map_Partitions_rdd(ParentRDD, Fn) ->
    #{partitions := ParentPartitions, compute := ParentCompute,
      dependencies := ParentDependencies, partitioner := ParentPartitioner,
      lineage := ParentLineage} = ParentRDD,

    Partitions = ParentPartitions,
    Compute = fun(Split) -> lazy_map_Partition(Split, Fn) end,
    Dependencies = {one_to_one_dependency, [dependency:one_to_one_dependency(ParentRDD)]},
    Partitioner = ParentPartitioner,
    Lineage = [one_to_one_dependency | ParentLineage],

    #{partitions => Partitions, compute => Compute,
      dependencies => Dependencies, partitioner => Partitioner,
      linage => Lineage}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lazy_map_value([], Fn) ->
    {none, fun() -> none end };
lazy_map_value([{K, V}],  Fn) ->
    {{K, Fn(V)}, fun() -> lazy_map_value([], Fn) end};
lazy_map_value(KVLists, Fn) ->
    [Head| Tail] = KVLists,
    {K, V} = Head,
    {{K, Fn(V)}, fun() -> lazy_map_value(Tail, Fn) end}.

test_lazy_map_value() -> 
    KVLists  = [{a,1}, {b,2}],
    Fn = fun(A) -> A + 1 end ,
    [{a,2}, {b,3}] = eval_lazy(lazy_map_value(KVLists, Fn)),
    io:format("test_lazy_map_value ok \n").

mapped_values_rdd(ParentRDD, Fn) ->
    #{partitions := ParentPartitions, compute := ParentCompute,
      dependencies := ParentDependencies, partitioner := ParentPartitioner,
      lineage := ParentLineage} = ParentRDD,

    Partitions = ParentPartitions,
    Compute = fun(KVLists) -> lazy_map_value(KVLists, Fn) end,
    Dependencies = {one_to_one_dependency, [dependency:one_to_one_dependency(ParentRDD)]},
    Partitioner = ParentPartitioner,
    Lineage = [one_to_one_dependency | ParentLineage],

    #{partitions => Partitions, compute => Compute,
      dependencies => Dependencies, partitioner => Partitioner,
      linage => Lineage}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lazy_kv_lists(K, []) ->
    {none, fun() -> none end};
lazy_kv_lists(K, [Ele]) ->
    {{K, Ele}, fun() -> lazy_kv_lists(K, []) end};
lazy_kv_lists(K, Lists) ->
    [Head| Tail] = Lists,
    {{K, Head}, fun() -> lazy_kv_lists(K, Tail) end}.

test_lazy_kv_lists()->
    Lists = [1,2,3],
    [{a,1}, {a,2}, {a,3}] = eval_lazy(lazy_kv_lists(a, Lists)),
    io:format("test_lazy_kv_lists ok\n").

lazy_flat_map_value([],ReturnListFn)->
    {none,fun() -> none end };
lazy_flat_map_value([Ele],ReturnListFn)->
    {K, V} = Ele,
    {{flat, lazy_kv_lists(K, ReturnListFn(V))}, fun() -> lazy_flat_map_value([], ReturnListFn) end};
lazy_flat_map_value(KVLists, ReturnListFn) ->
    [Head| Tail] = KVLists,
    {K, V} = Head,
    {{flat, lazy_kv_lists(K ,ReturnListFn(V))}, fun() -> lazy_flat_map_value(Tail, ReturnListFn) end}.

test_lazy_flat_map_value() ->
    KVLists = [{a,1}, {b,2}],
    ReturnListFn = fun(A) -> [A, A+1] end,
    [{a,1}, {a,2}, {b,2}, {b,3}] = eval_lazy(lazy_flat_map_value(KVLists, ReturnListFn)),
    io:format("test_lazy_flat_map_value ok\n").

flat_mapped_values_rdd(ParentRDD, ReturnListFn) -> 
    #{partitions := ParentPartitions, compute := ParentCompute,
      dependencies := ParentDependencies, partitioner := ParentPartitioner,
      lineage := ParentLineage} = ParentRDD,


    Partitions = ParentPartitions,
    Compute = fun(KVLists) -> lazy_flat_map_value(KVLists, ReturnListFn) end,
    Dependencies = {one_to_one_dependency, [dependency:one_to_one_dependency(ParentRDD)]},
    Partitioner = ParentPartitioner,
    Lineage = [one_to_one_dependency | ParentLineage],

    #{partitions => Partitions, compute => Compute,
      dependencies => Dependencies, partitioner => Partitioner,
      linage => Lineage}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

merge_pair(Combiner, K, V) ->
    case maps:is_key(K, Combiner) of
        true -> Combiner#{K := V};
        false -> Combiner#{K => V}
    end.

test_merge_pair() ->
    Combiner = #{a => 1, b => 2},
    #{a := 2, b := 2} = merge_pair(Combiner, a, 2),
    #{a := 1, b := 2, c := 3} = merge_pair(Combiner, c, 3),
    io:format("test_merge_pair ok\n").

shuffled_rdd(ParentRDD)->
    #{partitions := ParentPartitions, compute := ParentCompute,
      dependencies := ParentDependencies, partitioner := ParentPartitioner,
      lineage := ParentLineage} = ParentRDD,


    Partitions = ParentPartitions,
    Compute = fun(KVSplits) -> 
                Combiners = fetch:shuffle_fetch(),
                maps:to_list(Combiners)
              end,
    Dependencies = {shuffle_dependency, [dependency:shuffle_dependency(ParentRDD)]},
    Partitioner = ParentPartitioner,
    Lineage = [shuffle_dependency | ParentLineage],

    #{partitions => Partitions, compute => Compute,
      dependencies => Dependencies, partitioner => Partitioner,
      linage => Lineage}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seq_take([],_) ->
    [];
seq_take(_, 0) ->
    [];
seq_take(Seq, TakeNum) ->
    [Head | Tail] = Seq,
    [Head|seq_take(Tail, TakeNum -1)].

test_seq_take() ->
    Seq = lists:seq(1,10),
    [1,2,3,4] = seq_take(Seq, 4),
    io:format("test_seq_take ok\n").

seq_del([],_) ->
    [];
seq_del(Seq, 0) ->
    Seq;
seq_del(Seq, DelNum) ->
    [Head | Tail] = Seq,
    seq_del(Tail, DelNum -1).

test_seq_del() ->
    Seq = lists:seq(1,6),
    [5,6] = seq_del(Seq, 4),
    io:format("test_seq_del ok\n").

seq_take_to_end([],_) ->
    [];
seq_take_to_end(Seq, TakeNum) ->
    TakeHead = seq_take(Seq, TakeNum),
    TakeTail = seq_del(Seq,TakeNum),
    [TakeHead|seq_take_to_end(TakeTail, TakeNum)].

test_seq_take_to_end() ->
    Seq = lists:seq(1,10),
    [[1,2,3,4],[5,6,7,8],[9,10]] = seq_take_to_end(Seq, 4),
    io:format("test_seq_take_to_end ok\n").

seq_slice(Seq, NumSlices) ->
    M = length(Seq),
    NAdd = case M rem NumSlices =:= 0 of
        true  -> 0;
        false -> 1
    end,
    TakeNum = (M div NumSlices) + NAdd,
    seq_take_to_end(Seq, TakeNum).

test_seq_slice() ->
    Seq = lists:seq(1,5),
    NumSlices = 3,
    [[1,2],[3,4],[5]] = seq_slice(Seq, NumSlices),
    io:format("test_seq_slice ok\n").

parallel_collection(Seq, NumSlices) ->
    Partitions = fun() -> {partitions, seq_slice(Seq, NumSlices)} end,
    Compute = fun(Split) -> Split end,
    Dependencies = {none, []} ,
    Partitioner = fun() -> none end,
    Lineage = [none],

    #{partitions => Partitions, compute => Compute,
      dependencies => Dependencies, partitioner => Partitioner,
      linage => Lineage}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("kernel/include/file.hrl").

split_file(Split, DataFile, PDataFile) ->
    {Index, SplitSize} = Split,
    Start = Index * SplitSize,
    End = Start + SplitSize,
    Skip = case Start > 0 of
        true -> CurSeek = Start -1,
                Byte = file:pread(PDataFile, CurSeek, 1),
                TempSkip = not (Byte =:= "\n"),
                TempSkip;
        false -> flase
    end,
    file:position(DataFile, Start),
    SkipLen = case Skip of
        true -> length(io:get_line(DataFile));
        _ -> 0
    end,
    text_read(DataFile, Start + SkipLen, End).

text_read(DataFile, Start, End)
         when Start >= End ->
         [];
text_read(DataFile, Start, End) ->
    file:position(DataFile, Start),
    Line = io:get_line(DataFile, ''),
    [Line|text_read(DataFile,Start+length(Line),End)].

text_file_rdd(Path, NumSplits, SplitSize) ->
    {ok, DataFile} = file:open(Path, [read,raw]),
    FileSize = DataFile#file_info.size,

    NewSplitSize = case SplitSize of
        none -> case NumSplits of
                    none -> 64*1024*1024; %% 64M
                    _ -> FileSize div NumSplits
                end;
        _ -> SplitSize
    end,

    N = FileSize div NewSplitSize,
    NewNumSplits =  case FileSize rem NewSplitSize > 0 of
        true -> N + 1;
        false -> N
    end,

    Partitions = [{Index,NewSplitSize} || Index <- lists:seq(0,NewNumSplits-1)],
    Compute = fun(Split, DataFile) -> wait end,
    Dependencies = {none, []} ,
    Partitioner = fun() -> none end,
    Lineage = [none],

    #{partitions => Partitions, compute => Compute,
      dependencies => Dependencies, partitioner => Partitioner,
      linage => Lineage}.