-module (ta_partitioner).
-compile(export_all).

partition_find(_,[]) ->
    none; 
partition_find(Key, ZipKeys) ->
    [{Head1,Head2}|TailZipKeys] = ZipKeys,
    case Key =:= Head1 of
        true -> Head2;
        false  -> partition_find(Key,TailZipKeys)
    end.
range_partition_find(Key, [KeyHead|KeysTail]) ->
    ZipKeys = lists:zip([KeyHead|KeysTail], lists:seq(0,length([KeyHead|KeysTail])-1)),
    partition_find(Key,ZipKeys).

test_range_partition_find()->
    2 = range_partition_find(a,[b,c,a,d]),
    io:format("test_range_partition_find ok\n").

range_partitioner(Keys) ->
    SortKeys = lists:sort(Keys),
    GetPartiton = fun(Key) -> range_partition_find(Key,SortKeys) end,
    #{
    keys => SortKeys,
    num_partitions => length(SortKeys) + 1,
    get_partition => GetPartiton
    }.

hash_partitioner(Patitioners) ->
    NumPartition = max(1, Patitioners),
    #{
    num_partitions => NumPartition,
    get_partition => fun(Key) -> erlang:phash(Key, NumPartition) end
    }.
