-module (ta_task).
-compile(export_all).

result_task(StageID, RDD, Func, Partition) ->
    [Partitions, _, _, _, _] = RDD,
    Split = maps:get(Partition, Partitions),
    Run = fun() -> Func(Split) end ,
    Run.

split_task(Buckets,[],Partitioner, Aggregator) ->
    Buckets;
split_task(Buckets, Split, Partitioner, Aggregator) ->
    [{K,V}|SplitTail] = Split,
    BucketID = maps:get(get_partition, Partitioner),
    Bucket = maps:get(BucketID, Buckets),
    NewBucket = case masp:is_key(K, Bucket) of
        true -> 
            #{K := OldValue } = Bucket,
            Bucket#{K => Aggregator(mergeValues,OldValue,V)};
        false -> Bucket#{K => Aggregator(createCombiner,V)}
    end,
    NewBuckets = Buckets#{ BucketID => NewBucket},
    split_task(NewBuckets,SplitTail,Partitioner,Aggregator).


shuffle_map_task(StageID, RDD, Dep, Partition) ->
    [Partitions, _, _, _, _] = RDD,
    Split = maps:get(Partition, Partitions),
    ShuffleID = maps:get(shuffle_id, Dep),
    Aggregator = maps:get(aggregator, Dep),
    Partitioner = maps:get(partitioner, Dep),
    
    NumPartition = maps:get(num_partitions, Partitioner),
    IndexList = lists:seq(1, NumPartition),
    ListBuckets = [{}|| _ <- IndexList],

    Buckets = maps:from_list(lists:zip(IndexList, ListBuckets)),
    Run = fun() -> split_task(Buckets, Split, Partitioner, Aggregator) end,
    Run.