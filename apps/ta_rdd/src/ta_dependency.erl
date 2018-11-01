-module(ta_dependency).
-compile(export_all). % Just for test, remember to delete it when the project release.

one_to_one_dependency(RDD) ->
    #{rdd => RDD,
      get_parents => fun(Pid) -> [Pid] end}.

range_dependency(RDD, InStart, OutStart, Length) ->
    GetParents = fun(Pid) ->
        case Pid >= OutStart andalso Pid < OutStart + Length of
            true -> [Pid - OutStart + InStart];
            false -> []
        end
    end,

    #{rdd => RDD,
      get_parents => fun(Pid) -> GetParents(Pid) end}.

shuffle_dependency(ShuffleID, RDD, Aggregator, Partitioner) ->
    #{
    shuffle_id => ShuffleID,
    rdd => RDD, 
    aggregator => Aggregator,
    partitioner => Partitioner 
    }.
