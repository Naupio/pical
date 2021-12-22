-module (ta_aggregator).
-compile(export_all).

add_aggregator(createCombiner, Ele) ->
    Ele.
%% attention head mismatch
add_aggregator(mergeValues, Combiner, Ele) ->
    Combiner + Ele;
add_aggregator(mergeCombiner, CombinerA, CombinerB) ->
    CombinerA + CombinerB.

test_add_aggregator() ->
    Ele = 1,
    Acombiner = add_aggregator(createCombiner, Ele),
    Bcombiner = add_aggregator(mergeValues,Acombiner, 2),
    CombinerA = add_aggregator(mergeCombiner, Acombiner,Bcombiner),
    Acombiner = 1,
    Bcombiner = 3,
    CombinerA = 4,
    io:format("test_add_aggregator ok\n").

list_aggregator(createCombiner, Ele) ->
    [Ele].
%% attention head mismatch
list_aggregator(mergeValues, Combiner, Ele) ->
    lists:reverse([Ele|Combiner]);
list_aggregator(mergeCombiner, CombinerA, CombinerB) ->
    CombinerA ++ CombinerB.

test_list_aggregator() ->
    Ele = 1,
    Acombiner = list_aggregator(createCombiner, Ele),
    Bcombiner = list_aggregator(mergeValues,Acombiner, 2),
    CombinerA = list_aggregator(mergeCombiner, Acombiner,Bcombiner),
    Acombiner = [1],
    Bcombiner = [1,2],
    CombinerA = [1,1,2],
    io:format("test_list_aggregator ok\n").

uniq_aggregator(createCombiner, Ele) ->
    [Ele].
%% attention head mismatch
uniq_aggregator(mergeValues, Combiner, Ele) ->
    maps:keys(maps:from_list(lists:zip([Ele|Combiner],
                             lists:seq(1,1 + length(Combiner)))));
uniq_aggregator(mergeCombiner, CombinerA, CombinerB) ->
    LongCombiner = CombinerA++CombinerB,
    ZipLongConbiner = lists:zip(LongCombiner, lists:seq(1, length(LongCombiner))),
    maps:keys(maps:from_list(ZipLongConbiner)).

test_uniq_aggregator() ->
    Ele = 1,
    Acombiner = uniq_aggregator(createCombiner, Ele),
    Bcombiner = uniq_aggregator(mergeValues,Acombiner, 2),
    CombinerA = uniq_aggregator(mergeCombiner, Acombiner,Bcombiner),
    Acombiner = [1],
    Bcombiner = [1,2],
    CombinerA = [1,2],
    io:format("test_uniq_aggregator ok\n").