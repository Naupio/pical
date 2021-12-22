-module (ta_schedule).
-compile(export_all).

stage(Lineage) ->
    [Head|Tail] = Lineage,
    Head =:= shuffule_dependency,
    unimplement.

ta_schedule(FinalRDD, StageList, CurStage) ->
    Lineage = maps:get(lineage, FinalRDD),
    [Head|Tail] = Lineage,
    unimplement.


task_schedule() ->
    unimplement.