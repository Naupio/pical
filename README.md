# pita  
(Work In Process) **pita** is a general distributed computation system with **Erlang** language base on DAG model. This project is inspired by [DouBan 's DPark](https://github.com/douban/dpark) and [Apache Spark](https://github.com/apache/spark).  

# LICENSE
- The [MIT License](./LICENSE)  
- Copyright (c) 2016-2018 [Naupio Z.Y Huang](https://github.com/Naupio)  

# WARNING
This project is **not finish** (yet).

---
---

# **DAG Engine TODO LIST**
# RDD  

* getPartition  
* compute  
* Dependency  
* Partitioner for K-V RDDs (Optional)  
* preferredLocations (Optional)  

# BaseRDD  
```
ParallelCollectionRDD
MappedRDD
FlatMappedRDD
MapPartitionsRDD
MappedValuesRDD
FlatMappedValuesRDD
FilteredRDD
ShuffledRDD
TextFileRDD
OutputTextFileRDD
UnionRDD
CoGroupedRDD
CartesianRDD
CoalescedRDD
SampleRDD
CheckpointRDD

```
**`PipedRDD`**

# DataSource
* parallelize :> ParallelCollectionRDD  
* textFile :> TextFileRDD  

# Transformation  
## simpleTransformation
* map(func) :> MappedRDD  
compute:> iterator(split).map(f)  

* filter(func) :> FilteredRDD  
compute:> iterator(split).filter(f)  

* flatMap(func) :> FlatMappedRDD  
compute:> iterator(split).flatMap(f)  

* mapPartitions(func) :> MapPartitionsRDD  
compute:> f(iterator(split))  

* mapPartitionsIndex(func) :> MapPartitionsRDD  
compute:> f(split.index, iterator(split))  

* sample(withReplacement, fraction, seed) :> PartitionwiseSampledRDD  
compute:>  
PoissonSampler.sample(iterator(split))
BernoulliSampler.sample(iterator(split))

## complexThansformation
* union(otherDataset) :> (RDD a, RDD b) => UnionRDD  
* groupByKey([numTasks]) :>  RDD a => ShuffledRDD => MapPartitionsRDD  
* reduceByKey(func, [numTasks]) :> RDD a => MapPartitionsRDD => ShuffledRDD => MapPartitionsRDD  
* distinct([numTasks])) :> RDD a => MappedRDD => MapPartitionsRDD => ShuffledRDD => MapPartitionsRDD => MappedRDD  
* cogroup(otherDataset, [numTasks]) :> (RDD a, RDD b) => CoGroupedRDD => MappedValuesRDD  
* intersection(otherDataset) :> (RDD a, RDD b) => (MappedRDD a, MappedRDD b) => CoGroupedRDD => MappedValuesRDD => FilteredRDD => MappedRDD  
* join(otherDataset, [numTasks]) :> (RDD a, RDD b) => CoGroupedRDD => MappedValuesRDD => FlatMappedValuesRDD  
* sortByKey([ascending], [numTasks]) :> RDD a => ShuffledRDD => MapPartitionsRDD
* cartesian(otherDataset) :> (RDD a, RDD b) =>  CartesianRDD
* coalesce(numPartitions,shuffle=false) :> RDD a => CoalescedRDD
* repartition(numPartitions) == coalesce(numPartitions,shuffle=true) :> RDD a => MapPartitionsRDD => ShuffledRDD => CoalescedRDD => MappedRDD  
* combineByKey() :> 	aggregate and compute()
```
combineByKey(createCombiner:	V	=>	C,
						mergeValue:	(C,	V)	=>	C,
						mergeCombiners:	(C,	C)	=>	C,
						partitioner:	Partitioner,
						mapSideCombine:	Boolean	=	true,
						serializer:	Serializer	=	null):	RDD[(K,	C)])
```
* **pipe(command,	[envVars])  :> PipedRDD**  

# Action  
* reduce(func) :> (record1, record2) =>  result, (result, record i) => result  
compute(results) :> (result1,	result2)	=>	result,	(result, result i) => result  

* collect() :> Array[records] => result  
compute(results) :> Array[result]  

* count() :> count(records) => result  
compute(results) :> sum(result)  

* foreach(f) :> f(records) => result  
compute(results) :> Array[result]  

* take(n) :> record(i<n) => result  
compute(results) :> Array[result]  

* frist() :> record 1 => result  
compute(results) :> Array[result]  

* takeSample() :> selectd records => result  
compute(results) :> Array[result]  

* takeOrdered(n,[ordering]) :> TopN(records) => result  
compute(results) :> TopN(results)  

* saveAsFile(path) :> records => write(records)  
compute(results) :> null  

* countByKey() :> (K, V) => Map(K, count(K))  
compute(results) :> (Map,	Map)	=>	Map(K,	count(K))  

---

# Partitioner
* HashPartitioner  
* RangePartitioner  

# Aggregator
* createCombiner  
* mergeValue  
* mergeCombiner  

---

# Dependency

## NarrowDenpendency  
* OneToOneDependency (1:1)    
* RangeDependency  
* NarrowDenpendency (N:1)

## WideDenpendency  
* ShuffleDependency (M:N)  


---

# Scheduler
## DAGScheduler
* one ShuffleDependency one stage  

## TaskScheduler
* one finalRDD-partition one task  

## Job  
* runJob(rdd,	processPartition,	resultHandler)  
* runJob(rdd,	cleanedFunc,	partitions,	allowLocal,	resultHandler)  
* submitJob(rdd,	func,	partitions,	allowLocal,	resultHandler)  
* handleJobSubmmitted()  

## Stage
* noParentStage computeSoon  
* haveParentStage waitParentComputeFinish  
* newStage()  
* submitStage(finalStage)  
* submitWaitingStages()  

**ShuffleMapStage**  
**ResultStage**  

## Task  
* ShuffleMapTask  
* ResultTask  
* TaskSet
* submitTasks(taskSet)
* LaunchTask(new	SerializableBuffer(serializedTask))

---

# Shuffle  
## Shuffle	write  
ShuffleBlockFile/FileSegment :> record => partition => persist in bucket
FileConsolidation	:> cores *	R  

## Shuffle	read  
fetch and combine (aggregate in HashMap)

---
# RTS  
* masterNode  
* workerNode  
* driverNode  
* executorBackend  
* executorRunner  

---
# Persist
* Cache  
* Checkpoint

---
# Accumulator
* value  
* list  
* set  
* dict  

---
# Broadcast
* BroadcastManager
* P2PBroadcastManager


---
