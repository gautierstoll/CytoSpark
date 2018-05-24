

//import java.nio.ByteBuffer

val exp12FCS = new FCSParserCompact("Exp 12 T cells_Tube_001.fcs",-1000)

val kMeanExp12_0 = exp12FCS.kmeansCompensated(KMeanFCSInput(6,1000,100,0))

//val kMeanExp12_0Val = kMeanExp12_0.clusters.toArray.zip(kmeans.matToSparse(exp12FCS.dataCompensatedMatFCS)).
//  map( x => kmeans.euclid(x._2,kMeanExp12_0.means(x._1))).sum

val kMeanExp12_10 = exp12FCS.kmeansCompensated(KMeanFCSInput(6,1000,100,10))

//val kMeanExp12_10Val = kMeanExp12_10.clusters.toArray.zip(kmeans.matToSparse(exp12FCS.dataCompensatedMatFCS)).
//  map( x => kmeans.euclid(x._2,kMeanExp12_10.means(x._1))).sum


val kMeanExp12_100 = exp12FCS.kmeansCompensated(KMeanFCSInput(6,1000,100,100))

//val kMeanExp12_100Val = kMeanExp12_100.clusters.toArray.zip(kmeans.matToSparse(exp12FCS.dataCompensatedMatFCS)).
//  map( x => kmeans.euclid(x._2,kMeanExp12_100.means(x._1))).sum


val kMeanExp12_50 = exp12FCS.kmeansCompensated(KMeanFCSInput(6,1000,100,50))

//val kMeanExp12_50Val = kMeanExp12_50.clusters.toArray.zip(kmeans.matToSparse(exp12FCS.dataCompensatedMatFCS)).
//  map( x => kmeans.euclid(x._2,kMeanExp12_50.means(x._1))).sum

val kMeanExp12_Big = exp12FCS.kmeansCompensated(KMeanFCSInput(6,10000,100,50))

