package uk.ac.open.kmi.decipher.cluster;

import java.util.Set;

import weka.clusterers.ClusterEvaluation;
import weka.clusterers.FilteredClusterer;
import weka.clusterers.SimpleKMeans;
import weka.core.Instances;
import weka.core.converters.ConverterUtils.DataSource;
import weka.filters.unsupervised.attribute.Remove;

 

public class KMeansCluster implements IAlgorithm{
	
	int numberOfCluster;
	
	
	public int getNumberOfCluster() {
		return numberOfCluster;
	}


	public void setNumberOfCluster(int numberOfCluster) {
		this.numberOfCluster = numberOfCluster;
	}


	@Override
	public ClusterResult cluster(String in, String out) {
		// TODO Auto-generated method stub
		
		SimpleKMeans kmeans = new SimpleKMeans();
		kmeans.setPreserveInstancesOrder(true);
		kmeans.setSeed(10);
		
		try {
			kmeans.setNumClusters(numberOfCluster);
		    DataSource source = new DataSource(in);
		    Instances data = source.getDataSet();		    
			kmeans.buildClusterer(data);
			
			Instances ClusterCenter = kmeans.getClusterCentroids();
		    int[] ClusterSize = kmeans.getClusterSizes(); 
		    int numOfClusters = kmeans.getNumClusters();

	/*	    ClusterEvaluation eval = new ClusterEvaluation();
		    eval.setClusterer(kmeans);
		    eval.evaluateClusterer(data);
	*/	   

		    for(int i=0;i<ClusterCenter.numInstances();i++){
		        System.out.println("Cluster #"+( i +1)+ ": "+ClusterSize[i]+ "instances.");
		        System.out.println("Centroid:"+ ClusterCenter.instance(i));
//		        System.out.println("STDDEV:" + SDev.instance(i));
//		        System.out.println("Cluster Evaluation:"+eval.clusterResultsToString());
		    }
			
		    
			int[] assignments = kmeans.getAssignments();
			int i=0;
			for(int clusterNum : assignments) {
			    System.out.printf("Instance %d -> Cluster %d", i, clusterNum);	
			    i++;
			}
						
			return new ClusterResult(ClusterCenter,assignments,numOfClusters,ClusterSize);
        } catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;		
				
	}
	

	public ClusterResult clusterIgnoreAttributes(String in, String out, int[] usedAttrs, int[] ignoredAttrs) {
		// TODO Auto-generated method stub
		
		try {
			DataSource source = new DataSource(in);
		    Instances data = source.getDataSet();
		    int attrsNum = data.numAttributes();
	//	    System.err.println(data.attribute(1));
		    String attr_nums_para = "";
		  		
		    if(usedAttrs!=null)
		      for (int i=0;i<attrsNum;i++)
		    	{
		    		boolean chosen=false;
		    		for (int j=0;j<usedAttrs.length-1;j++)		    		
		    			if (i==usedAttrs[j])
		    				chosen =true;
		    		if (!chosen) attr_nums_para +=i+",";				    	
		    	}
		    if(ignoredAttrs!=null)
		    	for (int j=0;j<ignoredAttrs.length-1;j++)
		    		attr_nums_para +=ignoredAttrs[j] + ",";
		    attr_nums_para +=ignoredAttrs[ignoredAttrs.length-1];
	  		  
		    System.err.println (attr_nums_para);
		    			    
			FilteredClusterer fc = new FilteredClusterer(); //filtered clusterer to ignore attributes

			String[] options = new String[2];
			options[0] = "-R"; // "range"			
			options[1] = attr_nums_para; // we want to ignore the attribute that is in the position '1'
			Remove remove = new Remove(); // new instance of filter
			remove.setOptions(options); // set options
			remove.setInputFormat(data); // inform filter about dataset
			
			SimpleKMeans kmeans = new SimpleKMeans();
			kmeans.setPreserveInstancesOrder(true);
			kmeans.setSeed(10);
			kmeans.setNumClusters(numberOfCluster);
			
			fc.setFilter(remove); //add filter to remove attributes
			fc.setClusterer(kmeans); //bind FilteredClusterer to original clusterer
			
			fc.buildClusterer(data);
			
			Instances ClusterCenter = kmeans.getClusterCentroids();
		    int[] ClusterSize = kmeans.getClusterSizes(); 
		    int numOfClusters = kmeans.getNumClusters();

	/*	    ClusterEvaluation eval = new ClusterEvaluation();
		    eval.setClusterer(kmeans);
		    eval.evaluateClusterer(data);
	*/	   

		    for(int i=0;i<ClusterCenter.numInstances();i++){
		        System.out.println("Cluster #"+( i +1)+ ": "+ClusterSize[i]+ "instances.");
		        System.out.println("Centroid:"+ ClusterCenter.instance(i));
//		        System.out.println("STDDEV:" + SDev.instance(i));
//		        System.out.println("Cluster Evaluation:"+eval.clusterResultsToString());
		    }
			
		    
			int[] assignments = kmeans.getAssignments();
			int i=0;
			for(int clusterNum : assignments) {
			    System.out.printf("Instance %d -> Cluster %d", i, clusterNum);	
			    i++;
			}
						
			return new ClusterResult(ClusterCenter,assignments, numOfClusters, ClusterSize);
        } catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;		
				
	}


}
