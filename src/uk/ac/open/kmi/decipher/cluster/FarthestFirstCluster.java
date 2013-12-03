package uk.ac.open.kmi.decipher.cluster;

import weka.clusterers.AbstractClusterer;
import weka.core.Instances;
import weka.core.Utils;
import weka.filters.Filter;
import weka.filters.unsupervised.attribute.AddCluster;

public class FarthestFirstCluster implements IAlgorithm{

	public ClusterResult cluster(String in, String out) {
		// TODO Auto-generated method stub
		//FarthestFirst clusterer = new FarthestFirst();
				//clusterer.setNumClusters(4);
				//clusterer.setSeed(1);
		AddCluster unsupervised = new AddCluster();
        	try {
					String[] options = Utils.splitOptions("-N 4 -S 1");
					unsupervised.setClusterer(AbstractClusterer.forName("weka.clusterers.FarthestFirst", options));
					String[] clu_opt = {"-i","/Decipher/Documents/Annika/clustertest.arff", "-o", "/Decipher/Documents/Annika/clustertestres2.arff"};
					Filter.filterFile(unsupervised, clu_opt);
				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			return null;
				
		/*		
				String clustererString = algorithm.getClass().getName();
			    String[] clustererSpec = Utils.splitOptions(clustererString);
			    if (clustererSpec.length == 0) {
			      throw new Exception("Invalid clusterer specification string");
			    }
			    String clustererName = clustererSpec[0];
			    clustererSpec[0] = "";
			    unsupervised.setClusterer(AbstractClusterer.forName(clustererName, clustererSpec));
		*/	
//				unsupervised.setClusterer(clusterer);   		
				
	}

}
