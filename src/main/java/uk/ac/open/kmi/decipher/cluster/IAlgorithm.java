package uk.ac.open.kmi.decipher.cluster;

import weka.core.Instances;

public interface IAlgorithm {

	
	ClusterResult cluster(String infile, String outfile);
	

}
