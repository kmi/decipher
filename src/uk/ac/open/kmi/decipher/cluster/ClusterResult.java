package uk.ac.open.kmi.decipher.cluster;

import weka.core.Instances;

public class ClusterResult {

	Instances centroids;
	int[] assignments;
	int numberOfClusters;
	int[] clusterSize;

	public ClusterResult(Instances clu_center, int[] clu_aAssignments, int num_cluster, int clu_size[]) {
		this.centroids = clu_center;
		this.assignments = clu_aAssignments;
		this.numberOfClusters = num_cluster;
		this.clusterSize= clu_size;

	}

	

	public int[] getClusterSize() {
		return clusterSize;
	}



	public void setClusterSize(int[] clusterSize) {
		this.clusterSize = clusterSize;
	}



	public int getNumberOfClusters() {
		return numberOfClusters;
	}

	public void setNumberOfClusters(int numberOfClusters) {
		this.numberOfClusters = numberOfClusters;
	}

	public Instances getCentroids() {
		return centroids;
	}

	public void setCentroids(Instances centroids) {
		this.centroids = centroids;
	}

	public int[] getAssignments() {
		return assignments;
	}

	public void setAssignments(int[] assignments) {
		this.assignments = assignments;
	}

}
