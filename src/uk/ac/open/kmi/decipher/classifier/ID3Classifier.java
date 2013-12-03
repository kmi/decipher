package uk.ac.open.kmi.decipher.classifier;

import java.io.File;
import java.io.IOException;

import uk.ac.open.kmi.decipher.reasoner.DossierTest;
import weka.classifiers.Classifier;
import weka.classifiers.trees.Id3;
import weka.classifiers.trees.J48;
import weka.core.Instances;
import weka.core.converters.CSVLoader;


import weka.classifiers.Evaluation;
import java.util.Random;

public class ID3Classifier {
	// load data
	public Double classify(String input_fn){
		
		Classifier id3_class = new Id3();
		
		CSVLoader loader = new CSVLoader();
		String[] options = new String[1];
//		options[0] = "-U";            // unpruned tree
//		J48 tree = new J48();         // new instance of tree
		
		try {
//			loader.setFile(new File("/Decipher/Documents/Annika/clustering/location.csv"));
			loader.setFile(new File(input_fn));
//			id3_class.setOptions(options);
			Instances data = loader.getDataSet();
			data.setClassIndex(data.numAttributes() - 1);
			int folder=data.numInstances()<10? data.numInstances():10;
		    
			id3_class.buildClassifier(data);   // build classifier
            
            Evaluation eval = new Evaluation(data);
            eval.crossValidateModel(id3_class, data, folder, new Random(1));
   //         System.out.println(eval.toSummaryString());           
   //         System.out.println("Correctly Classified Instances Rate: " + eval.pctCorrect());
            return eval.pctCorrect();
            
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}
	
	public static void main(String[] args) throws Exception{
		ID3Classifier classifier= new ID3Classifier();
		classifier.classify(null);
	}
	
}
