package uk.ac.open.kmi.decipher.classifier;

import java.io.File;
import java.io.IOException;
import java.util.Random;

import weka.classifiers.Classifier;
import weka.classifiers.Evaluation;
import weka.classifiers.trees.J48;
import weka.core.Instances;
import weka.core.converters.CSVLoader;

public class J48Classifier {

	public Double classify(String input_fn){
		
		
		CSVLoader loader = new CSVLoader();
		String[] options = new String[1];
		options[0] = "-U";            // unpruned tree
		J48 classifier = new J48();         // new instance of tree
		
		try {
//			loader.setFile(new File("/Decipher/Documents/Annika/clustering/time.csv"));
			loader.setFile(new File(input_fn));
			classifier.setOptions(options);
			Instances data = loader.getDataSet();
			data.setClassIndex(data.numAttributes() - 1);
			int folder=data.numInstances()<10? data.numInstances():10;

			classifier.buildClassifier(data);   // build classifier
//            System.out.println(classifier.toString());
            
            Evaluation eval = new Evaluation(data);
            eval.crossValidateModel(classifier, data, folder, new Random(1));
//            System.out.println(eval.toSummaryString());           
 //           System.out.println("Correctly Classified Instances Rate: " + (1-eval.errorRate()));

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
		J48Classifier classifier= new J48Classifier();
		classifier.classify(null);
	}
}
