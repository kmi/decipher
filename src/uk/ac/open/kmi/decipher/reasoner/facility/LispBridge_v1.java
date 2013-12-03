package uk.ac.open.kmi.decipher.reasoner.facility;

import java.net.URL;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.armedbear.lisp.AbstractString;
import org.armedbear.lisp.CharHashMap;
import org.armedbear.lisp.ComplexString;
import org.armedbear.lisp.Cons;
import org.armedbear.lisp.Fixnum;
import org.armedbear.lisp.Function;
import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.JavaObject;
import org.armedbear.lisp.Lisp;
import org.armedbear.lisp.LispObject;
import org.armedbear.lisp.Symbol;
import org.armedbear.lisp.Packages;
import org.armedbear.lisp.SimpleString;
import org.armedbear.lisp.Java;


import uk.ac.open.kmi.decipher.reasoner.PlotCluster;

public class LispBridge_v1 {
    
	private static LispBridge_v1 instance = null;

	Interpreter interpreter;
	private LispBridge_v1() {
		interpreter = Interpreter.createInstance();
		URL codeUrl = this.getClass().getClassLoader().getResource("codes/decipher-atms-single-file.lisp");
		interpreter.eval("(load \""+ codeUrl+ "\")");
	}
	static {
		instance = new LispBridge_v1();
	}

	public static LispBridge_v1 getInstance () {
		return instance;
	}

	public Map<String, Map<String, Set<String>>> initPara(){
		Map<String, Map<String, Set<String>>> PlotElements = new HashMap<String,Map<String, Set<String>>>();

		Map<String, Set<String>> pElement1 = new HashMap<String, Set<String>>();
		pElement1.put("SOURCE", new HashSet(Arrays.asList("E1", "E2")));
		pElement1.put("CONSEQUENCE", new HashSet(Arrays.asList("E3", "E4")));
		PlotElements.put("PE1", pElement1);

		Map<String, Set<String>> pElement3 = new HashMap<String, Set<String>>();
		pElement3.put("SOURCE", new HashSet(Arrays.asList("E1", "E4")));
		pElement3.put("CONSEQUENCE", new HashSet(Arrays.asList("E3", "E2")));
		PlotElements.put("PE3", pElement3);

		Map<String, Set<String>> pElement2 = new HashMap<String, Set<String>>();
		pElement2.put("RELATED", new HashSet(Arrays.asList("E1", "E2", "E3", "E4")));
		PlotElements.put("PE2", pElement2);

		Map<String, Set<String>> pElement4 = new HashMap<String, Set<String>>();
		pElement4.put("RELATED", new HashSet(Arrays.asList("E1", "E2", "E3", "E4")));
		PlotElements.put("PE4", pElement4);

		return PlotElements;
	}

	public Set<Set<String>> bridge(Map<String, Map<String, Set<String>>> in_param){
		//load a file of Lisp functions
		if (interpreter == null) {
			interpreter = Interpreter.createInstance(); 
			URL codeUrl = this.getClass().getClassLoader().getResource("codes/decipher-atms-single-file.lisp");
			interpreter.eval("(load \""+ codeUrl+ "\")");

		}
	//	 interpreter.eval("(load \"/Decipher/codes/decipher-atms-single-file.lisp\")");
	//	 interpreter.eval("(load \"http://people.kmi.open.ac.uk/ning/atms/decipher-atms-single-file.lisp\")");
		 
		// then load the package containing a function you want to call
	     org.armedbear.lisp.Package ATMSPackage = Packages.findPackage("CL-USER");	
	    
	   //get the function called my-function defined in my-lisp-code.lisp
	    Symbol myFunctionSym = ATMSPackage.findAccessibleSymbol("RUN-ATMS-FROM-PLOT-DATA-STRING");	    
	    Function myFunction = (Function) myFunctionSym.getSymbolFunction();
	    
	    String inputAsStr="(";
	    Set<String> peKeys = in_param.keySet();
	    for (String peKey:peKeys)
	    {  
	    	String peStr="("+ peKey.replace("/", "@") + " "; //e.g. change 3820/3848 to 3820@3848
	    	Map<String, Set<String>> plotValues = in_param.get(peKey);	    	
	    	Set<String> types = plotValues.keySet();
	    	for (String type:types)
	    	{   String eventStr = "(" + type + " (";
	    		Set<String> peValues = plotValues.get(type);
	    		for (String peValue:peValues)
	    		{
	    			eventStr +=peValue + " ";
	    		}
	    		eventStr =eventStr.substring(0,eventStr.length()-1) + "))";
	    		peStr += eventStr + " ";
	    		
	    	}
	    	peStr = peStr.substring(0,peStr.length()-1) + ")";
	    	inputAsStr +=peStr +" ";
	    }	    
	    inputAsStr = inputAsStr.substring(0,inputAsStr.length()-1) + ")";
	    
	    System.out.println("the ATMS input is:" + inputAsStr);
	    LispObject parameter = JavaObject.getInstance(inputAsStr);
	    
	   //call the function. ABCL's Cons Java class corresponds to a Lisp list
	    SimpleString resultStr = (SimpleString)myFunction.execute(parameter);
	    System.out.println("the ATMS output is: " + resultStr.toString());
	    Set<Set<String>> result = new HashSet<Set<String>>();	    
	    String temp = resultStr.toString().substring(2,resultStr.toString().length()-2);//remove the double brackets at both ends
	    String[] splittedStr = temp.split("\\) \\(");
	    for (int i=0; i<splittedStr.length; i++)
	    {
	    	String[] PEs = splittedStr[i].split(" ");
	    	Set<String> peSet= new HashSet<String>();
		    for (int j=0; j<PEs.length; j++)
		    	peSet.add(PEs[j].replace("@", "/"));
		    result.add(peSet);
	    }
	    
		return result;
	}	
	public static void main(String[] args) {
//		String dossier = "http://mckm219286.open.ac.uk/decipher/trunk/decipher-3/cstory/2727";
//		String queryURI = "http://localhost:8080/openrdf-sesame/repositories/Decipher?query=";
		LispBridge_v1 lBridge= new LispBridge_v1();
		lBridge.bridge(lBridge.initPara());
	}	
}
