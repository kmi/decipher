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

public class LispBridge {
    
	private static LispBridge instance = null;

	Interpreter interpreter;
	private LispBridge() {
		interpreter = Interpreter.createInstance();
		URL codeUrl = this.getClass().getClassLoader().getResource("codes/lisp-process-input.lisp");
		interpreter.eval("(load \""+ codeUrl+ "\")");
	}
	static {
		instance = new LispBridge();
	}

	public static LispBridge getInstance () {
		return instance;
	}

	public String bridge(String inStr){
		//load a file of Lisp functions
		if (interpreter == null) {
			interpreter = Interpreter.createInstance(); 
			URL codeUrl = this.getClass().getClassLoader().getResource("codes/lisp-process-input.lisp");
			interpreter.eval("(load \""+ codeUrl+ "\")");

		}
	//	 interpreter.eval("(load \"http://people.kmi.open.ac.uk/ning/atms/decipher-atms-single-file.lisp\")");
		 
		// then load the package containing a function you want to call
	     org.armedbear.lisp.Package ATMSPackage = Packages.findPackage("CL-USER");	
	    
	   //get the function called my-function defined in my-lisp-code.lisp
	    Symbol myFunctionSym = ATMSPackage.findAccessibleSymbol("PROCESS-INPUT"); //the function name must be in capital letters    
	    Function myFunction = (Function) myFunctionSym.getSymbolFunction();
	    
	    LispObject parameter = JavaObject.getInstance(inStr);
	    
	   //call the function. ABCL's Cons Java class corresponds to a Lisp list
	    SimpleString resultStr = (SimpleString)myFunction.execute(parameter);
	    System.out.println("the ATMS output is: " + resultStr.toString());
	    
		return resultStr.getStringValue();
	}	
	public static void main(String[] args) {
//		String dossier = "http://mckm219286.open.ac.uk/decipher/trunk/decipher-3/cstory/2727";
//		String queryURI = "http://localhost:8080/openrdf-sesame/repositories/Decipher?query=";
		LispBridge lBridge= new LispBridge();
//		lBridge.bridge(lBridge.initPara());
	}	
}
