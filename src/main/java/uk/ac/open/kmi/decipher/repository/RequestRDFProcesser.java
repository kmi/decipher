package uk.ac.open.kmi.decipher.repository;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;

import org.ontoware.aifbcommons.collection.ClosableIterator;
import org.ontoware.rdf2go.exception.ModelRuntimeException;
import org.ontoware.rdf2go.model.QueryResultTable;
import org.ontoware.rdf2go.model.QueryRow;
import org.openrdf.rdf2go.RepositoryModel;
import org.openrdf.repository.RepositoryException;

public class RequestRDFProcesser {

	private static final String REPOSITORY_URL="http://localhost:8080/openrdf-sesame";
	private static final String REPOSITORY_NAME="Decipher";
	
	public RDFRepositoryConnector RDF_rep_conn;
/*	
	private static RequestRDFProcesser _instance; 
	public static RequestRDFProcesser getInstance() {
		  return _instance;
		 } 
*/
	public void RequestRDFProcesser()
	{			
		try {
			RDF_rep_conn = new RDFRepositoryConnector(REPOSITORY_URL,REPOSITORY_NAME);
		} catch (RepositoryException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}	 
	}

	public void loadRDF2Repository(String rdfFileURI, String contextID) {
			
		RepositoryModel rep_model = RDF_rep_conn.openRepositoryModel();
		try {
			
			  URL u = new URL(rdfFileURI);
			  URLConnection uc = u.openConnection();
			  uc.connect();
			  InputStream in = uc.getInputStream();
              rep_model.readFrom(in);            

		} catch (ModelRuntimeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}    
		if (rep_model != null) rep_model.close();
	}
	
	public void loadInvocationData2Repository(String rdfFileURI, String contextID, String opName, String invokeID) {
		
	  /*  if (contextID.contains(".rdf"))
			contextID =contextID.replace(".rdf", "");
		contextID =contextID +"/" +opName+ "/Invoke" +invokeID;
	   */	
		System.out.println("context ID is:" + contextID);
		RepositoryModel rep_model = RDF_rep_conn.openRepositoryModel(contextID);
		try {
			
			  URL u = new URL(rdfFileURI);
			  URLConnection uc = u.openConnection();
			  uc.connect();
			  InputStream in = uc.getInputStream();
              rep_model.readFrom(in);              
              
              //add extra statements
/*            	URI sub=model.createURI(s);		
        		URI pred = model.createURI(p);
            	URI obj = model.createURI(o); 
        		model.addStatement(sub, pred, obj);    	
*/ 
  
		} catch (ModelRuntimeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}    
		if (rep_model != null) rep_model.close();
	
	}
	
public void loadSD2Repository(String rdfFileURI, String contextID) {
		
		RepositoryModel rep_model = RDF_rep_conn.openRepositoryModel();
		try {
			
			  URL u = new URL(rdfFileURI);
			  URLConnection uc = u.openConnection();
			  uc.connect();
			  InputStream in = uc.getInputStream();
              rep_model.readFrom(in);            

		} catch (ModelRuntimeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}    
		if (rep_model != null) rep_model.close();
	}
	public void queryInputValue() {
		RepositoryModel rep_model = RDF_rep_conn.openRepositoryModel();
				    
	    String prefix="PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>"+
            "PREFIX xsd:<http://www.w3.org/2001/XMLSchema#>" +
            "PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>" +
            "PREFIX owl:<http://www.w3.org/2002/07/owl#>" +
            "PREFIX wsl:<http://www.wsmo.org/ns/wsmo-lite#>" +
            "PREFIX sawsdl:<http://www.w3.org/ns/sawsdl#>" +
            "PREFIX msm:<http://cms-wg.sti2.org/ns/minimal-service-model#>"+
            "PREFIX hrests:<http://www.wsmo.org/ns/hrests#>"+
            "PREFIX purl:<http://www.purl.org/vocabularies/service-ontology#>"+
			"PREFIX location:<http://people.kmi.open.ac.uk/ning/Ontologies/Location.rdf#>"+
			"PREFIX property:<http://people.kmi.open.ac.uk/ning/Ontologies/Property.rdf#>";

        String queryString= prefix +
             	   "SELECT DISTINCT ?part ?in ?value WHERE {" +
                 "?part rdf:type msm:MessagePart." +		
                 "optional {?part sawsdl:modelReference ?modRef.}" +
                 "optional {?in ?modRef ?value.}" +
                 "}";     
       	 if (rep_model!= null)
           {  QueryResultTable results = rep_model.sparqlSelect(queryString);          
              ClosableIterator<QueryRow> iter = results.iterator();
   		      while (iter.hasNext()) 
   		        {
   			      QueryRow row = iter.next();
   			      String in = row.getValue("in").toString();
   			      String value = row.getValue("value").toString();
   			      String part = row.getValue("part").toString();
   			      System.out.println(part + " -> "+ in + " -> " + value + ";" );
   		         }
           }
 		if (rep_model != null) rep_model.close();

	}
	
	public void delRDFFromRepository(String rdfFileURI) {
	    String contextUri = "";
		RepositoryModel rep_model=RDF_rep_conn.openRepositoryModel(contextUri);	
	    rep_model.removeAll();

	}
}
