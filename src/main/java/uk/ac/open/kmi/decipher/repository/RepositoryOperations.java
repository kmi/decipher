package uk.ac.open.kmi.decipher.repository;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.LinkedList;
import java.util.List;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.methods.DeleteMethod;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.methods.StringRequestEntity;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.ValueFactoryImpl;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.rdfxml.RDFXMLParser;
import org.openrdf.rio.rdfxml.RDFXMLWriter;


public class RepositoryOperations {
	private final String STORAGE_SERVICE_ENDPOINT = "http://localhost:8080//openrdf-sesame/repositories/";
	private final String REPO_ID = "Decipher";
	
	private static final String RDF="http://www.w3.org/1999/02/22-rdf-syntax-ns#";
	private static final String RDFS="http://www.w3.org/2000/01/rdf-schema#";
	private static final String OWL="http://www.w3.org/2002/07/owl#";
	private static final String XSD="http://www.w3.org/2001/XMLSchema#";
	private static final String DECIPHER = "http://decipher.open.ac.uk/curate/ontology/";
	private static final String DUL = "http://ontologydesignpatterns.org/ont/dul/DUL.owl#";
	private static final String CRM ="http://www.cidoc-crm.org/rdfs/cidoc_crm_v5.0.2_english_label.rdfs#";
	private static final String LODE="http://linkedevents.org/ontology/";
	private static final String DC="http://purl.org/dc/elements/1.1/";
	
	private static final String queryEndPoint ="http://localhost:8088/openrdf-sesame/repositories/ServiceInvocationSpace?query=";
	
	private static final String PREFIX ="PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>"+
    "PREFIX xsd:<http://www.w3.org/2001/XMLSchema#>" +
    "PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>" +
    "PREFIX owl:<http://www.w3.org/2002/07/owl#>" +
    "PREFIX foaf:<http://xmlns.com/foaf/0.1#>"+
	"PREFIX estate:<http://people.kmi.open.ac.uk/ning/Ontologies/Property.rdf#>"+
	"PREFIX sawsdl:<http://www.w3.org/ns/sawsdl#>" +
	"PREFIX msm:<http://cms-wg.sti2.org/ns/minimal-service-model#>" +
	"PREFIX loc:<http://people.kmi.open.ac.uk/ning/Ontologies/Location.rdf#>";
		          
	
    public static void checkRecord(){
	     	/*String queryString=PREFIX+ "ASK{" +
	    	                   "?user rdf:type foaf:person." +
	    	                   "?user foaf:mbox ?mbox. FILTER regex(str(?mbox), \""+ checkcri + "\", \"i\") }"; 
	    	*/
			
			String queryString=PREFIX+
			       "SELECT DISTINCT ?part ?value WHERE { ?part rdf:type msm:MessagePart."+ 	
	               "  ?part sawsdl:modelReference ?modRef."+
	               "   {?modRef rdf:type rdf:Property." +
	               "    ?anymIn ?modRef ?value.} ";
			
	    	HttpClient client = new HttpClient(); 
	    	GetMethod queryServicesMethod = null;
	    	String queryResult="";
	    	try {
	    		queryServicesMethod = new GetMethod(queryEndPoint + URLEncoder.encode( queryString, "UTF-8"));
	    		queryServicesMethod.setRequestHeader("Accept", "application/sparql-results+xml");
	    	} catch (UnsupportedEncodingException e) {
	    		// TODO Auto-generated catch block
	    		e.printStackTrace();
	    	}    	
	    	try {
	    		client.executeMethod(queryServicesMethod);
	    	    queryResult=queryServicesMethod.getResponseBodyAsString();
	    	    System.out.println(queryResult);
	    	} catch (HttpException httpe) {
	    		// TODO Auto-generated catch block
	    		httpe.printStackTrace();
	    	} catch (IOException ioe) {
	    		// TODO Auto-generated catch block
	    		ioe.printStackTrace();
	    	}finally{
	    		queryServicesMethod.releaseConnection();
	    	}
    } 
	
	/**
	 * @param rdfData
	 * @throws Exception
	 * Post statement topple to repository
	 */
	public void postStatements(String rdfData) throws Exception {

		String requestURL = STORAGE_SERVICE_ENDPOINT + "/repositories/"
				+ REPO_ID + "/statements/";

		PostMethod postMtd = new PostMethod(requestURL);
		HttpClient httpclient = new HttpClient();

		try {

			postMtd.setRequestEntity(new StringRequestEntity(URLEncoder.encode(
					rdfData, "UTF-8"), "application/xml", "UTF-8"));

			int result = httpclient.executeMethod(postMtd);
			System.out.println("Response status code: " + result);
			if (result != 200) {
				System.out.println("Error message: ");
				System.out.println(postMtd.getResponseHeader("Error"));
			} else {
				System.out.println("Response OK");
			}
		} finally {
			postMtd.releaseConnection();
		}
	}

	/**
	 * @param subj
	 * @throws Exception
	 * Delete statements on repository based on subject value
	 */
	public void deleteStatements(String subj) throws Exception {
		String requestURL = STORAGE_SERVICE_ENDPOINT + "/repositories/"
				+ REPO_ID + "/statements/?subj="
				+ URLEncoder.encode(subj, "UTF-8") + "&context="
				+ URLEncoder.encode("defaultContext", "UTF-8");

		DeleteMethod deleteMtd = new DeleteMethod(requestURL);
		HttpClient httpclient = new HttpClient();
		String error = null;
		try {
			int result = httpclient.executeMethod(deleteMtd);
			if (result != 200) {
				error = ((deleteMtd.getResponseHeader("Error") != null) ? deleteMtd
						.getResponseHeader("Error").getValue()
						: "unknown");
			}
		} finally {
			deleteMtd.releaseConnection();
		}
		if (error != null) {
			throw new Exception(error);
		}
	}

	/**
	 * @param userID
	 * @param propID
	 * @param value
	 * @throws Exception
	 * Add property and value to a subject in the repository 
	 */
	public void addProperty(String userID, String propID, String value)
			throws Exception {

		ValueFactory factory = new ValueFactoryImpl();
		StringWriter buffer = new StringWriter();
		RDFXMLWriter writer = new RDFXMLWriter(buffer);
		writer.startRDF();
		// writer.writeStatement(factory.createURI(userID),
		// factory.createURI(propID), factory.createLiteral(value));
		writer.handleStatement(factory.createStatement(factory
				.createURI(userID), factory.createURI(propID), factory
				.createLiteral(value)));
		writer.endRDF();
		postStatements(buffer.getBuffer().toString());
	}

	/**
	 * @param userID
	 * @param propID
	 * @return
	 * @throws Exception
	 * Read property of a statement in the repository
	 */
	public List<String> readProperty(String userID, String propID)
			throws Exception {

		String rdfData = getRDFData(userID, propID, null);
		final List<String> props = new LinkedList<String>();
		if (rdfData != null) {
			RDFXMLParser parser = new RDFXMLParser();
			parser.setRDFHandler(new RDFHandler() {
				public void handleStatement(Statement arg)
						throws RDFHandlerException {
					props.add(arg.getObject().toString());

				}

				public void endRDF() throws RDFHandlerException {
				}

				public void handleComment(String arg0)
						throws RDFHandlerException {
				}

				public void handleNamespace(String arg0, String arg1)
						throws RDFHandlerException {
				}

				public void startRDF() throws RDFHandlerException {
				}
			});
			parser.parse(new StringReader(rdfData), "");
		}
		return props;
	}

	/**
	 * @param propID
	 * @param objID
	 * @return
	 * @throws Exception
	 * Get the subject of a statement 
	 */
	public List<String> readSubject(String propID, String objID)
			throws Exception {

		String rdfData = getRDFData(null, propID, objID);
		final List<String> props = new LinkedList<String>();
		if (rdfData != null) {
			RDFXMLParser parser = new RDFXMLParser();
			parser.setRDFHandler(new RDFHandler() {
				public void handleStatement(Statement arg)
						throws RDFHandlerException {
					props.add(arg.getSubject().toString());
				}

				public void endRDF() throws RDFHandlerException {
				}

				public void handleComment(String arg0)
						throws RDFHandlerException {
				}

				public void handleNamespace(String arg0, String arg1)
						throws RDFHandlerException {
				}

				public void startRDF() throws RDFHandlerException {
				}
			});
			parser.parse(new StringReader(rdfData), "");
		}
		return props;
	}

	/**
	 * @param subj
	 * @param prop
	 * @param obj
	 * @return
	 * @throws Exception
	 * Get the value of a subject, property or object 
	 */
	public String getRDFData(String subj, String prop, String obj)
			throws Exception {
		String requestURL = STORAGE_SERVICE_ENDPOINT + "/repositories/"
				+ REPO_ID + "/statements";
		boolean paramAdded = false;
		if (subj != null) {
			requestURL += ((paramAdded) ? '&' : '?') + "subj="
					+ URLEncoder.encode(subj, "UTF-8");
			paramAdded = true;
		}
		if (prop != null) {
			requestURL += ((paramAdded) ? '&' : '?') + "pred="
					+ URLEncoder.encode(prop, "UTF-8");
			paramAdded = true;
		}
		if (obj != null) {
			requestURL += ((paramAdded) ? '&' : '?') + "obj="
					+ URLEncoder.encode(obj, "UTF-8");
		}

		requestURL += ((paramAdded) ? '&' : '?') + "context="
				+ URLEncoder.encode("defaultContext", "UTF-8");

		GetMethod getMtd = new GetMethod(requestURL);
		HttpClient httpclient = new HttpClient();
		String error = null;
		String rdfData = null;

		try {
			int result = httpclient.executeMethod(getMtd);
			if (result != 200) {
				if (getMtd.getResponseHeader("Error") != null) {
					error = getMtd.getResponseHeader("Error").getValue();
				}
			} else {
				rdfData = getMtd.getResponseBodyAsString();
			}
		} finally {
			getMtd.releaseConnection();
		}
		if (error != null) {
			throw new Exception(error);
		}
		return rdfData;
	}
	
	/**
	 * @param resource
	 * @param property
	 * @param value
	 * @return
	 * @throws RDFHandlerException
	 * Create string representation of statements
	 */
	public String createStatementToString(String resource,String property,String value) throws RDFHandlerException{
		
		 ValueFactory factory = new ValueFactoryImpl();
	        StringWriter buffer = new StringWriter();
	        RDFXMLWriter writer = new RDFXMLWriter(buffer);

	        writer.startRDF();
	        
            writer.handleStatement(
            		factory.createStatement(
            				factory.createURI(resource), 
            				factory.createURI(property), 
            				factory.createLiteral(value)));
	            
	        
	        writer.endRDF();
	        
	        return buffer.toString();
	}
	
	/**
	 * @throws Exception
	 * Print all the statements in the repository
	 */
	public void listRepositoryStatements() throws Exception {

		String requestURL = STORAGE_SERVICE_ENDPOINT + "/repositories/"
				+ REPO_ID + "/statements";

		System.out.println("Request URL: " + requestURL);
		GetMethod getMtd = new GetMethod(requestURL);
		HttpClient httpclient = new HttpClient();

		try {
			int result = httpclient.executeMethod(getMtd);
			System.out.println("Response status code: " + result);
			if (result != 200) {
				System.out.println("Error message: ");
				System.out.println(getMtd.getResponseHeader("Error"));
			} else {
				System.out.println("Response OK");
				System.out.println(getMtd.getResponseBodyAsString());
			}
		} finally {
			getMtd.releaseConnection();
		}
	}
}
