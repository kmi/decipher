package uk.ac.open.kmi.decipher.repository;

import java.io.IOException;
import java.io.InputStream;
import java.net.URLEncoder;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.methods.GetMethod;
import org.ontoware.aifbcommons.collection.ClosableIterator;
import org.ontoware.rdf2go.model.QueryResultTable;
import org.ontoware.rdf2go.model.QueryRow;
import org.openrdf.query.MalformedQueryException;
import org.openrdf.query.QueryEvaluationException;
import org.openrdf.query.TupleQueryResultHandlerException;
import org.openrdf.rdf2go.RepositoryModel;
import org.openrdf.repository.RepositoryException;
import org.openrdf.rio.RDFHandlerException;

import uk.ac.open.kmi.decipher.util.FileUtil;

public class SPARQLClient {


	public String httpQuery(String queryEndPoint, String queryString, String format){
	 HttpClient client = new HttpClient(); 
//	client.getHostConfiguration().setProxy("wwwcache.open.ac.uk", 80);
	GetMethod queryServicesMethod = null;
	String queryResult="";
	try {
		queryServicesMethod = new GetMethod(queryEndPoint + URLEncoder.encode(queryString, "UTF-8"));
//		queryServicesMethod.setRequestHeader("Accept", "application/sparql-results+xml");
		queryServicesMethod.setRequestHeader("Accept", format);
		queryServicesMethod.setRequestHeader("Accept-Charset", "UTF-8");
 		client.executeMethod(queryServicesMethod);
 //  	    queryResult=queryServicesMethod.getResponseBodyAsString();	    	
 //  	    System.out.println(queryResult);
   	    InputStream queryStream=queryServicesMethod.getResponseBodyAsStream();
   	    queryResult = FileUtil.convertStreamToString(queryStream);

	} catch (HttpException httpe) {
		// TODO Auto-generated catch block
		httpe.printStackTrace();
	} catch (IOException ioe) {
		// TODO Auto-generated catch block
		ioe.printStackTrace();
	}/*finally{
		queryServicesMethod.releaseConnection();
	}*/
		return queryResult;
   }
	
	public String SesameQuery(String queryString){	 
		
		RDFRepositoryConnector RDF_rep_conn;
		try {
			RDF_rep_conn = new RDFRepositoryConnector("http://localhost:8080/openrdf-sesame/","Decipher");
			String result = RDF_rep_conn.query(queryString);
			System.out.println(result);
			return result;
		} catch (RepositoryException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (MalformedQueryException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (QueryEvaluationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (TupleQueryResultHandlerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (RDFHandlerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
        return null;
	}	
	
	public void RDF2GoQuery(String queryString){	 
		try {
			RDFRepositoryConnector RDF_rep_conn = new RDFRepositoryConnector("http://localhost:8080/openrdf-sesame/","Decipher");
			RepositoryModel model = RDF_rep_conn.openRepositoryModel();
			QueryResultTable results = model.sparqlSelect(queryString);
			ClosableIterator<QueryRow> iter = results.iterator();
			while (iter.hasNext()) {
				QueryRow row = iter.next();
				String event = row.getValue("event").toString();
				System.out.println("event is: " + event);
				String sTime = row.getValue("sTime").toString();
				System.out.println("starting time is: " + sTime);
				String eTime = row.getValue("sTime").toString();
				System.out.println("ending time is: " + eTime);
				String theme = row.getValue("theme").toString();
				System.out.println("theme is: " + theme);

			}
		} catch (RepositoryException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
