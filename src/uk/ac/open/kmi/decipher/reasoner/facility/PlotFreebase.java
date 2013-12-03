package uk.ac.open.kmi.decipher.reasoner.facility;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.methods.GetMethod;

import com.freebase.json.*;

import org.json.simple.parser.ParseException;
import org.ontoware.rdf2go.ModelFactory;
import org.ontoware.rdf2go.RDF2Go;
import org.ontoware.rdf2go.model.Model;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.rdf2go.RepositoryModel;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;
import org.openrdf.repository.http.HTTPRepository;

import uk.ac.open.kmi.decipher.SEC_Config;
import uk.ac.open.kmi.decipher.repository.RDFRepositoryConnector;
import uk.ac.open.kmi.decipher.repository.SPARQLClient;
import uk.ac.open.kmi.decipher.util.QueryResult;
import uk.ac.open.kmi.decipher.util.QueryRow;
import uk.ac.open.kmi.decipher.util.SPARQLQueryResultParser;
import uk.ac.open.kmi.decipher.util.SPARQLUtil;

public class PlotFreebase {

    public static final String FBAPI_URI = "http://www.freebase.com/api/service/mqlread?query=";
	private static final String ff_queryEndPoint ="http://factforge.net/sparql?query=";
	private static final String PREFIX = "PREFIX fb: <http://rdf.freebase.com/ns/> PREFIX dbpedia: <http://dbpedia.org/resource/> PREFIX dbp-ont: <http://dbpedia.org/ontology/>";

	/*
	public void getInfluencedPlot(){
		
		String qStr= "{\"query\":" 
				+ "{\"id\":null, \"name\":null,"// \"type\":[],"
				+ "\"/influence/influence_node/influenced\": " 
				+ "[{ \"id\": \"/en/martin_heidegger\","
				+ "\"name\": \"Martin Heidegger\","
				+ "\"type\":[]"  
				+ "}]"
				+ "}}";
		String result = mqlQuery(qStr);	
		RDFify(result);
	}
	
	public void RDFify(String jsonResult){
		try {
			JSON njson = JSON.parse(jsonResult);
			JSON names = njson.get ("name");
			System.out.println("String: " + names.toString());
		} catch (ParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
*/	
	private String askInfluencedTest() {
		String plotType = "http://rdf.freebase.com/ns/influence.influence_node.influenced";
		String askStr= SPARQLUtil.PREFIX 
				+ "ASK " 
				+ "WHERE {"
				+ " <http://rdf.freebase.com/ns/en.bill_cosby> owl:sameAs <http://rdf.freebase.com/ns/m.014zfs>." 
				+ "}";

		 SPARQLClient sClient = new SPARQLClient();
		 String results = sClient.httpQuery(ff_queryEndPoint, askStr, "application/sparql-results+xml");
//		 this.generateStatement(results, plotType);
     	 return results;		
	}		
	
	private String queryInfluencedTest() {
		String plotType = "http://rdf.freebase.com/ns/influence.influence_node.influenced";
		String queryStr= SPARQLUtil.PREFIX 
				+ "SELECT DISTINCT ?v ?b " 
				+ "FROM NAMED <http://www.ontotext.com/disable-SameAs> "		               
				+ "WHERE {"
				+ "    ?v <" + plotType + "> ?b." 
				+ "    FILTER regex(STR(?v), \"rdf.freebase.com/ns/\", \"i\")"
				+ "    FILTER regex(STR(?b), \"rdf.freebase.com/ns/\", \"i\")" 
                + "    FILTER (STR(?v) != STR(?b)) " 
                + "}";
//		        + "LIMIT 24";

		 SPARQLClient sClient = new SPARQLClient();
		 String results = sClient.httpQuery(ff_queryEndPoint, queryStr, "application/sparql-results+xml");
//		 this.generateStatement(results, plotType);
     	 return results;		
	}		
	private String queryInfluenced() {
		String plotType = "http://rdf.freebase.com/ns/influence.influence_node.influenced";
		String queryStr= SPARQLUtil.PREFIX 
				+ "SELECT DISTINCT ?v  ?b " 
				+ "WHERE {"
				+ "    ?v <" + plotType + "> ?b." 
				+ "    FILTER regex(STR(?v), \"rdf.freebase.com/ns/en\", \"i\")"
				+ "    FILTER regex(STR(?b), \"rdf.freebase.com/ns/en\", \"i\")" 
                + "    FILTER (STR(?v) != STR(?b)) " 
                + "}";
		 SPARQLClient sClient = new SPARQLClient();
		 String results = sClient.httpQuery(ff_queryEndPoint, queryStr, "application/sparql-results+xml");
		 this.generateStatement(results, plotType);
     	 return results;		
	}			
	private String queryInfluencedBy() {
		String plotType = "http://rdf.freebase.com/ns/influence.influence_node.influenced_by";
		String queryStr= PREFIX	
//				+ "SELECT DISTINCT (COUNT(?v) as ?vv) (COUNT(?b) as ?bb) " 
				+ "SELECT DISTINCT ?v ?b " 
				+ "WHERE {"
				+ "   ?v <" + plotType + "> ?b." 
                + "     FILTER regex(STR(?v), \"rdf.freebase.com/ns/en\", \"i\")"
				+ "     FILTER regex(STR(?b), \"rdf.freebase.com/ns/en\", \"i\")"
				+ "     FILTER (STR(?v) != STR(?b))"
				+ "}";

		 SPARQLClient sClient = new SPARQLClient();
		 String results = sClient.httpQuery(ff_queryEndPoint, queryStr, "application/sparql-results+xml");
		 this.generateStatement(results, plotType);
     	 return results;		
	}		
	
	private String queryPeers() {
		String plotType="http://rdf.freebase.com/ns/influence.peer_relationship.peers";
		String queryStr= PREFIX 
//				+ "SELECT DISTINCT (COUNT(?v) as ?vv) (COUNT(?b) as ?bb) " 
				+ "SELECT DISTINCT ?v ?b " 
				+ "WHERE {"
                + "   ?rn <"+ plotType + "> ?v." 
                + "   ?rn <"+ plotType + "> ?b." 
                + "     FILTER regex(STR(?v), \"rdf.freebase.com/ns/en\", \"i\")"
				+ "     FILTER regex(STR(?b), \"rdf.freebase.com/ns/en\", \"i\") "
				+ "     FILTER (STR(?v) != STR(?b))"
				+ "}";

		 SPARQLClient sClient = new SPARQLClient();
		 String results = sClient.httpQuery(ff_queryEndPoint, queryStr, "application/sparql-results+xml");
		 this.generateStatement(results, plotType);
     	 return results;		
	}		
	
	private void generateStatement(String sparqlResXML, String plotType){

//		SEC_Config config = SEC_Config.getConfig();
		RDFRepositoryConnector RDF_rep_conn;
		ValueFactory f = null;
		RepositoryConnection conn = null;
		try {
//			RDF_rep_conn = new RDFRepositoryConnector(config.getProperty("REPOSITORY_URL"), config.getProperty("REPOSITORY_NAME"));
//			RDF_rep_conn = new RDFRepositoryConnector("http://localhost:8080/openrdf-sesame/","Test");
			RDF_rep_conn = new RDFRepositoryConnector("http://decipher.open.ac.uk/openrdf-sesame/","Decipher");
			f = RDF_rep_conn.getRepository().getValueFactory();
			conn = RDF_rep_conn.getRepository().getConnection();			
		} catch (RepositoryException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}	

		QueryResult qr = SPARQLQueryResultParser.parse(sparqlResXML);	
		List<QueryRow> rows = qr.getQueryRows();
		Iterator<QueryRow> rows_iter = rows.iterator();
		while (rows_iter.hasNext()){
			QueryRow row = rows_iter.next();
			URI context = f.createURI("http://freebase.com/"+ plotType.substring(plotType.lastIndexOf(".") +1));
			URI sub = f.createURI(row.getValue("v"));
			URI pre = f.createURI(plotType);
			URI obj = f.createURI(row.getValue("b"));
			try {
				conn.add(sub, pre, obj, context);
			} catch (RepositoryException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		if (conn!=null)
			try {
				conn.close();
			} catch (RepositoryException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}		
	}
	
	
	public static void main(String[] args) throws Exception{
		PlotFreebase pfb= new PlotFreebase();
		pfb.queryInfluencedBy();
//		pfb.queryPeers();
//		pfb.queryInfluencedTest();
	}

}
