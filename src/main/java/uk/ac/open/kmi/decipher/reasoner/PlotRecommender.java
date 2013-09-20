package uk.ac.open.kmi.decipher.reasoner;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import uk.ac.open.kmi.decipher.repository.SPARQLClient;
import uk.ac.open.kmi.decipher.util.FileUtil;
import uk.ac.open.kmi.decipher.util.QueryResult;
import uk.ac.open.kmi.decipher.util.QueryRow;
import uk.ac.open.kmi.decipher.util.SPARQLQueryResultParser;
import uk.ac.open.kmi.decipher.util.SPARQLUtil;

public class PlotRecommender {

	private static final String PLOT_ROOT_ELEMENT = "story_section";//"plot_element";

	private String queryEndPoint;
    private String dossierID;
    private String cacheFileName;
//	public static final String ff_queryEndPoint ="http://factforge.net/sparql?query=";
 
	public PlotRecommender(String repQueryURI, String dossID) {
		super();
		this.queryEndPoint = repQueryURI;
		this.dossierID = dossID;
		this.cacheFileName = File.separator + "Decipher" + File.separator +"tmp" + File.separator + dossierID.substring(dossierID.lastIndexOf("/")+1) +"_recommendedBinaryPlots.xml";
	}	
	
	public String returnCacheOrEmpty(){
		File cacheFile = new File(cacheFileName);
		if(!cacheFile.exists())
		{
			String respStr ="<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?> <"+ PLOT_ROOT_ELEMENT+"s>" + "</"+ PLOT_ROOT_ELEMENT+"s>";
			return respStr;
			
		} else
		{
			String respStr = FileUtil.readFile(cacheFileName);
			return respStr;
		}
	}
	public String recommend(){
		File cacheFile = new File(cacheFileName);
		if(!cacheFile.exists())
		{
			String respStr =recommendPlots();
	    	FileUtil.write2File(respStr, cacheFileName);
			return respStr;
			
		} else
		{
			String respStr = FileUtil.readFile(cacheFileName);
			return respStr;
		}
	}
	
	private String getEventAgents (){		
	 String queryEventProperities = SPARQLUtil.PREFIX 
				+ "SELECT  ?agent "
	 		    + "WHERE {?event dul:satisfies <" + dossierID +">. "
				+ "?event curate:hasAgent ?agent. "
				+ "}";	 
	 SPARQLClient sClient = new SPARQLClient();
	 String results = sClient.httpQuery(queryEndPoint, queryEventProperities, "application/sparql-results+xml");
	 return results;
	}
	
	private Set<String> parseSparqlXML(String sparqlXML) {
		Set<String> agents = new HashSet<String>();
		QueryResult qr = SPARQLQueryResultParser.parse(sparqlXML);	
		List<QueryRow> rows = qr.getQueryRows();
		Iterator<QueryRow> rows_iter = rows.iterator();
		while (rows_iter.hasNext()){
			QueryRow row = rows_iter.next();
			String agent_o = row.getValue("agent");
			if (!agent_o.contains("freebase.com"))
			{  String agent_m = agent_o;
			   if (agent_o.contains("("))
				 agent_m = agent_m.substring(agent_o.indexOf("(")+1, agent_o.indexOf(")")-1);
				String agent_n =agent_m.replaceAll("Sir ", "").replaceAll("Dr ", "").replaceAll(",", "").replaceAll(" ","_").toLowerCase(); 
				String[] agent_names = agent_n.split("_"); //new version has last name and first name swapped.
				if (agent_names.length ==2)
				{
					String agent ="http://rdf.freebase.com/ns/en.".concat(agent_names[1] +"_"+ agent_names[0]);				
					System.out.println(agent);
					agents.add(agent);
				}
			}
		}
		System.out.println(agents.size());
		return agents;
	}
	
	public String recommendPlots(){
		String binPlots = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?> <"+ PLOT_ROOT_ELEMENT+"s>";
		binPlots += recommendAction();
		binPlots +="</" +PLOT_ROOT_ELEMENT+"s>";
		System.out.println(binPlots);
    	FileUtil.write2File(binPlots, cacheFileName);
		return binPlots;	
	}
	
	public String recommendAction(){

		String plots = "";
		SPARQLClient sClient = new SPARQLClient();
		String sparqlXML = getEventAgents();
		Set<String> agents = parseSparqlXML(sparqlXML);		
		Iterator<String> agents_ite = agents.iterator();
		while (agents_ite.hasNext()){
			String agent = agents_ite.next();
			System.out.println(agent);
			//?v1 influenced ?v2
			String recommendQuery = SPARQLUtil.PREFIX +
					"SELECT DISTINCT  ?v1 ?v2 "
					+ "WHERE {" 
/*							+ "       ?te1 rdf:type curate:EventSituation."
							+ "       ?te2 rdf:type curate:EventSituation." 
							+ "<"+  dossID + "> dul:isSatisfiedBy ?te1, ?te2."
							+ "       ?te1 curate:hasAgent ?v1."
							+ "       ?te2 curate:hasAgent ?v2." */
					+ "       ?v1 <http://rdf.freebase.com/ns/influence.influence_node.influenced> ?v2." 	
					//		+ "       FILTER (?te1 != ?te2) "
					+ "  FILTER regex(STR(?v1), \"^" + agent +"$\", \"i\")" 
					+ "}";
			String results = sClient.httpQuery(queryEndPoint, recommendQuery, "application/sparql-results+xml");

			QueryResult qr = SPARQLQueryResultParser.parse(results);	
			List<QueryRow> rows = qr.getQueryRows();
			if(rows.size() !=0)
			{Iterator<QueryRow> rows_iter = rows.iterator();
			while (rows_iter.hasNext()){
				QueryRow row = rows_iter.next();	
				plots += "<"+PLOT_ROOT_ELEMENT+">";
				plots +="<type>influenced</type>";
				plots +="<title>&lt;"+ row.getValue("v1") + "&gt; INFLUENCED &lt;" + row.getValue("v2") + "&gt;</title>"; 
				plots +="<source>list of events</source>";
				plots +="<consequence>list of events</consequence>";
				plots += "</"+PLOT_ROOT_ELEMENT+">";
			}
			}
			//?v1 influenced by ?v2
			recommendQuery = SPARQLUtil.PREFIX 
					+ "SELECT DISTINCT  ?v1 ?v2 "
					+ "WHERE {" 
					+ "  ?v1 <http://rdf.freebase.com/ns/influence.influence_node.influenced_by> ?v2." 	
					+ "  FILTER regex(STR(?v1), \"^" + agent +"$\", \"i\")" 
					+ "}";
			results = sClient.httpQuery(queryEndPoint, recommendQuery, "application/sparql-results+xml");			
			qr = SPARQLQueryResultParser.parse(results);	
			rows = qr.getQueryRows();
			if(rows.size() !=0)
			{Iterator<QueryRow> rows_iter = rows.iterator();
			while (rows_iter.hasNext()){
				QueryRow row = rows_iter.next();				
				plots += "<"+PLOT_ROOT_ELEMENT+">";
				plots +="<type>influenced</type>";
				plots +="<title>&lt;"+ row.getValue("v1") + "&gt; IS INFLUENCED BY &lt;" + row.getValue("v2") + "&gt;</title>"; 
				plots +="<source>list of events</source>";
				plots +="<consequence>list of events</consequence>";
				plots += "</"+PLOT_ROOT_ELEMENT+">";
			}
			}
			//?v1 peers ?v2
			recommendQuery = SPARQLUtil.PREFIX 
					+ "SELECT DISTINCT  ?v1 ?v2 "
					+ "WHERE {" 
					+ "       ?v1 <http://rdf.freebase.com/ns/influence.peer_relationship.peers> ?v2." 	
					+ "  FILTER regex(STR(?v1), \"^" + agent +"$\", \"i\")" 
					+ "}";
			results = sClient.httpQuery(queryEndPoint, recommendQuery, "application/sparql-results+xml");			
			qr = SPARQLQueryResultParser.parse(results);	
			rows = qr.getQueryRows();
			if(rows.size() !=0)
			{Iterator<QueryRow> rows_iter = rows.iterator();
			while (rows_iter.hasNext()){
				QueryRow row = rows_iter.next();
				plots += "<"+PLOT_ROOT_ELEMENT+">";
				plots +="<type>influenced</type>";
				plots +="<title>&lt;"+ row.getValue("v1") + "&gt; IS PEERS OF &lt;" + row.getValue("v2") + "&gt;</title>"; 
				plots +="<source>list of events</source>";
				plots +="<consequence>list of events</consequence>";
				plots += "</"+PLOT_ROOT_ELEMENT+">";
			}
			}
		}
		
		return plots;

	//produce a new triple ?v1 influenced ?v2	

/*		
		// ?a ->? -> ?b influenced/influenced_by		
		String recommendQuery2 = SPARQLUtil.PREFIX 
				+ "SELECT DISTINCT ?te1 ?te2 ?v1 ?v2 ?b"
				+ "where {?te1 rdf:type curate:EventSituation."
				+ "       ?te2 rdf:type curate:EventSituation." 
				+        dossID + "dul:isSatisfiedBy ?te1."
				+        dossID + "dul:isSatisfiedBy ?te2."
				+ "       ?v1 <http://rdf.freebase.com/ns/influence.influence_node.influenced> ?b." 
				+ "       ?v2 <http://rdf.freebase.com/ns/influence.influence_node.influenced_by> ?b." 

				+ "       FILTER(?te1 != ?te2)."
				+ "       ?te1 curate:storyFacet ?v1."
				+ "       ?te2 curate:storyFacet ?v2."
				+ "       FILTER(?v1 != ?v2)}";
				
			//produce two new triple ?v1 influenced ?b. ?v2 influcedby ?b
		
		// ?a ->? -> ?b influenced/influenced		
		String recommendQuery3 = SPARQLUtil.PREFIX 
				+ "SELECT DISTINCT ?te1 ?te2 ?v1 ?v2 ?b"
				+ "where {?te1 rdf:type curate:EventSituation."
				+ "       ?te2 rdf:type curate:EventSituation." 
				+        dossID + "dul:isSatisfiedBy ?te1."
				+        dossID + "dul:isSatisfiedBy ?te2."
				+ "       ?v1 <http://rdf.freebase.com/ns/influence.influence_node.influenced> ?b." 
				+ "       ?v2 <http://rdf.freebase.com/ns/influence.influence_node.influenced> ?b." 
				+ "       FILTER(?te1 != ?te2)."
				+ "       ?te1 curate:storyFacet ?v1."
				+ "       ?te2 curate:storyFacet ?v2."
				+ "       FILTER(?v1 != ?v2)}";
				
			//produce two new triple ?v1 influenced ?b. ?v2 influced ?b

		// ?a ->? -> ?b influenced_by/influenced_by
		 String recommendQuery4 = SPARQLUtil.PREFIX 
				+ "SELECT DISTINCT ?te1 ?te2 ?v1 ?v2 ?b"
				+ "where {?te1 rdf:type curate:EventSituation."
				+ "       ?te2 rdf:type curate:EventSituation." 
				+        dossID + "dul:isSatisfiedBy ?te1."
				+        dossID + "dul:isSatisfiedBy ?te2."
				+ "       ?v1 <http://rdf.freebase.com/ns/influence.influence_node.influenced_by> ?b." 
				+ "       ?v2 <http://rdf.freebase.com/ns/influence.influence_node.influenced_by> ?b." 
				+ "       FILTER(?te1 != ?te2)."
				+ "       ?te1 curate:storyFacet ?v1."
				+ "       ?te2 curate:storyFacet ?v2."
				+ "       FILTER(?v1 != ?v2)}";
			//produce two new triple ?v1 influenced by ?b. ?v2 influced by ?b
				
		 // ?a ->? -> ?b influenced_by/influenced
	
			// ?a ->? -> ?b influenced_by/influenced_by
		 String recommendQuery5 = SPARQLUtil.PREFIX 
				+ "SELECT DISTINCT ?te1 ?te2 ?v1 ?v2 ?b"
				+ "where {?te1 rdf:type curate:EventSituation."
				+ "       ?te2 rdf:type curate:EventSituation." 
				+        dossID + "dul:isSatisfiedBy ?te1."
				+        dossID + "dul:isSatisfiedBy ?te2."
				+ "       ?v1 <http://rdf.freebase.com/ns/influence.influence_node.influenced_by> ?b." 
				+ "       ?v2 <http://rdf.freebase.com/ns/influence.influence_node.influenced> ?b." 
				+ "       FILTER(?te1 != ?te2)."
				+ "       ?te1 curate:storyFacet ?v1."
				+ "       ?te2 curate:storyFacet ?v2."
				+ "       FILTER(?v1 != ?v2)}";
			//produce two new triple ?v1 influenced by ?b. ?v2 influced  ?b

	 // ?a ->? -> ?b where ? is an event in common
		 String recommendQuery6 = SPARQLUtil.PREFIX 
					+ "SELECT DISTINCT ?te1 ?te2 ?v1 ?v2 ?b ?name ?id"
					+ "where {?te1 rdf:type curate:EventSituation."
					+ "       ?te2 rdf:type curate:EventSituation." 
					+        dossID + "dul:isSatisfiedBy ?te1."
					+        dossID + "dul:isSatisfiedBy ?te2."
					+ "       FILTER(?te1 != ?te2)."
					+ "       ?te1 curate:storyFacet ?v1."
					+ "       ?te2 curate:storyFacet ?v2."
					+ "       FILTER(?v1 != ?v2)" 
					+ "?v1 curate:has_related_freebase_event_article ?b."
					+ "?v2 curate:has_related_freebase_event_article ?b."
					+ "?v1 rdf:type crm:E39.Actor."
					+ "?v2 rdf:type crm:E39.Actor."
					+ "	 ?b curate:has_related_freebase_event_name ?name."
					+ "	 ?b curate:has_related_freebase_event_id ?id."
                    + "}";
				//produce two new triple ?v1 and ?v2 shares the same event ?name 
	*/	 
	}
	
	

	public static void main(String[] args) throws Exception{
		String dossID = "http://decipher-research.eu/dossiers/1305";
		String queryURI = "http://localhost:8080/openrdf-sesame/repositories/Decipher2?query=";
		PlotRecommender pr= new PlotRecommender(queryURI,dossID);
//		String sparqlXML = pr.getEventAgents(dossID);
//		Set<String> agents = pr.parseSparqlXML(sparqlXML);
		pr.recommendPlots();
	}


}
