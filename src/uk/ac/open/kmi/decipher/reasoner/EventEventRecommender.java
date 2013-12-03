package uk.ac.open.kmi.decipher.reasoner;

import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.UUID;

import org.apache.commons.lang.StringEscapeUtils;

import uk.ac.open.kmi.decipher.repository.SPARQLClient;
import uk.ac.open.kmi.decipher.util.FileUtil;
import uk.ac.open.kmi.decipher.util.QueryResult;
import uk.ac.open.kmi.decipher.util.QueryRow;
import uk.ac.open.kmi.decipher.util.SPARQLQueryResultParser;

public class EventEventRecommender {
	
//	private static final String ff_queryEndPoint ="http://factforge.net/sparql?query=";
	private static final String ff_queryEndPoint ="http://factforge.net/sparql.xml?query=";
	private static final String LIMIT =" LIMIT 3";
	private static final int TOTAL_LIMIT =20; 
    private static final String TEMP_DIR = File.separator + "data" + File.separator + "Decipher" + File.separator +"tmp"; 
//    private static final String TEMP_DIR = File.separator + "Decipher" + File.separator +"tmp"; 

	String queryEndPoint;
	int eventCluNum;
    private String event;
	private String cacheFileName;
	
	int total_results_num=0;
	
	public EventEventRecommender(String repQueryURI, String eventID) {
		this.queryEndPoint =repQueryURI;
		this.event = eventID;
		this.cacheFileName = TEMP_DIR + File.separator + "event" + event.substring(event.lastIndexOf("/")+1) +"_recommendedEvents.xml";
	}

	public String returnCacheOrEmpty() {		
        File cacheFile = new File(cacheFileName);
        if(!cacheFile.exists())
        {
        	String respStr = "<?xml version=\"1.0\" encoding=\"UTF-8\"?> <events></events>";
        	return respStr;
        }
        else
        {
        	String respStr = FileUtil.readFile(cacheFileName);
        	return respStr;
        }
	}
	public String recommend() {
		
        File cacheFile = new File(cacheFileName);
        if(!cacheFile.exists())
        {
        	total_results_num=0; 
	     	Map<String, String> setting = new HashMap<String, String>();
    	 	Set<String> agentsSet = this.getAgents();
            String respStr = recommendEvents(agentsSet);            
        	FileUtil.write2File(respStr, cacheFileName);
        	return respStr;
        }
        else
        {
        	String respStr = FileUtil.readFile(cacheFileName);
        	return respStr;
        }
	}
	
	public String recommendEvents(Set<String> settings){
		
		String respStr="<?xml version=\"1.0\" encoding=\"UTF-8\"?> <events>";
        long cTime =System.currentTimeMillis();
        
        Iterator<String> setting_iter = settings.iterator();
 		while (setting_iter.hasNext() && ( total_results_num< TOTAL_LIMIT ))
  		{       
 			String agent_tmp = setting_iter.next();
            String agent ="";
 			if (agent_tmp.startsWith("http://freebase.com/m/"))
 			agent = "http://rdf.freebase.com/ns/m." + agent_tmp.substring(agent_tmp.lastIndexOf("/")+1);
 			if (agent_tmp.startsWith("http://freebase.com/en/"))
 			agent = "http://rdf.freebase.com/ns/en." + agent_tmp.substring(agent_tmp.lastIndexOf("/")+1);

 			System.out.println ("using"+ agent +" to recommend more events");
 			String result_student = queryStudent(agent);    	
 					respStr +=generateResponse(result_student,"educated",agent);
 			String result_creator =queryCreator(agent);
            		respStr +=generateResponse(result_creator,"created", agent);
            String result_awardee= queryAwardee(agent);
            		respStr +=generateResponse(result_awardee, "awarded", agent);
            String result_nominee= queryNominee(agent);
            		respStr +=generateResponse(result_nominee, "nominated", agent);
            String	result_author = queryAuthor(agent);
              		respStr +=generateResponse(result_author,"published", agent);  

            String	result_subject = queryExhibitionSubject(agent);
            		respStr +=generateResponse(result_subject, "exhibited subject",agent);
            String	result_producer= queryExhibitionProducer(agent);
            		respStr +=generateResponse(result_producer, "exhibited producer", agent);           		  
      }
        System.out.println( "the time cost to run the queries is: " + Long.toString(System.currentTimeMillis()-cTime));                    

    	respStr += "</events>";
    	FileUtil.write2File(respStr, cacheFileName);    	
    	return respStr;
	}
		
	public String generateResponse(String sparqlResult, String activity, String agent) {
		String str ="";
		Set<String> qTerms = new HashSet<String>();
		QueryResult qr = SPARQLQueryResultParser.parse(sparqlResult);	
		List<QueryRow> rows = qr.getQueryRows();
		Iterator<QueryRow> rows_iter = rows.iterator();
		while (rows_iter.hasNext()){
			QueryRow row = rows_iter.next();
			if (row.getValue("queryTerm") !=null) 
			{ 
				String qTerm = row.getValue("queryTerm");
				String qTerm_name = queryLabel(qTerm);	
				String qTerm_sh =qTerm_name.replaceAll("_", " ");
				String qTerm_id =qTerm_name.replaceAll("[^A-Za-z0-9]", "");
				
				String start_date = row.getValue("start_date");
				String end_date=null, location=null;
				if	(activity.equalsIgnoreCase("educated") || activity.equalsIgnoreCase("exhibited subject") || activity.equalsIgnoreCase("exhibited producer"))
				{	 
					end_date = row.getValue("end_date"); location = row.getValue("location");
				}
				
				String dob = row.getValue("dob"); String dob_val=dob!=null? dob:"";
				String dod = row.getValue("dod"); String dod_val=dod!=null? dod:"";
				if (!qTerms.contains(qTerm_id))
				{
					total_results_num +=1;			
					String agentLabel = queryLabel(agent);
					str += "<event>";
					str += "<id>" + UUID.randomUUID().toString() + "</id>";
					if (activity.equalsIgnoreCase("educated"))
					{	
						String locationLabel=null;
						if (location !=null) locationLabel = queryLabel(location);	
						str += "<title>"+ agentLabel + " was educated at "+ locationLabel +"</title>";
						str += "<location>";
						str += "<uri>" + location +"</uri>"; 
						str += "<label>" + locationLabel +"</label>"; 
						str += "</location>";

					}
					else if	(activity.equalsIgnoreCase("created"))
					{
						str += "<title>"+ agentLabel + " created &quot;"+ qTerm_sh +"&quot;</title>";
						str += "<tags>"; 
						str += "<tag><uri>"+ qTerm +"</uri><label>"+ qTerm_sh +"</label></tag>";
						str += "</tags>";
					}
					else if	(activity.equalsIgnoreCase("awarded"))
					{
						str += "<title>"+ agentLabel + " was awarded &quot;"+ qTerm_sh +"&quot;</title>";
						str += "<tags>"; 
						str += "<tag><uri>"+ qTerm +"</uri><label>"+ qTerm_sh +"</label></tag>";
						str += "<tag><uri>"+ agent +"</uri><label>"+ agentLabel +"</label></tag>";
						str += "</tags>";
					}
					else if	(activity.equalsIgnoreCase("nominated"))
					{
						str += "<title>"+ agentLabel + " was nominated for &quot;"+ qTerm_sh +"&quot;</title>";
						str += "<tags>"; 
						str += "<tag><uri>"+ qTerm +"</uri><label>"+ qTerm_sh +"</label></tag>";
						str += "<tag><uri>"+ agent +"</uri><label>"+ agentLabel +"</label></tag>";
						str += "</tags>";
					}
					else if	(activity.equalsIgnoreCase("published"))
					{
						str += "<title>"+ agentLabel + " published &quot;"+ qTerm_sh +"&quot;</title>";
						str += "<tags>"; 
						str += "<tag><uri>"+ qTerm +"</uri><label>"+ qTerm_sh +"</label></tag>";
						str += "</tags>";
					}
					else if	(activity.equalsIgnoreCase("exhibited subject"))
					{
						String locationLabel=null;
						if (location !=null) locationLabel = queryLabel(location);						
						str += "<title>"+ agentLabel + " was subject of exhibition at &quot;"+ qTerm_sh +"&quot;</title>";
						str += "<tags>"; 
						str += "<tag><uri>"+ qTerm +"</uri><label>"+ qTerm_sh +"</label></tag>";
						str += "<tag><uri>"+ agent +"</uri><label>"+ agentLabel +"</label></tag>";
						str += "</tags>";
						str += "<location>";
						str += "<uri>" + location +"</uri>"; 
						str += "<label>" + locationLabel +"</label>"; 
						str += "</location>";
					}
					else if	(activity.equalsIgnoreCase("exhibited producer"))
					{
						String locationLabel=null;
						if (location !=null) locationLabel=queryLabel(location);						
						str += "<title>"+ agentLabel + " produced exhibition at &quot;"+ qTerm_sh +"&quot;</title>";
						str += "<tags>"; 
						str += "<tag><uri>"+ qTerm +"</uri><label>"+ qTerm_sh +"</label></tag>";
						str += "</tags>";
						str += "<location>";
						str += "<uri>" + location +"</uri>"; 
						str += "<label>" + locationLabel +"</label>"; 
						str += "</location>";
					}
					str += "<agents><agent>";
					str += "<uri>" + agent +"</uri>"; 
					str += "<label>" + agentLabel +"</label>"; 
					str += "<DOB>" + dob_val +"</DOB>";
					str += "<DOD>" + dod_val +"</DOD>";
					str += "</agent></agents>";
					if (start_date!=null) str += "<start_date>"+ start_date +"</start_date>";
					if (end_date!=null)	str += "<end_date>"+ end_date +"</end_date>";					
					str += "<activity>" + activity +"</activity>";
					str += "</event>";

					qTerms.add(qTerm_id);
				}				
			}
		}
		return str;
	}
		
		
	private String queryRDF(String queryStr){		 
		 SPARQLClient sClient = new SPARQLClient();
		 String results = sClient.httpQuery(ff_queryEndPoint, queryStr, "application/sparql-results+xml");
		 return results;
	}
	private String queryLabel(String entity){
		
		String result=null;
		String queryStr= "PREFIX fb: <http://rdf.freebase.com/ns/> PREFIX skos: <http://www.w3.org/2004/02/skos/core#> "
				+ "SELECT DISTINCT ?entity_name ?entity_label ?rdfs_label WHERE {"
				+ "optional {<" + entity + "> fb:type.object.name ?entity_name. FILTER langMatches(lang(?entity_name), \"EN\" )}"	//to get the label of setting location 			
				+ "optional {{{<" + entity + "> skos:prefLabel ?entity_label. FILTER langMatches(lang(?entity_label), \"EN\" )} UNION {<" + entity + "> <http://factforge.net/preferredLabel> ?entity_label}}}"	//to get the label of setting location 				
				+ "optional {<" + entity + "> rdfs:label ?rdfs_label. FILTER langMatches(lang(?rdfs_label), \"EN\" )}"	//to get the label of setting location 				
				+ "} LIMIT 1";
//		System.out.println(queryStr);
		String sparqlResult = queryRDF(queryStr);
//		System.out.println(sparqlResult);
		QueryResult qr = SPARQLQueryResultParser.parse(sparqlResult);	
		List<QueryRow> rows = qr.getQueryRows();
		Iterator<QueryRow> rows_iter = rows.iterator();
		while (rows_iter.hasNext()){
			QueryRow row = rows_iter.next();
            result = row.getValue("entity_name");
            if (result == null)
            {  
            	String temp = row.getValue("entity_label");
            	if (temp !=null)
            	{
            		if (temp.contains("(")) result = temp.substring(0,temp.indexOf("(")); //e.g. Dublin (Ireland)
            		else result = temp;
            	}
            	else
            	{
            		temp = row.getValue("rdfs_label");
            		if (temp !=null)
            		{
            			if (temp.contains("(")) result = temp.substring(0,temp.indexOf("(")); //e.g. Dublin (Ireland)
            			else result = temp;
            		}
            	}
            }          
		}		
		return result;
	}
	public String queryStudent(String agent){
			
		String queryStr= "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "SELECT DISTINCT ?queryTerm ?location ?start_date ?end_date ?dob ?dod WHERE {"
				+ "<" + agent + "> fb:people.person.education ?queryTerm ."
				+ "	?queryTerm fb:education.education.institution ?location."
				+ "	OPTIONAL {<" + agent + "> fb:people.person.date_of_birth ?dob.}"
				+ "	OPTIONAL {<" + agent + "> fb:people.deceased_person.date_of_death ?dod.}"
				+ "	OPTIONAL {?queryTerm fb:education.education.start_date ?start_date.}"
				+ "	OPTIONAL {?queryTerm fb:education.education.end_date ?end_date.}"
				+ "}"
				+ LIMIT;
//		System.out.println(queryStr);
		String result = queryRDF(queryStr);
//		System.out.println(result);
		return result;
	}
	
	public String queryCreator(String agent){
		
		String queryStr= "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "SELECT DISTINCT ?start_date ?end_date ?queryTerm ?dob ?dod WHERE {"
				+ "<" +agent +"> fb:visual_art.visual_artist.artworks ?queryTerm."
				+ " { {?queryTerm fb:visual_art.artwork.date_begun ?start_date ." 
				+ "    ?queryTerm  fb:visual_art.artwork.date_completed ?end_date } UNION " 
				+ "   {?queryTerm  fb:visual_art.artwork.date_completed ?start_date}} ."
//				+ "  ?queryTerm fb:type.object.name ?queryTerm_name ."
//				+ "  FILTER(langMatches(lang(?queryTerm_name), 'EN'))."	
				+ "	 OPTIONAL {<" + agent + "> fb:people.person.date_of_birth ?dob.}"
				+ "	 OPTIONAL {<" + agent + "> fb:people.deceased_person.date_of_death ?dod.}"
				+ "}"
				+ LIMIT;
//		System.out.println(queryStr);
		String result = queryRDF(queryStr);
//		System.out.println(result);
		return result;
	}

	public String queryAwardee(String agent){

		String queryStr= "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "SELECT DISTINCT ?queryTerm ?start_date ?dob ?dod WHERE {"
				+ "<"+ agent +"> fb:award.award_winner.awards_won ?win ."
				+ "	?win fb:award.award_honor.year ?start_date ."
				+ "	?win fb:award.award_honor.award ?queryTerm ."
				+ "	 OPTIONAL {<" + agent + "> fb:people.person.date_of_birth ?dob.}"
				+ "	 OPTIONAL {<" + agent + "> fb:people.deceased_person.date_of_death ?dod.}"
				+ "}"
				+ LIMIT;
//		System.out.println(queryStr);
		String result = queryRDF(queryStr);
//		System.out.println(result);
		return result;
	}
	
	public String queryNominee(String agent){

		String queryStr= "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "SELECT DISTINCT ?queryTerm ?start_date ?dob ?dod WHERE {"
				+ "<"+ agent +"> fb:award.award_nominee.award_nominations ?nom ."
				+ "	?nom fb:award.award_nomination.year ?start_date ."
				+ "	?nom fb:award.award_nomination.award ?queryTerm ."
				+ "	 OPTIONAL {<" + agent + "> fb:people.person.date_of_birth ?dob.}"
				+ "	 OPTIONAL {<" + agent + "> fb:people.deceased_person.date_of_death ?dod.}"
				+ "}"
				+ LIMIT;
//		System.out.println(queryStr);
		String result = queryRDF(queryStr);
//		System.out.println(result);
		return result;
	}
	public String queryAuthor(String agent){

		String queryStr= "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "SELECT DISTINCT ?queryTerm ?start_date ?dob ?dod WHERE {"
				+ "<" +agent +"> fb:book.author.works_written ?queryTerm ."
				+ "  ?queryTerm fb:book.book.editions ?ed ."
				+ "  ?ed fb:book.book_edition.publication_date ?start_date ."
				+ "	 OPTIONAL {<" + agent + "> fb:people.person.date_of_birth ?dob.}"
				+ "	 OPTIONAL {<" + agent + "> fb:people.deceased_person.date_of_death ?dod.}"
				+ "}"
				+ LIMIT;
//		System.out.println(queryStr);
		String result = queryRDF(queryStr);
//		System.out.println(result);
		return result;
	}	

	
	public String queryExhibitionSubject(String agent){	
		String queryStr= "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>" 
				+ "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+" SELECT DISTINCT ?queryTerm ?start_date ?end_date ?location ?dob ?dod WHERE {"
				+"<" + agent +"> fb:exhibitions.exhibition_subject.exhibitions_created_about_this_subject ?queryTerm ."
				+"	    OPTIONAL {?queryTerm fb:exhibitions.exhibition.venues ?venue ."
				+"	    OPTIONAL {?venue fb:exhibitions.exhibition_run.opened_on ?start_date .}"
				+"	    OPTIONAL {?venue fb:exhibitions.exhibition_run.closed_on ?end_date .}"
				+"	    OPTIONAL {?venue fb:exhibitions.exhibition_run.venue ?location.} }"
				+ "	 OPTIONAL {<" + agent + "> fb:people.person.date_of_birth ?dob.}"
				+ "	 OPTIONAL {<" + agent + "> fb:people.deceased_person.date_of_death ?dod.}"
				+"}"
				+ LIMIT;		
//		System.out.println(queryStr);
		String result = queryRDF(queryStr);
//		System.out.println(result);
		return result;
	}

	public String queryExhibitionProducer(String agent){		
		String queryStr= "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>"
				+ "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+" SELECT DISTINCT ?queryTerm ?start_date ?end_date ?location ?dob ?dod WHERE {"
				+"<" + agent + "> fb:exhibitions.exhibition_producer.exhibitions_produced ?queryTerm ."
				+ "	OPTIONAL {?queryTerm fb:exhibitions.exhibition.venues ?venue." 
				+ " OPTIONAL {?venue fb:exhibitions.exhibition_run.opened_on ?start_date }."
				+ " OPTIONAL {?venue fb:exhibitions.exhibition_run.closed_on ?end_date } ."
				+ " OPTIONAL {?venue fb:exhibitions.exhibition_run.venue ?location.} }"
				+ "	OPTIONAL {<" + agent + "> fb:people.person.date_of_birth ?dob.}"
				+ "	OPTIONAL {<" + agent + "> fb:people.deceased_person.date_of_death ?dod.}"
				+ "}"
				+ LIMIT;
			
//		System.out.println(queryStr);
		String result = queryRDF(queryStr);
//		System.out.println(result);
		return result;
	}

	public Set<String> getAgents(){
		Set<String> result = new HashSet<String>();
		String queryStr= "PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>"
			 +" PREFIX curate:<http://decipher.open.ac.uk/curate/ontology/>"
			 +" SELECT DISTINCT ?agent WHERE {"
			 + "<" + event + "> rdf:type curate:Event ."
			 + "{{<" + event + "> curate:hasAgent ?agent.} UNION"
			 + " {<" + event + "> rdfs:seeAlso ?agent.}}"
			 + "}";
// 	    System.out.println(queryStr);
 		SPARQLClient sClient = new SPARQLClient();
 		String sparqlResult = sClient.httpQuery(queryEndPoint, queryStr, "application/sparql-results+xml");
		QueryResult qr = SPARQLQueryResultParser.parse(sparqlResult);	
		List<QueryRow> rows = qr.getQueryRows();
		Iterator<QueryRow> rows_iter = rows.iterator();
		while (rows_iter.hasNext()){
			QueryRow row = rows_iter.next();			
			
			String agent = row.getValue("agent");
			result.add(agent);
		}
		return result;
	}
	public static void main(String[] args) throws Exception{
		String event = "http://decipher-research.eu/events/1579";//1867";
//		String queryURI = "http://localhost:8088/openrdf-sesame/repositories/RecommenderDemo?query=";
//		String queryURI = "http://storyscope5.ssl.co.uk:8080/openrdf-sesame/repositories/test?query=";
		String queryURI = "http://decipher.open.ac.uk/openrdf-sesame/repositories/Decipher?query=";

//		String plot = "http://mckm219286.open.ac.uk/decipher/trunk/decipher-3/plot/4413";
		EventEventRecommender recommend= new EventEventRecommender(queryURI, event);
//		EventCluster eCluster= new EventCluster(queryURI, dossier);
		recommend.recommend();
//		EventRecommender recommend= new EventRecommender(dossier,plot);
//		PlotEventCluster eCluster= new PlotEventCluster(queryURI, dossier,plot);

//		Map<String, Set<String>> setting = eCluster.clusterEvents(3);
//		recommend.recommendEvents(setting);
//		recommend.queryArtworksOfTimeAndPlace("1934", "London");	
	}

}
