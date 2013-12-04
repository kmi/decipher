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
import java.util.UUID;

import org.apache.commons.lang.StringEscapeUtils;

import uk.ac.open.kmi.decipher.repository.SPARQLClient;
import uk.ac.open.kmi.decipher.util.FileUtil;
import uk.ac.open.kmi.decipher.util.QueryResult;
import uk.ac.open.kmi.decipher.util.QueryRow;
import uk.ac.open.kmi.decipher.util.SPARQLQueryResultParser;

public class EventRecommender_v1 {
	
	private static final String ff_queryEndPoint ="http://factforge.net/sparql?query=";
	private static final String LIMIT =" LIMIT 3";
    private static final String TEMP_DIR = File.separator + "Decipher" + File.separator +"tmp"; 
	
	String queryEndPoint;
	int eventCluNum;
    private String dossier;
	private String plot;
	private String cacheFileName;
	
	String uriLookUpStr ="<uri-lookups>";
	
	public EventRecommender_v1(String dossID) {
		this.dossier = dossID;
		this.cacheFileName = TEMP_DIR + File.separator + dossier.substring(dossier.lastIndexOf("/")+1) +"_recommendedEvents.xml";
	}
	public EventRecommender_v1(String repQueryURI, String dossID, int clusterNum) {
		this.queryEndPoint =repQueryURI;
		this.dossier = dossID;
		this.eventCluNum =clusterNum;
		this.cacheFileName = TEMP_DIR + File.separator + dossier.substring(dossier.lastIndexOf("/")+1) +"_recommendedEvents.xml";
	}
	public EventRecommender_v1(String repQueryURI, String dossID, String plotID, int clusterNum) {
		this.dossier = dossID;
		this.plot = plotID;
		this.eventCluNum =clusterNum;
		this.cacheFileName = TEMP_DIR + File.separator + dossier.substring(dossier.lastIndexOf("/")+1) + "-"+ plot.substring(plot.lastIndexOf("/")+1) +"_recommendedEvents.xml";
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
			EventCluster eveCluster = new EventCluster(queryEndPoint, dossier);
			Map<String, Set<String>> setting = eveCluster.clusterEvents(eventCluNum);
            String respStr = recommendEvents(setting);
	    	FileUtil.write2File(respStr, cacheFileName);
        	return respStr;
        }
        else
        {
        	String respStr = FileUtil.readFile(cacheFileName);
        	return respStr;
        }
	}
	
	public String recommendEvents(Map<String, Set<String>> setting){
		
		String respStr="<?xml version=\"1.0\" encoding=\"UTF-8\"?> <events>";
    	List<String> times = new ArrayList<String>(setting.get("time"));
    	List<String> locations = new ArrayList<String>(setting.get("loc"));
    	System.out.println ("using");
    	for (String time:times)
    		System.out.println(" " + time + " ");
    	for (String loc:locations)    		
    		System.out.println(" " + loc + " ");    		
    	System.out.println ("to recommend more events");
	
    //	if (locations.contains("England") && locations.contains("London")) locations.remove("England");
    //	if (locations.contains("Florence") && locations.contains("Italy")) locations.remove("Italy");
    //	if (locations.contains("Bologna") && locations.contains("Italy")) locations.remove("Italy");
        if (!times.isEmpty() && !locations.isEmpty())
    	{
      //  	String result_artworks = queryArtworksOfTimeAndPlace(times,locations);    	
      //  	respStr +=generateAAWResponse(result_artworks,"creation");

        	String result_acquisitions =queryAcquisitionsOfTimeAndPlace(times,locations);
        	respStr +=generateAAWResponse(result_acquisitions,"acquisition");

        	String result_births= queryBirthOfTimeAndPlace(times,locations);
        	respStr +=generateBirthDeathResponse(result_births, "birth");

        	String result_deaths = queryDeathOfTimeAndPlace(times,locations);
        	respStr +=generateBirthDeathResponse(result_deaths, "death");

        	String result_writings = queryWritingsOfTimeAndPlace(times,locations);
        	respStr +=generateAAWResponse(result_writings,"publishing");
    	}
    	uriLookUpStr +="</uri-lookups>";
    	respStr += uriLookUpStr;
    	respStr += "</events>";
    	FileUtil.write2File(respStr, cacheFileName);    	
    	return respStr;
	}
	
	public String generateBirthDeathResponse(String sparqlResult, String activity) {
		String str ="";
		Set<String> persons = new HashSet<String>();
		QueryResult qr = SPARQLQueryResultParser.parse(sparqlResult);	
		List<QueryRow> rows = qr.getQueryRows();
		Iterator<QueryRow> rows_iter = rows.iterator();
		while (rows_iter.hasNext()){

			QueryRow row = rows_iter.next();
			if (row.getValue("person") !=null && row.getValue("pob") !=null && row.getValue("dob") !=null && row.getValue("name") !=null)			
			{  
				String person_sh=null, person_id=null;

				String person = row.getValue("person");
				String person_name = row.getValue("name");
				if (person.startsWith("http://")) 
				{
						if (person.contains("http://rdf.freebase.com/ns/m.")) 
						{
							person_sh =person_name.replaceAll("_", " ");
							person_id =person_name.replaceAll("[^A-Za-z0-9]", "");
						}
						else
						{	
							person_sh=person.substring(person.lastIndexOf("/")+1).replaceAll("_", " ").replaceAll("en.", "");
							person_id=person.substring(person.lastIndexOf("/")+1).replaceAll("en.", "").replaceAll("[^A-Za-z0-9]", "");										
						}
				}
				if (!persons.contains(person_id))  //to get rid of duplicate output
				{
					String dob = row.getValue("dob");
					String pob = row.getValue("pob");
					String dob_sh=dob, pob_sh=pob;
					if (dob.startsWith("http://")) dob_sh=dob.substring(dob.lastIndexOf("/")+1);
					if (pob.startsWith("http://")) pob_sh=pob.substring(pob.lastIndexOf("/")+1);

					String person_val=person_sh!=null? person_sh:person;
					String dob_val=dob_sh!=null? dob_sh:dob;
					String pob_val=pob_sh!=null? pob_sh:pob;

					String action=activity.equalsIgnoreCase("birth")? " was born in ":" died in ";

					str += "<event>";
					str += "<id>" + UUID.randomUUID().toString() + "</id>";
					str += "<title>"+ person_val + action + pob_val + " in " + dob_val + "</title>";
					str += "<agents><agent>"+ person +"</agent></agents>";
					str += "<start_time>"+ dob +"</start_time>";
					str += "<location>" + pob + "</location>";
					str += "<activity>" + activity +"</activity>";
					str += "</event>";

					add_uriLookup(person,person_sh,dob_sh);
					add_uriLookup(pob,pob_sh, null);

					persons.add(person_id);
				}
			}
		}
		return str;
	}
	
	public String generateAAWResponse(String sparqlResult, String activity) {
		String str ="";
		Set<String> objects = new HashSet<String>();
		QueryResult qr = SPARQLQueryResultParser.parse(sparqlResult);	
		List<QueryRow> rows = qr.getQueryRows();
		Iterator<QueryRow> rows_iter = rows.iterator();
		while (rows_iter.hasNext()){
			QueryRow row = rows_iter.next();
			if (row.getValue("person") !=null && row.getValue("object") !=null && row.getValue("date") !=null && row.getValue("name") !=null)
			{ 
				String person_sh=null, person_id=null;
				String object_sh=null, object_id=null;
				String date_sh=null;
				
				String person = row.getValue("person");
				String object = row.getValue("object");
				String object_name = row.getValue("name");				
				String date = row.getValue("date");
                System.err.print("object name is: " + object_name);

				if (person.startsWith("http://")) 
				{
					person_sh=person.substring(person.lastIndexOf("/")+1).replaceAll("_", " ");
					person_id=person.substring(person.lastIndexOf("/")+1).replaceAll("en.", "").replaceAll("[^A-Za-z0-9]", "");					
				}
				if (object.startsWith("http://")) 
				{
					if (object.contains("http://rdf.freebase.com/ns/m.")) 
					{
						object_sh =object_name.replaceAll("_", " ");
						object_id =object_name.replaceAll("[^A-Za-z0-9]", "");
					}
					else
					{	
						object_sh=object.substring(object.lastIndexOf("/")+1).replaceAll("_", " ").replaceAll("en.", "");
						object_id=object.substring(object.lastIndexOf("/")+1).replaceAll("en.", "").replaceAll("[^A-Za-z0-9]", "");										
					}
						
				}
				if (!objects.contains(object_id))
				{
					if (date.startsWith("http://")) date_sh=date.substring(date.lastIndexOf("/")+1);
					String person_val=person_sh!=null? person_sh:person;
					String object_val=object_sh!=null? object_sh:object;
					String date_val=date_sh!=null? date_sh:date;
                    System.err.print("object value is: " + object_val);
					str += "<event>";
					str += "<id>" + UUID.randomUUID().toString() + "</id>";
					if (activity.equalsIgnoreCase("acquisition"))
						str += "<title>&quot;"+ object_val + "&quot; was acquired by "+ person_val + " in " + date_val + "</title>";
					else if	(activity.equalsIgnoreCase("creation"))
						str += "<title>&quot;"+ object_val + "&quot; was created by "+ person_val + " in " + date_val + "</title>";
					else if	(activity.equalsIgnoreCase("publishing"))
						str += "<title>&quot;"+ object_val + "&quot; by "+ person_val + " was published in " + date_val + "</title>";
					str += "<agents><agent>"+ person +"</agent></agents>";
					str += "<start_time>"+ date +"</start_time>";
					str += "<object>" + object + "</object>";
					str += "<activity>" + activity +"</activity>";
					str += "</event>";
					//for looking up
					String dob=row.getValue("dob"), dob_sh=dob;
					if (dob.startsWith("http://")) dob_sh=dob.substring(dob.lastIndexOf("/")+1);
					add_uriLookup(person,person_sh, dob_sh);
					add_uriLookup(object,object_sh, null);

					objects.add(object_id);
				}				
			}
		}
		return str;
	}
	
	
	private void add_uriLookup(String uri, String label, String dob){
		if (uri.startsWith("http://") && !uriLookUpStr.contains(uri))
		{
			if (label!=null || dob != null)
			{   
				uriLookUpStr +="<uri-lookup>";			
				uriLookUpStr +="<uri>" + uri +"</uri>"; //&lt; &gt;
				if (label!=null) uriLookUpStr +="<label>" + label +"</label>"; //&quot;
				if (dob!=null) uriLookUpStr +="<DOB>" + dob +"</DOB>";
				uriLookUpStr +="</uri-lookup>";
			}
		}
	}
	
	private String queryRDF(String queryStr){		 
		 SPARQLClient sClient = new SPARQLClient();
		 String results = sClient.httpQuery(ff_queryEndPoint, queryStr, "application/sparql-results+xml");
		 return results;
	}
	
	public String queryBirthOfTimeAndPlace(List<String> times,List<String> places){
		String timesStr="", placesStr="";
		timesStr += " FILTER (";
		for (int i=0;i<times.size();i++)
		{
			timesStr += "regex(?dob, \"^" + times.get(i) +"\")";
			if(i!= times.size()-1)
				timesStr +=" || ";	
		}
		timesStr +=") ";		
		System.out.println(timesStr);
		if (!places.isEmpty())
		{
			placesStr += " FILTER (";
			for (int i=0;i<places.size();i++)
			{
				placesStr += "?pob = dbpedia:" + places.get(i);
				if(i!= places.size()-1)
					placesStr +=" || ";	
			}
			placesStr +=") ";
			placesStr +=" {";
			for (int i=0;i<places.size();i++)
			{   
				placesStr +="{?person dbp-ont:wikiPageWikiLink dbpedia:" + places.get(i) +"}";
				if(i!= places.size()-1)
					placesStr +=" UNION ";	
			}
			placesStr +="} ";
			System.out.println(placesStr);
		}
			
			
		String queryStr= "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "SELECT DISTINCT ?person ?dob ?pob ?name WHERE {"
				+ "    ?person fb:people.person.date_of_birth ?dob."
				+ "    ?person fb:people.person.profession dbpedia:Artist."
				+ "    ?person fb:type.object.name ?name."
				+  timesStr
				+ "    ?person fb:people.person.place_of_birth ?pob."
				+ placesStr 
				+ "}"
				+ LIMIT;
		System.out.println(queryStr);
		String result = queryRDF(queryStr);
		System.out.println(result);
		return result;
	}
/*	
	public String queryBirthOfTimeAndPlace(String time,String place){
		String queryStr= "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "SELECT DISTINCT ?person ?dob ?pob WHERE {"
				+ "    ?person fb:people.person.date_of_birth ?dob."
				+ "    ?person fb:people.person.profession dbpedia:Artist."
				+ "       FILTER (regex(?dob, \"^" + time +"\"))"
				+ "    ?person fb:people.person.place_of_birth ?pob."
				+ "       { {FILTER (?pob = dbpedia:" + place + ")} UNION" 
				+ "                 {?person dbp-ont:wikiPageWikiLink dbpedia:" + place +"} }" 
				+ "}";
		String result = queryRDF(queryStr);
		return result;
	}
*/
	public String queryDeathOfTimeAndPlace(List<String> times,List<String> places){
		String timesStr="", placesStr="";
		timesStr += " FILTER (";
		for (int i=0;i<times.size();i++)
		{
			timesStr += "regex(?dob, \"^" + times.get(i) +"\")";
			if(i!= times.size()-1)
				timesStr +=" || ";	
		}
		timesStr +=") ";
		if (!places.isEmpty())
		{
			placesStr += " FILTER (";
			for (int i=0;i<places.size();i++)
			{
				placesStr += "?pob = dbpedia:" + places.get(i);
				if(i!= places.size()-1)
					placesStr +=" || ";	
			}
			placesStr +=") ";
			placesStr +=" {";
			for (int i=0;i<places.size();i++)
			{   
				placesStr +="{?person dbp-ont:wikiPageWikiLink dbpedia:" + places.get(i) +"}";
				if(i!= places.size()-1)
					placesStr +=" UNION ";	
			}
			placesStr +="} ";
			
	/*		
			placesStr +=" { ";
			for (int i=0;i<places.size();i++)
			{   
				System.out.println(places.get(i));
				placesStr +=" {?x ?y ?pob FILTER (?pob = dbpedia:" + places.get(i) + ")} UNION {?person dbp-ont:wikiPageWikiLink dbpedia:" + places.get(i) +"}";
				if(i!= places.size()-1)
					placesStr +=" UNION ";	
			}
			placesStr +=" } ";*/
		}	
			
		String queryStr= "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "SELECT DISTINCT ?person ?dob ?pob ?name WHERE {"
				+ "    ?person fb:people.person.date_of_death ?dob."
				+ "    ?person fb:people.person.profession dbpedia:Artist."
				+ "    ?person fb:type.object.name ?name."
				+  timesStr
				+ "    ?person fb:people.person.place_of_death ?pob."
				+ placesStr 
				+ "}"
				+ LIMIT;
		System.out.println(queryStr);
		String result = queryRDF(queryStr);
		System.out.println(result);
		return result;
	}
/*		
	public String queryDeathOfTimeAndPlace(String time,String place){
		String queryStr= "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "SELECT DISTINCT ?person ?dob ?pob WHERE {"
				+ "   ?person fb:people.deceased_person.date_of_death ?dob."
				+ "   ?person fb:people.person.profession dbpedia:Artist."
				+ "      FILTER (regex(?dob, \"" + time + "\"))"
				+ "   ?person fb:people.deceased_person.place_of_death ?pob."
				+ "      { {FILTER (?pob = dbpedia:" + place + ")} UNION" 
				+ "                {?person dbp-ont:wikiPageWikiLink dbpedia:" + place +"}}" 
				+ "}";
		System.out.println(queryStr);
		String result = queryRDF(queryStr);
		System.out.println(result);
		return result;
	}
*/
	
	public String queryArtworksOfTimeAndPlace(List<String> times,List<String> places){
		
		String timesStr1="", timesStr2="", timesStr3="", placesStr="";
		for (int i=0;i<times.size();i++)
		 {
			timesStr1 += " FILTER (!bound(?begundec) || ?begundec <= "+ times.get(i) +").";			
		    timesStr2 += " FILTER (!bound(?completedec) || ?completedec >= " + times.get(i) +").";
		    timesStr3 += " FILTER (bound(?begundec) || (!bound(?begundec) && ?completedec = " + times.get(i) + ")).";
		    timesStr3 += " FILTER (bound(?completedec) || (!bound(?completedec) && ?begundec = " + times.get(i) + ")).";
		 }
		if (!places.isEmpty())
		{
			placesStr +=" { ";
			for (int i=0;i<places.size();i++)
			{   
				placesStr +=" {?person dbp-ont:wikiPageWikiLink dbpedia:"+ places.get(i) + "} UNION {?object dbp-ont:wikiPageWikiLink dbpedia:"+ places.get(i) + "}";
				if(i!= places.size()-1)
					placesStr +=" UNION ";	
			}
			placesStr +=" } ";
			System.out.println(placesStr);
		}
		
		String queryStr= "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>" 
				+ "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "SELECT DISTINCT ?person ?dob ?object ?date ?name WHERE {" 
				+ "   ?person fb:visual_art.visual_artist.artworks ?object."
				+ "	  ?person fb:people.person.date_of_birth ?dob."
				+ "   ?object fb:type.object.name ?name."
				+ placesStr
				+ "   OPTIONAL {?object fb:visual_art.artwork.date_begun ?date."
				+ "         BIND( SUBSTR( xsd:string(?date), 1, 4) AS ?begunsub )." 
				+ "         BIND (xsd:decimal(?begunsub) as ?begundec) ."
				+ timesStr1 + "}"
				+ "   OPTIONAL {?object fb:visual_art.artwork.date_completed ?complete."
				+ "         BIND( SUBSTR( xsd:string(?complete), 1, 4) AS ?completesub)." 
				+ "         BIND (xsd:decimal(?completesub) as ?completedec) ."
				+ timesStr2+ "}"
				+ "     FILTER (bound(?begundec) || bound(?completedec))."
				+ timesStr3
				+ "   ?object fb:type.object.name ?artwork_name."
				+ "     FILTER(langMatches(lang(?artwork_name), 'EN'))."
				+"}"
				+ LIMIT;
		System.out.println(queryStr);
		String result = queryRDF(queryStr);
		System.out.println(result);
		return result;
	}
/*
   public String queryArtworksOfTimeAndPlace(String time,String place){	   	   
	   String queryStr= "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>" 
				+ "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "SELECT DISTINCT ?artist ?artwork ?begun WHERE {" 
				+ "   ?artist fb:visual_art.visual_artist.artworks ?artwork."
				+ "   { {?artist dbp-ont:wikiPageWikiLink dbpedia:"+ place + "} UNION"
				+ "     {?artwork dbp-ont:wikiPageWikiLink dbpedia:"+ place + "}}."
				+ "   OPTIONAL {?artwork fb:visual_art.artwork.date_begun ?begun."
				+ "         BIND( SUBSTR( xsd:string(?begun), 1, 4) AS ?begunsub )." 
				+ "         BIND (xsd:decimal(?begunsub) as ?begundec). "
				+ "     FILTER ( ?begundec <= "+ time +").}"

//				+ "     FILTER (!bound(?begundec) || ?begundec <= "+ time +")."
				+ "   OPTIONAL {?artwork fb:visual_art.artwork.date_completed ?complete."
				+ "         BIND( SUBSTR( xsd:string(?complete), 1, 4) AS ?completesub)." 
				+ "         BIND (xsd:decimal(?completesub) as ?completedec) ."
				+ "     FILTER (?completedec >= " + time +").}"
//				+ "     FILTER (!bound(?completedec) || ?completedec >= " + time +")."
//				+ "     FILTER (bound(?begundec) || bound(?completedec))."
//				+ "     FILTER (bound(?begundec) || (!bound(?begundec) && ?completedec = " + time + "))."
//				+ "     FILTER (bound(?completedec) || (!bound(?completedec) && ?begundec = " + time + "))."
				+ "   ?artwork fb:type.object.name ?artwork_name."
				+ "     FILTER(langMatches(lang(?artwork_name), 'EN'))."
				+"}";

		System.out.println(queryStr);
		long start=System.currentTimeMillis();
		String result = queryRDF(queryStr);
		long end = System.currentTimeMillis();
		System.err.println("the time elapsed: " + (end-start));
		System.out.println(result);
		return result;
	}
*/
	public String queryAcquisitionsOfTimeAndPlace(List<String> times, List<String> places){
		
		String timesStr="", placesStr="";
		for (int i=0;i<times.size();i++)
			timesStr += " FILTER (regex(?date, \"^" + times.get(i) +"\"))";
		System.out.println(timesStr);
		if (!places.isEmpty())
		{
			placesStr +=" { ";
			for (int i=0;i<places.size();i++)
			{   
				placesStr +=" {?object dbp-ont:wikiPageWikiLink dbpedia:" + places.get(i) + "} UNION {?owner dbp-ont:wikiPageWikiLink dbpedia:" + places.get(i) +"}";
				if(i!= places.size()-1)
					placesStr +=" UNION ";	
			}
			placesStr +=" } ";
			System.out.println(placesStr);
		}
		String queryStr= "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>"
				+ "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "SELECT DISTINCT ?person ?dob ?object ?date ?name WHERE {"
				+ "?object fb:visual_art.artwork.owners ?relationship."
				+ "?person fb:visual_art.visual_artist.artworks ?object."
				+ "	  ?person fb:people.person.date_of_birth ?dob."
				+ "?object fb:type.object.name ?name."
				+ "     ?relationship fb:visual_art.artwork_owner_relationship.date_acquired  ?date."
				+ timesStr
				+ "      ?relationship  fb:visual_art.artwork_owner_relationship.owner ?owner."
                + placesStr
				+ " }"
				+ LIMIT;
		
		System.out.println(queryStr);
		String result = queryRDF(queryStr);
		System.out.println(result);
		return result;
	}
/*	
	public String queryAcquisitionsOfTimeAndPlace(String time,String place){
		String queryStr= "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>"
				+ "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "SELECT DISTINCT ?artwork ?artist ?acquired WHERE {"
				+ "?artwork fb:visual_art.artwork.owners ?relationship."
				+ "?artist fb:visual_art.visual_artist.artworks ?artwork."
				+ "?relationship fb:visual_art.artwork_owner_relationship.date_acquired  ?acquired."
				+ "      FILTER (regex(?acquired, \"^" + time + "\"))."
				+ "?relationship  fb:visual_art.artwork_owner_relationship.owner ?owner."
				+ "      {{?artwork dbp-ont:wikiPageWikiLink dbpedia:" + place +"} UNION"
				+ "       {?owner dbp-ont:wikiPageWikiLink dbpedia:"+ place +"}}}";

		System.out.println(queryStr);
		String result = queryRDF(queryStr);
		System.out.println(result);
		return result;
	}
*/	
	public String queryWritingsOfTimeAndPlace(List<String> times, List<String> places){
	
		String timesStr="", placesStr="";
		for (int i=0;i<times.size();i++)
			timesStr += " FILTER (?datedec = " + times.get(i) + ")";
		System.out.println(timesStr);
		if (!places.isEmpty())
		{
			placesStr +=" { ";
			for (int i=0;i<places.size();i++)
			{   
				placesStr +=" {?person dbp-ont:wikiPageWikiLink dbpedia:" + places.get(i) + "} UNION {?object dbp-ont:wikiPageWikiLink dbpedia:" + places.get(i) + "}";
				if(i!= places.size()-1)
					placesStr +=" UNION ";	
			}
			placesStr +=" } ";
			System.out.println(placesStr);
		}
		String queryStr= "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>"
				+ "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "SELECT DISTINCT ?person ?dob ?object ?date ?name WHERE {"
				+ "      ?person fb:book.author.works_written ?object."  
				+ "	     ?person fb:people.person.date_of_birth ?dob."
				+ "      ?object fb:type.object.name ?name."
				+ placesStr
				+ "      ?object fb:book.book.editions ?edition."
				+ "      ?edition fb:book.book_edition.publication_date ?date."  
				+ "        BIND( SUBSTR( xsd:string(?date), 1, 4) AS ?datesub )."
				+ "        BIND (xsd:decimal(?datesub) as ?datedec)."
				+ timesStr
				+ "      ?person dbp-ont:wikiPageWikiLink dbpedia:Art.}"
				+ LIMIT;
		System.out.println(queryStr);
		String result = queryRDF(queryStr);
		System.out.println(result);
	return result;
	}
/*	
	public String queryWritingsOfTimeAndPlace(String time,String place){
		String queryStr= "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>"
				+ "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "SELECT DISTINCT ?author ?work ?date WHERE {"
				+ "      ?author fb:book.author.works_written ?work."       
				+ "      {{?author dbp-ont:wikiPageWikiLink dbpedia:" + place + "} UNION"
				+ "       {?work dbp-ont:wikiPageWikiLink dbpedia:" + place + "}}"     
				+ "      ?work fb:book.book.editions ?edition."
				+ "      ?edition fb:book.book_edition.publication_date ?date."  
				+ "        BIND( SUBSTR( xsd:string(?date), 1, 4) AS ?datesub )."
				+ "        BIND (xsd:decimal(?datesub) as ?datedec)."
				+ "       FILTER (?datedec = " + time + ")."
				+ "      ?author  dbp-ont:wikiPageWikiLink dbpedia:Art.}";

		System.out.println(queryStr);
		String result = queryRDF(queryStr);
		System.out.println(result);
	return result;
	}
*/	
	public static void main(String[] args) throws Exception{
		String dossier = "http://decipher-research.eu/dossiers/1308";
		String queryURI = "http://localhost:8088/openrdf-sesame/repositories/RecommenderDemo?query=";
//		String plot = "http://mckm219286.open.ac.uk/decipher/trunk/decipher-3/plot/4413";
		EventRecommender_v1 recommend= new EventRecommender_v1(dossier);
		EventCluster eCluster= new EventCluster(queryURI, dossier);
//		EventRecommender recommend= new EventRecommender(dossier,plot);
//		PlotEventCluster eCluster= new PlotEventCluster(queryURI, dossier,plot);

		Map<String, Set<String>> setting = eCluster.clusterEvents(3);
		recommend.recommendEvents(setting);
//		recommend.queryArtworksOfTimeAndPlace("1934", "London");	
	}

}
