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

public class EventRecommender {
	
	private static final String ff_queryEndPoint ="http://factforge.net/sparql?query=";
	private static final String LIMIT =" LIMIT 3";
	private static final int TOTAL_LIMIT =20; 
	private static final int TOTAL_BD_LIMIT =10;
	private static final int TOTAL_AAW_LIMIT =10;

	static private final String CONNECTOR="::";
	private enum Activities { birthIn, deathIn, birthAssociated , deathAssociated; }
	
	String queryEndPoint;
	int eventCluNum;
    private String dossier;
	private String plot;
	private String cacheFileName;
	
	String uriLookUpStr ="<uri-lookups>";
	int total_results_num=0, total_BD_num=0, total_AAW_num=0;
	
	public EventRecommender(String dossID) {
		this.dossier = dossID;
		this.cacheFileName = File.separator + "Decipher" + File.separator +"tmp" + File.separator + dossier.substring(dossier.lastIndexOf("/")+1) +"_recommendedEvents.xml";
	}
	public EventRecommender(String repQueryURI, String dossID) {
		this.queryEndPoint =repQueryURI;
		this.dossier = dossID;
		this.cacheFileName = File.separator + "Decipher" + File.separator +"tmp" + File.separator + dossier.substring(dossier.lastIndexOf("/")+1) +"_recommendedEvents.xml";
	}
	public EventRecommender(String repQueryURI, String dossID, int clusterNum) {
		this.queryEndPoint =repQueryURI;
		this.dossier = dossID;
		this.eventCluNum =clusterNum;
		this.cacheFileName = File.separator + "Decipher" + File.separator +"tmp" + File.separator + dossier.substring(dossier.lastIndexOf("/")+1) +"_recommendedEvents.xml";
	}
	public EventRecommender(String repQueryURI, String dossID, String plotID, int clusterNum) {
		this.dossier = dossID;
		this.plot = plotID;
		this.eventCluNum =clusterNum;
		this.cacheFileName = File.separator + "Decipher" + File.separator +"tmp" + File.separator + dossier.substring(dossier.lastIndexOf("/")+1) + "-"+ plot.substring(plot.lastIndexOf("/")+1) +"_recommendedEvents.xml";
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
        	total_results_num=0; total_BD_num=0;total_AAW_num=0;
		/*
		 	EventCluster eveCluster = new EventCluster(queryEndPoint, dossier);
		 	Map<String, Set<String>> setting = eveCluster.clusterEvents(eventCluNum);
            String respStr = recommendEvents(setting);
	    */
        	Map<String, String> setting = new HashMap<String, String>();
        	EventSettingCalculator eveSetCalculator = new EventSettingCalculator(queryEndPoint, dossier);
		 	Set<String> settingsSet = eveSetCalculator.clusterSettings();
            String respStr = recommendEvents(settingsSet);
            
        	FileUtil.write2File(respStr, cacheFileName);
        	return respStr;
        }
        else
        {
        	String respStr = FileUtil.readFile(cacheFileName);
        	return respStr;
        }
	}
	
//	public String recommendEvents(Map<String, Set<String>> setting){
	public String recommendEvents(Set<String> settings){
		
		String respStr="<?xml version=\"1.0\" encoding=\"UTF-8\"?> <events>";
/*		Set<String> settings = new HashSet<String>(settingsSet); 
		if (settings.contains("1970" + CONNECTOR + "http://freebase.com/m/02_286"))
		{
			settings.remove("1970" + CONNECTOR + "http://freebase.com/m/02_286");
 			String set1 = "1970" + CONNECTOR + "http://freebase.com/m/02_286";
 			String[] set1s = set1.split(CONNECTOR);
 			String time = set1s[0];
 			String loc = set1s[1];
 			String location = "http://rdf.freebase.com/ns/m." + loc.substring(loc.lastIndexOf("/")+1);

			String result_artworks = queryArtworksOfTimeAndPlace(time,location);    	
				respStr +=generateAAWResponse(result_artworks,"creation", time, location);

 			String result_acquisitions =queryAcquisitionsOfTimeAndPlace(time,location);
        		respStr +=generateAAWResponse(result_acquisitions,"acquisition", time, location);

        	String result_births_in= queryBirthOfTimeAndPlace(time,location);
        		respStr +=generateBirthDeathResponse(result_births_in, "birthIn", time, location);

        	String result_births= queryAssociatedBirthOfTimeAndPlace(time,location);
        		respStr +=generateBirthDeathResponse(result_births, "birthAssociated", time, location);

        	String result_deaths_in = queryDeathOfTimeAndPlace(time,location);
        		respStr +=generateBirthDeathResponse(result_deaths_in, "deathIn", time, location);

        	String result_deaths= queryAssociatedDeathOfTimeAndPlace(time,location);
        		respStr +=generateBirthDeathResponse(result_deaths, "deathAssociated", time, location);

        	String result_writings = queryWritingsOfTimeAndPlace(time,location);
        		respStr +=generateAAWResponse(result_writings,"publishing", time, location);          		
		}
	*/	
        Iterator<String> setting_iter = settings.iterator();
 		while (setting_iter.hasNext() && ( total_results_num< TOTAL_LIMIT ))//||total_BD_num< TOTAL_BD_LIMIT ||total_AAW_num< TOTAL_AAW_LIMIT))
  		{       
 			String set = setting_iter.next();
 			System.out.println("in Recommender: " + set);
 			String[] sets = set.split(CONNECTOR);
 			String time = sets[0];
 			String loc = sets[1];
 			String location = "http://rdf.freebase.com/ns/m." + loc.substring(loc.lastIndexOf("/")+1);

//          String time="1999";
//          String location="http://rdf.freebase.com/en/m.07b_l";
//			String time="1921";
//			String location="http://rdf.freebase.com/ns/m.018hqt";
// 			String time="1970";//"1921";
// 			String location="http://rdf.freebase.com/ns/m.02_286";//"http://rdf.freebase.com/ns/m.018hqt";
 			System.out.println ("using"+ time + " and " + location +" to recommend more events");
 //			System.out.println ("total result number is: " + total_results_num);
 //			System.out.println ("total BD number is: " + total_BD_num);
 //			System.out.println ("total AAW number is: " + total_AAW_num);

 			String result_artworks = queryArtworksOfTimeAndPlace(time,location);    	
 				respStr +=generateAAWResponse(result_artworks,"creation", time, location);
 			String	result_acquisitions =queryAcquisitionsOfTimeAndPlace(time,location);
            //	if (total_AAW_num< TOTAL_AAW_LIMIT) 
            		respStr +=generateAAWResponse(result_acquisitions,"acquisition", time, location);

            String	result_births_in= queryBirthOfTimeAndPlace(time,location);
            //	if (total_BD_num< TOTAL_BD_LIMIT) 
            		respStr +=generateBirthDeathResponse(result_births_in, "birthIn", time, location);

            String result_births= queryAssociatedBirthOfTimeAndPlace(time,location);
            //	if (total_BD_num< TOTAL_BD_LIMIT) 
            		respStr +=generateBirthDeathResponse(result_births, "birthAssociated", time, location);

            String	result_deaths_in = queryDeathOfTimeAndPlace(time,location);
            //	if (total_BD_num< TOTAL_BD_LIMIT) 
            		respStr +=generateBirthDeathResponse(result_deaths_in, "deathIn", time, location);

            String	result_deaths= queryAssociatedDeathOfTimeAndPlace(time,location);
            //	if (total_AAW_num< TOTAL_AAW_LIMIT) 
            		respStr +=generateBirthDeathResponse(result_deaths, "deathAssociated", time, location);

            String	 result_writings = queryWritingsOfTimeAndPlace(time,location);
            //	if (total_AAW_num< TOTAL_AAW_LIMIT) 
            		respStr +=generateAAWResponse(result_writings,"publishing", time, location);  
           
      }

    //	uriLookUpStr +="</uri-lookups>";
    //	respStr += uriLookUpStr;
    	respStr += "</events>";
    	FileUtil.write2File(respStr, cacheFileName);    	
    	return respStr;
	}
	
	public String generateBirthDeathResponse(String sparqlResult, String activity, String time, String location) {
		String str ="";
		Set<String> persons = new HashSet<String>();
		QueryResult qr = SPARQLQueryResultParser.parse(sparqlResult);	
		List<QueryRow> rows = qr.getQueryRows();
		Iterator<QueryRow> rows_iter = rows.iterator();
		while (rows_iter.hasNext()){

			QueryRow row = rows_iter.next();
			if (row.getValue("person") !=null && row.getValue("pob") !=null && row.getValue("dob") !=null && row.getValue("name") !=null && row.getValue("pob_name") !=null && row.getValue("place_name") !=null)			
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
					
					total_results_num+=1; total_BD_num+=1;
					String dob = row.getValue("dob");
					String pob = row.getValue("pob");
					String pob_name = row.getValue("pob_name");
					String place_name = row.getValue("place_name");
					String dob_sh=dob, pob_name_sh=pob_name, place_name_sh=place_name;
					if (dob.startsWith("http://")) dob_sh=dob.substring(dob.lastIndexOf("/")+1);
					if (pob_name.startsWith("http://")) pob_name_sh=pob_name.substring(pob_name.lastIndexOf("/")+1);
					if (place_name.startsWith("http://")) place_name_sh=place_name.substring(place_name.lastIndexOf("/")+1);

					String person_val=person_sh!=null? person_sh:person;
					String dob_val=dob_sh!=null? dob_sh:dob;
					String pob_name_val=pob_name_sh!=null? pob_name_sh:pob_name;
					String place_name_val=place_name_sh!=null? place_name_sh:place_name;

					//String action=activity.equalsIgnoreCase("birth")? " was born in ":" died in ";
					Activities act = Activities.valueOf(activity);
					String action="", prep=""; 
                    switch (act)
                    {
                    case birthIn:
                    	action = " was born in ";
                    	prep = " in ";
                    	break;
                    case birthAssociated:
                    	action = " was born in ";
                    	prep = " and is associated with ";
                    	break;
                    case deathIn:
                    	action = " died in ";
                    	prep = " in ";
                    	break;
                    case deathAssociated:
                    	action = " died in ";
                       	prep = " and is associated with ";
                        break;
                    }
					
					str += "<event>";
					str += "<id>" + UUID.randomUUID().toString() + "</id>";
					str += "<title>"+ person_val + action + time + prep + place_name_val + "</title>";
					str += "<agents><agent>";
					str += "<uri>" + person +"</uri>"; 
					str += "<label>" + person_sh +"</label>"; 
					str += "<DOB>" + dob_sh +"</DOB>";
					str += "</agent></agents>";
					str += "<start_time>"+ dob +"</start_time>";
					str += "<location>";
					str += "<uri>" + pob +"</uri>"; 
					str += "<label>" + pob_name_sh +"</label>"; 
					str += "</location>";
					str += "<activity>" + activity.substring(0, 5) +"</activity>";
					str += "</event>";

				//	add_uriLookup(person,person_sh,dob_sh);
				//	add_uriLookup(pob,pob_name_sh, null);
				//	add_uriLookup(location,place_name_sh, null);

					persons.add(person_id);
				}
			}
		}
		return str;
	}
	
	public String generateAAWResponse(String sparqlResult, String activity, String time, String location) {
		String str ="";
		Set<String> objects = new HashSet<String>();
		QueryResult qr = SPARQLQueryResultParser.parse(sparqlResult);	
		List<QueryRow> rows = qr.getQueryRows();
		Iterator<QueryRow> rows_iter = rows.iterator();
		while (rows_iter.hasNext()){
			QueryRow row = rows_iter.next();
			if (row.getValue("person") !=null && row.getValue("object") !=null && row.getValue("date") !=null && row.getValue("name") !=null && row.getValue("place_name") !=null)
			{ 
				String person_sh=null, person_id=null;
				String object_sh=null, object_id=null;
				String date_sh=null;
				
				String person = row.getValue("person");
				String object = row.getValue("object");
				String object_name = row.getValue("name");				
				String date = row.getValue("date");
 //               System.err.print("object name is: " + object_name);

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
					total_results_num +=1; total_AAW_num+=1;	 			
					String place_name = row.getValue("place_name");
					String place_name_sh=place_name;
					if (place_name.startsWith("http://")) place_name_sh=place_name.substring(place_name.lastIndexOf("/")+1);
					if (date.startsWith("http://")) date_sh=date.substring(date.lastIndexOf("/")+1);
					String person_val=person_sh!=null? person_sh:person;
					String object_val=object_sh!=null? object_sh:object;
					String date_val=date_sh!=null? date_sh:date;
					String place_val=place_name_sh!=null?place_name_sh:place_name;

					String dob=row.getValue("dob"), dob_sh=dob;
					if (dob.startsWith("http://")) dob_sh=dob.substring(dob.lastIndexOf("/")+1);

//                    System.err.print("object value is: " + object_val);
					str += "<event>";
					str += "<id>" + UUID.randomUUID().toString() + "</id>";
					if (activity.equalsIgnoreCase("acquisition"))
						str += "<title>&quot;"+ object_val + "&quot; was acquired by "+ person_val + " in " + time + ". Acquisition or buyer are associated with &quot;" + place_val +"&quot;</title>";
					else if	(activity.equalsIgnoreCase("creation"))
						str += "<title>&quot;"+ object_val + "&quot; was created by "+ person_val + " in " + time + ".  Creator or work are associated with &quot;" + place_val + "&quot;</title>";
					else if	(activity.equalsIgnoreCase("publishing"))
						str += "<title>&quot;"+ object_val + "&quot; by "+ person_val + " was published in " + time + ". Author or publication are associated with &quot;" + place_val + "&quot;</title>";
					str += "<agents><agent>";
					str += "<uri>" + person +"</uri>"; 
					str += "<label>" + person_sh +"</label>"; 
					str += "<DOB>" + dob_sh +"</DOB>";
					str += "</agent></agents>";
					str += "<start_time>"+ date +"</start_time>";
					str += "<object>"; 
					str += "<uri>" + object +"</uri>"; 
					str += "<label>" + object_sh +"</label>"; 
					str += "</object>";
					str += "<activity>" + activity +"</activity>";
					str += "</event>";
					//for looking up
				//	add_uriLookup(person,person_sh, dob_sh);
				//	add_uriLookup(object,object_sh, null);

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
	
	public String queryBirthOfTimeAndPlace(String time, String place){
			
		String queryStr= "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "SELECT DISTINCT ?person ?dob ?pob ?name ?pob_name ?place_name WHERE {"
				+ "    ?person fb:people.person.date_of_birth ?dob."
				+ "    ?person fb:people.person.profession dbpedia:Artist."
				+ "    ?person fb:type.object.name ?name. FILTER langMatches(lang(?name), \"EN\" )"
				+ "		FILTER (regex(?dob, \"^" + time + "\"))" 
				+ "    ?person fb:people.person.place_of_birth ?pob."
				+ "     FILTER (?pob =<" + place + ">)"
				+ "     ?pob fb:type.object.name ?pob_name.FILTER langMatches(lang(?pob_name), \"EN\" )"  //to get the label of real pob
				+ "     <" + place + "> fb:type.object.name ?place_name. FILTER langMatches(lang(?place_name), \"EN\" )"	//to get the label of setting location 			
				+ "}"
				+ LIMIT;
//		System.out.println(queryStr);
		String result = queryRDF(queryStr);
//		System.out.println(result);
		return result;
	}
	
	public String queryAssociatedBirthOfTimeAndPlace(String time, String place){
		
		String queryStr= "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "SELECT DISTINCT ?person ?dob ?pob ?name ?pob_name ?place_name WHERE {"
				+ "    ?person fb:people.person.date_of_birth ?dob."
				+ "    ?person fb:people.person.profession dbpedia:Artist."
				+ "    ?person fb:type.object.name ?name. FILTER langMatches(lang(?name), \"EN\" )"
				+ "		FILTER (regex(?dob, \"^" + time + "\"))" 
				+ "    ?person dbp-ont:wikiPageWikiLink <" + place + ">."
//				+ "    MINUS (?person fb:people.person.place_of_birth <" + place +">)"
				+ "    ?person fb:people.person.place_of_birth ?pob. "
				+ "    ?pob fb:type.object.name ?pob_name. FILTER langMatches(lang(?pob_name), \"EN\" )"  //to get the label of real pob
				+ "   <" + place + "> fb:type.object.name ?place_name. FILTER langMatches( lang(?place_name), \"EN\" )"	//to get the label of setting location 			
				+ "    FILTER (?pob != <" + place + "> && STR(?pob_name) != STR(?place_name))"
				+ "}"
				+ LIMIT;
//		System.out.println(queryStr);
		String result = queryRDF(queryStr);
//		System.out.println(result);
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
	public String queryDeathOfTimeAndPlace(String time,String place){

		String queryStr= "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "SELECT DISTINCT ?person ?dob ?pob ?name ?pob_name ?place_name WHERE {"
				+ "    ?person fb:people.deceased_person.date_of_death ?dob."
				+ "    ?person fb:people.person.profession dbpedia:Artist."
				+ "    ?person fb:type.object.name ?name. FILTER langMatches(lang(?name), \"EN\" )"
				+ "		FILTER (regex(?dob, \"^" + time + "\"))" 
				+ "    ?person fb:people.deceased_person.place_of_death ?pob."
				+ "    FILTER (?pob =<" + place + ">)"
				+ "    ?pob fb:type.object.name ?pob_name. FILTER langMatches(lang(?pob_name), \"EN\" )"  //to get the label of real pob
				+ "   <" + place + "> fb:type.object.name ?place_name. FILTER langMatches(lang(?place_name), \"EN\" )"	//to get the label of setting location 			
				+ "}"
				+ LIMIT;
//		System.out.println(queryStr);
		String result = queryRDF(queryStr);
//		System.out.println(result);
		return result;
	}
	public String queryAssociatedDeathOfTimeAndPlace(String time,String place){

		String queryStr= "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "SELECT DISTINCT ?person ?dob ?pob ?name ?pob_name ?place_name WHERE {"
				+ "    ?person fb:people.deceased_person.date_of_death ?dob."
				+ "    ?person fb:people.person.profession dbpedia:Artist."
				+ "    ?person fb:type.object.name ?name. FILTER langMatches(lang(?name), \"EN\" )"
				+ "		FILTER (regex(?dob, \"^" + time + "\"))" 
				+ "    ?person dbp-ont:wikiPageWikiLink <" + place + ">."
	            + "    ?person fb:people.deceased_person.place_of_death ?pob. "
				+ "    ?pob fb:type.object.name ?pob_name. FILTER langMatches(lang(?pob_name), \"EN\" )"  //to get the label of real pob
				+ "   <" + place + "> fb:type.object.name ?place_name. FILTER langMatches( lang(?place_name), \"EN\" )"	//to get the label of setting location 			
				+ "    FILTER (?pob != <" + place + "> && STR(?pob_name) != STR(?place_name))"
				+ "}"
				+ LIMIT;
//		System.out.println(queryStr);
		String result = queryRDF(queryStr);
//		System.out.println(result);
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
	
	public String queryArtworksOfTimeAndPlace(String time, String place){
		/*		
		String queryStr= "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>" 
				+ "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "SELECT DISTINCT ?person ?dob ?object ?date ?name ?place_name WHERE {" 
				+ "   ?person fb:visual_art.visual_artist.artworks ?object."
				+ "	  ?person fb:people.person.date_of_birth ?dob."
				+ "   ?object fb:type.object.name ?name."
				+ "   {{?person dbp-ont:wikiPageWikiLink <" + place + ">} UNION {?object dbp-ont:wikiPageWikiLink <"+ place +">}}" 
				+ "    <" + place + "> fb:type.object.name ?place_name."	//to get the label of setting location 
			    + " 	FILTER langMatches( lang(?place_name), \"EN\" )"
             	+ "   OPTIONAL {?object fb:visual_art.artwork.date_begun ?date."
				+ "         BIND( SUBSTR( xsd:string(?date), 1, 4) AS ?begunsub )." 
				+ "         BIND (xsd:decimal(?begunsub) as ?begundec) .}"
                + "    FILTER (!bound(?begundec) || ?begundec <= "+ time +"). "
				+ "   OPTIONAL {?object fb:visual_art.artwork.date_completed ?complete."
				+ "         BIND( SUBSTR( xsd:string(?complete), 1, 4) AS ?completesub)." 
				+ "         BIND (xsd:decimal(?completesub) as ?completedec) .}"
				+ "     FILTER (!bound(?completedec) || ?completedec >= " + time +"). "
				+ "     FILTER (bound(?begundec) || bound(?completedec))."
				+ "     FILTER (bound(?begundec) || (!bound(?begundec) && ?completedec = " + time + "))."
				+ "     FILTER (bound(?completedec) || (!bound(?completedec) && ?begundec = " + time + "))."
				+ "    ?object fb:type.object.name ?artwork_name."
				+ "     FILTER(langMatches(lang(?artwork_name), 'EN'))."
				+"}"
				+ LIMIT;
			*/	
		String queryStr= "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>" 
				+ "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "SELECT DISTINCT ?person ?dob ?object ?date ?name ?place_name WHERE {" 
				+ "   ?person fb:visual_art.visual_artist.artworks ?object."
				+ "	  ?person fb:people.person.date_of_birth ?dob."
				+ "   ?object fb:type.object.name ?name."
				+ "   {{?person dbp-ont:wikiPageWikiLink <" + place + ">} UNION {?object dbp-ont:wikiPageWikiLink <"+ place +">}}" 
				+ "    <" + place + "> fb:type.object.name ?place_name."	//to get the label of setting location 
			    + " 	FILTER langMatches( lang(?place_name), \"EN\" )"
             	+ "   OPTIONAL{?object fb:visual_art.artwork.date_begun ?date."
				+ "         FILTER (regex(?date, \"^" + time + "\")). }" 
				+ "   OPTIONAL {?object fb:visual_art.artwork.date_completed ?complete."
				+ "         FILTER (regex(?complete, \"^" + time + "\")).}" 
				+ "    ?object fb:type.object.name ?artwork_name."
				+ "     FILTER(langMatches(lang(?artwork_name), 'EN'))."
				+"}"
				+ LIMIT;
		
//		System.out.println(queryStr);
		String result = queryRDF(queryStr);
//		System.out.println(result);
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
	public String queryAcquisitionsOfTimeAndPlace(String time, String place){
		
		String queryStr= "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>"
				+ "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "SELECT DISTINCT ?person ?dob ?object ?date ?name ?place_name WHERE {"
				+ "?person fb:visual_art.visual_artist.artworks ?object."
				+ "	  ?person fb:people.person.date_of_birth ?dob."
				+ "?object fb:visual_art.artwork.owners ?relationship."
				+ "?object fb:type.object.name ?name."
				+ "?relationship fb:visual_art.artwork_owner_relationship.date_acquired  ?date."
				+ "  FILTER (regex(?date, \"^" + time + "\"))."
				+ "?relationship  fb:visual_art.artwork_owner_relationship.owner ?owner."
				+ "  {{?object dbp-ont:wikiPageWikiLink <" + place +">} UNION {?owner dbp-ont:wikiPageWikiLink <"+ place +">}}"
				+ "    <" + place + "> fb:type.object.name ?place_name."	//to get the label of setting location 			
				+ " 	FILTER langMatches(lang(?place_name), \"EN\" )"
				+ " }"
				+ LIMIT;
		
//		System.out.println(queryStr);
		String result = queryRDF(queryStr);
//		System.out.println(result);
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
	public String queryWritingsOfTimeAndPlace(String time, String place){
	
		String queryStr= "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>"
				+ "PREFIX fb: <http://rdf.freebase.com/ns/>"
				+ "PREFIX dbp-ont: <http://dbpedia.org/ontology/>"
				+ "PREFIX dbpedia: <http://dbpedia.org/resource/>"
				+ "SELECT DISTINCT ?person ?dob ?object ?date ?name ?place_name WHERE {"
				+ "      ?person fb:book.author.works_written ?object." 
				+ "	     ?person fb:people.person.date_of_birth ?dob."
				+ "      ?object fb:type.object.name ?name."
				+ "      {{?person dbp-ont:wikiPageWikiLink <" + place + ">} UNION {?object dbp-ont:wikiPageWikiLink <" + place + ">}}" 
				+ "    <" + place + "> fb:type.object.name ?place_name."	//to get the label of setting location 			
				+ " 	FILTER langMatches( lang(?place_name), \"EN\" )"
				+ "      ?object fb:book.book.editions ?edition."
				+ "      ?edition fb:book.book_edition.publication_date ?date."  
				+ "      BIND( SUBSTR( xsd:string(?date), 1, 4) AS ?datesub )."
				+ "      BIND (xsd:decimal(?datesub) as ?datedec)."
				+ "      FILTER (?datedec = " + time + ")."
//				+ "      ?person dbp-ont:wikiPageWikiLink dbpedia:Art." 
				+"}"
				+ LIMIT;
//		System.out.println(queryStr);
		String result = queryRDF(queryStr);
//		System.out.println(result);
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
		String dossier = "http://decipher-research.eu/dossiers/1533";
//		String queryURI = "http://localhost:8088/openrdf-sesame/repositories/RecommenderDemo?query=";
		String queryURI = "http://storyscope5.ssl.co.uk:8080/openrdf-sesame/repositories/test?query=";
//		String plot = "http://mckm219286.open.ac.uk/decipher/trunk/decipher-3/plot/4413";
		EventRecommender recommend= new EventRecommender(queryURI, dossier);
//		EventCluster eCluster= new EventCluster(queryURI, dossier);
		recommend.recommend();
//		EventRecommender recommend= new EventRecommender(dossier,plot);
//		PlotEventCluster eCluster= new PlotEventCluster(queryURI, dossier,plot);

//		Map<String, Set<String>> setting = eCluster.clusterEvents(3);
//		recommend.recommendEvents(setting);
//		recommend.queryArtworksOfTimeAndPlace("1934", "London");	
	}

}
