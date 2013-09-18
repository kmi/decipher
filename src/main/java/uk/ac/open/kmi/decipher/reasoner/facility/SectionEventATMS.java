package uk.ac.open.kmi.decipher.reasoner.facility;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.UUID;

import uk.ac.open.kmi.decipher.cluster.ClusterResult;
import uk.ac.open.kmi.decipher.cluster.IAlgorithm;
import uk.ac.open.kmi.decipher.cluster.KMeansCluster;
import uk.ac.open.kmi.decipher.repository.SPARQLClient;
import uk.ac.open.kmi.decipher.util.FileUtil;
import uk.ac.open.kmi.decipher.util.QueryResult;
import uk.ac.open.kmi.decipher.util.QueryRow;
import uk.ac.open.kmi.decipher.util.SPARQLQueryResultParser;
import uk.ac.open.kmi.decipher.util.SPARQLUtil;
import weka.core.Instance;
import weka.core.Instances;

public class SectionEventATMS {

	private static final String STORY_ID ="3676";//"2682";"2849"; "2933"; "3676";
	private String queryEndPoint;
	private String dossierID;

	List<String> taggedEvents_list= new ArrayList<String>();

	public SectionEventATMS(String repQueryURI, String dossier) {
		this.queryEndPoint = repQueryURI;
		this.dossierID = dossier;
	}

	private String getEventProperties (){
		String queryEventProperities = SPARQLUtil.PREFIX 
				+ "SELECT  ?section ?object ?event ?sTime ?eTime ?agent ?act ?style ?genre ?loc ?val ?obj "   //now plot is story, plotDesc is story section
//				+ "WHERE {<" + dossierID +"> curate:tellsStoryOfDossier ?plot. "
//				+ "?plot curate:containsStorySection ?plotDesc. "
				+ "WHERE {<http://decipher-research.eu/stories/"+ STORY_ID+"> curate:containsStorySection ?section. "
				+ "?section curate:hasAssociatedObject ?object. "
				+ "optional {?section curate:hasAssociatedEvent ?event. "
				+ "			optional {?event curate:hasStartTime ?sTime.} "
				+ "			optional {?event curate:hasEndTime ?eTime.}"
				+ "			optional {?event curate:hasAgent ?agentFB. ?agentFB rdfs:label ?agent.}"
				+ "			optional {?event curate:hasActivity ?actFB. ?actFB rdfs:label ?act. }" 
				+ "			optional {?event curate:hasStyleMovement ?styleFB. ?styleFB rdfs:label ?style.} "
				+ "			optional {?event curate:hasGenre ?genreFB. ?genreFB rdfs:label ?genre.} "
				+ "			optional {?event curate:hasLocation ?locFB. ?locFB rdfs:label ?loc.} "
				+ "			optional {?event curate:hasObservedValue ?valFB. ?valFB rdfs:label ?val. } "
				+ "			optional {?event curate:includesPhysicalObject ?objFB. ?objFB rdfs:label ?obj.}" 
				+ "			} "
				+ "}";	 
		SPARQLClient sClient = new SPARQLClient();
		String results = sClient.httpQuery(queryEndPoint, queryEventProperities, "application/sparql-results+xml");
		System.out.println(results);
		return results;
	}
	

	public void clusterAction(){
		
		String input = File.separator + "Decipher" + File.separator +"tmp" + File.separator + dossierID.substring(dossierID.lastIndexOf("/")+1) + "_" +STORY_ID +"_input.arff";
		String sparqlResults = getEventProperties(); 
		generateArff(sparqlResults, input);		
	}
	

	public void generateArff(String sparqlResult, String input){
		//for all the values of each property (across all events)
		Set<String> sTime=new HashSet<String>(), eTime=new HashSet<String>(), agent=new HashSet<String>(), act=new HashSet<String>(), 
				style=new HashSet<String>(), theme=new HashSet<String>(), genre=new HashSet<String>(), loc=new HashSet<String>(), 
				dim=new HashSet<String>(), val=new HashSet<String>(), obj=new HashSet<String>(), material=new HashSet<String>();

		//for all the values of each property of one event
		Set<String> e_sTime=new HashSet<String>(), e_eTime=new HashSet<String>(), e_agent=new HashSet<String>(), e_act=new HashSet<String>(), 
				e_style=new HashSet<String>(), e_theme=new HashSet<String>(), e_genre=new HashSet<String>(), e_loc=new HashSet<String>(), 
				e_dim=new HashSet<String>(), e_val=new HashSet<String>(), e_obj=new HashSet<String>(), e_material=new HashSet<String>();
	    //name value pair of each property of one event
		Map<String,Set<String>> pairs = new HashMap<String,Set<String>>();
        
		//all the sections of one story		
		Set<String> sections = new HashSet<String>();
		//all the events of one story
		Set<String> taggedEvents_all = new HashSet<String>();
		//all the objects of one story
		Set<String> objects_all = new HashSet<String>();
		//all the events of one section
		Set<String> taggedEvents = new HashSet<String>();
		//all the objects of one section
		Set<String> objects = new HashSet<String>();
		
		
		Map<String, Set<String>> sectionObjects = new HashMap<String, Set<String>>();
        Map<String, Set<String>> sectionEvents = new HashMap<String, Set<String>>();
 		
		Map<String, Set<String>> propertiesValues = new HashMap<String,Set<String>>(); //for every single property, property is the key
		Map<String, Map<String,Set<String>>> eventsPropertiesValues = new HashMap<String, Map<String,Set<String>>>(); //for every event, event is the key
        
       
		boolean FIRST_SECTION = true; String prev_section=null;
		boolean FIRST_EVENT = true; String prev_event=null;

		QueryResult qr = SPARQLQueryResultParser.parse(sparqlResult);	
		List<QueryRow> rows = qr.getQueryRows();
		Iterator<QueryRow> rows_iter = rows.iterator();
		while (rows_iter.hasNext()){
			QueryRow row = rows_iter.next();
			String section = row.getValue("section");		
//			String section_domainURI= section.substring(0,section.lastIndexOf("/")+1); //4 digits now//8 digits plus one forward slash
			String sectionID = section.substring(section.lastIndexOf("/")+1); 
			
			if(!FIRST_SECTION && !sections.contains(sectionID)) //until it meets the new section
			{
				sectionObjects.put(prev_section, new HashSet<String>(objects));
				sectionEvents.put(prev_section, new HashSet<String>(taggedEvents));
				objects_all.addAll(objects);taggedEvents_all.addAll(taggedEvents);
				objects.clear(); taggedEvents.clear();
			}			
			FIRST_SECTION = false;
			
			if (row.getValue("object")!=null)
			{
				String object = row.getValue("object");
				String objectID = object.substring(object.lastIndexOf("/")+1);
				objects.add(objectID);				
			}
			if (row.getValue("event")!=null)
			{	String event = row.getValue("event");
				String eventID = event.substring(event.lastIndexOf("/")+1);				
				if (!FIRST_EVENT && !taggedEvents.contains(eventID))
				{				
					pairs.put("sTime", new HashSet<String>(e_sTime)); 
					pairs.put("eTime", new HashSet<String>(e_eTime));
					pairs.put("agent", new HashSet<String>(e_agent));
					pairs.put("act", new HashSet<String>(e_act));
					pairs.put("style", new HashSet<String>(e_style));
					//	pairs.put("theme", new HashSet<String>(e_theme));
					pairs.put("genre", new HashSet<String>(e_genre));
					pairs.put("loc", new HashSet<String>(e_loc));
					//	pairs.put("dim", new HashSet<String>(e_dim));
					pairs.put("val", new HashSet<String>(e_val));
					pairs.put("obj", new HashSet<String>(e_obj));
					//	pairs.put("material", new HashSet<String>(e_material));
					eventsPropertiesValues.put(prev_event, new HashMap<String,Set<String>>(pairs));
					e_sTime.clear(); e_eTime.clear(); e_agent.clear(); e_act.clear();e_style.clear();e_theme.clear();e_genre.clear();
					e_loc.clear(); e_dim.clear();e_val.clear();e_obj.clear();e_material.clear();
					pairs.clear();
				}	
				FIRST_EVENT = false;

				if (row.getValue("sTime") !=null && !row.getValue("sTime").contains("freebase.com"))
				{   
					String tmp_sTime = row.getValue("sTime");
				    String temp = tmp_sTime.substring(0,tmp_sTime.indexOf("-")).replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
					sTime.add(temp); e_sTime.add(tmp_sTime.substring(0,tmp_sTime.indexOf("-")));
				} //System.out.println ("sTime: " + row.getValue("sTime")); 
				if (row.getValue("eTime") !=null && !row.getValue("eTime").contains("freebase.com")) 
				{
					String tmp_eTime=row.getValue("eTime");
					String temp = tmp_eTime.substring(0,tmp_eTime.indexOf("-")).replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
					eTime.add(temp); e_eTime.add(tmp_eTime.substring(0,tmp_eTime.indexOf("-"))); 
				}//System.out.println ("eTime: " + row.getValue("eTime")); }
				if (row.getValue("agent") !=null && !row.getValue("agent").contains("freebase.com")) {String temp = row.getValue("agent").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				agent.add(temp); e_agent.add(row.getValue("agent")); } //System.out.println ("agent: " + row.getValue("agent"));} 
				if (row.getValue("act") !=null && !row.getValue("act").contains("freebase.com")) {String temp = row.getValue("act").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				act.add(temp); e_act.add(row.getValue("act")); } //System.out.println ("act: " + row.getValue("act")); }
				if (row.getValue("style") !=null && !row.getValue("style").contains("freebase.com")) {String temp = row.getValue("style").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				style.add(temp); e_style.add(row.getValue("style"));}  //System.out.println ("style: " + row.getValue("style")); }
				//		if (row.getValue("theme") !=null ) {String temp = row.getValue("theme").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				//		   theme.add(temp); e_theme.add(row.getValue("theme"));} //System.out.println ("theme: " + row.getValue("theme")); }
				if (row.getValue("genre") !=null && !row.getValue("genre").contains("freebase.com")) {String temp = row.getValue("genre").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				genre.add(temp); e_genre.add(row.getValue("genre")); } //System.out.println ("genre: " + row.getValue("genre")); } 
				if (row.getValue("loc") !=null && !row.getValue("loc").contains("freebase.com")) {String temp = row.getValue("loc").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				loc.add(temp); e_loc.add(row.getValue("loc")); } //System.out.println("loc: " + row.getValue("loc"));} 
				//		if (row.getValue("dim") !=null ) {String temp = row.getValue("dim").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				//		   dim.add(temp); e_dim.add(row.getValue("dim")); } //System.out.println("dim: " + row.getValue("dim"));} 
				if (row.getValue("val") !=null && !row.getValue("val").contains("freebase.com")) {String temp = row.getValue("val").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				val.add(temp); e_val.add(row.getValue("val")); }//System.out.println("val: " + row.getValue("val")); }
				if (row.getValue("obj") !=null && !row.getValue("obj").contains("freebase.com")) {String temp = row.getValue("obj").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				obj.add(temp); e_obj.add(row.getValue("obj")); } //System.out.println("obj: " + row.getValue("obj"));} 
				//		if (row.getValue("material") !=null ) {String temp = row.getValue("material").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				//			material.add(temp); e_material.add(row.getValue("material")); } //System.out.println ("materail: " + row.getValue("material")); }

				taggedEvents.add(eventID);
				prev_event = eventID;
			}
			
			sections.add(sectionID);
			prev_section =sectionID;

		}		
		// for the last event		
		pairs.put("sTime", new HashSet<String>(e_sTime)); 
		pairs.put("eTime", new HashSet<String>(e_eTime));
		pairs.put("agent", new HashSet<String>(e_agent));
		pairs.put("act", new HashSet<String>(e_act));
		pairs.put("style", new HashSet<String>(e_style));
	//	pairs.put("theme", new HashSet<String>(e_theme));
		pairs.put("genre", new HashSet<String>(e_genre));
		pairs.put("loc", new HashSet<String>(e_loc));
	//	pairs.put("dim", new HashSet<String>(e_dim));
		pairs.put("val", new HashSet<String>(e_val));
		pairs.put("obj", new HashSet<String>(e_obj));
	//	pairs.put("material", new HashSet<String>(e_material));
		eventsPropertiesValues.put(prev_event, new HashMap<String,Set<String>>(pairs));
		
		//for the last section
		sectionObjects.put(prev_section, new HashSet<String>(objects));
		sectionEvents.put(prev_section, new HashSet<String>(taggedEvents));
		objects_all.addAll(objects);taggedEvents_all.addAll(taggedEvents);
	
		propertiesValues.put("sTime", sTime); 
		propertiesValues.put("eTime", eTime);
		propertiesValues.put("agent", agent);
		propertiesValues.put("act", act);
		propertiesValues.put("style", style);
	//	propertiesValues.put("theme",theme);
		propertiesValues.put("genre", genre);
		propertiesValues.put("loc", loc);
	//	propertiesValues.put("dim", dim);
		propertiesValues.put("val", val);
		propertiesValues.put("obj", obj);
	//	propertiesValues.put("material", material);

		String str="%The Mohan WEKA cluster test file\n";
		str += "@relation " + dossierID + "\n";
		str += "@attribute sectionID numeric\n";
		str += "@attribute eventID numeric\n";

		String othersStr="", sTimeStr="", eTimeStr="";

		List<String> propertyValues_list = new ArrayList<String>(); 
		propertyValues_list.add(0,"sectionID");
		propertyValues_list.add(1,"eventID");
		
		Set<String> props = propertiesValues.keySet();
		Iterator propsIter = props.iterator();
		while (propsIter.hasNext()){
			String prop = (String) propsIter.next();
			Set<String> propVals= propertiesValues.get(prop);
			
			if (prop.equalsIgnoreCase("sTime"))
			{ sTimeStr = "@attribute start_date {";
			  propertyValues_list.add(2,prop);			
			  for (String propVal:propVals)
			  {  
				  sTimeStr += propVal + ",";
			   }
			  sTimeStr += "}\n";
			 }
			else if (prop.equalsIgnoreCase("eTime"))
			{    eTimeStr = "@attribute end_date {";
			    propertyValues_list.add(3,prop);
			    for (String propVal:propVals)
				{  
					eTimeStr += propVal + ",";
					
				}
				eTimeStr += "}\n";
			}
			else//if (! prop.equalsIgnoreCase("sTime") && !prop.equalsIgnoreCase("eTime"))
			{
				for (String propVal:propVals)
				{  
					othersStr += "@attribute " + prop +"_" + propVal + "{yes,no}\n";
					propertyValues_list.add(prop +"_" + propVal);
				}	
			}
		}
		
		str += sTimeStr + eTimeStr + othersStr;
		str += "\n @data \n";
		Set<String> sectionKeys = sectionEvents.keySet();
		Iterator secKey_iter = sectionKeys.iterator();
		while (secKey_iter.hasNext())
		{
			String sec = (String) secKey_iter.next();
			Set<String> sec_events = sectionEvents.get(sec);
			Iterator te_iter = sec_events.iterator();
			while (te_iter.hasNext()){
				String te = (String) te_iter.next();			

				List<String> eventData = new ArrayList<String>();
				eventData.add(sec);eventData.add(te);
				eventData.add("?");eventData.add("?");
				for (int i=4;i< propertyValues_list.size();i++)
					eventData.add("no");

				Map<String,Set<String>> evevtDataPairs = eventsPropertiesValues.get(te);

				Set<String> eDataKeys = evevtDataPairs.keySet();
				Iterator eDataKeysIter = eDataKeys.iterator();
				while (eDataKeysIter.hasNext()){
					String eDataKey = (String) eDataKeysIter.next();
					Set<String> eData= evevtDataPairs.get(eDataKey);
					if (eDataKey.equalsIgnoreCase("sTime"))
					{
						Iterator eDataIter = eData.iterator();
						while (eDataIter.hasNext()){
							String eVal = (String) eDataIter.next();						
							eventData.set(propertyValues_list.indexOf("sTime"), eVal);

						}
					}
					else if (eDataKey.equalsIgnoreCase("eTime"))
					{
						Iterator eDataIter = eData.iterator();
						while (eDataIter.hasNext()){
							String eVal = (String) eDataIter.next();						
							eventData.set(propertyValues_list.indexOf("eTime"), eVal);
						}
					}
					else{
						Iterator eDataIter = eData.iterator();
						while (eDataIter.hasNext()){
							String eVal = (String) eDataIter.next();
							//		System.out.println(eDataKey+ "_" + eVal);
							if(propertyValues_list.contains(eDataKey+ "_" + eVal.replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "")))
							{
								//		  System.err.println("MATCHED" + eDataKey+ "_" + eVal);	
								eventData.set(propertyValues_list.indexOf(eDataKey+ "_" + eVal.replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "")), "yes");
							}						
						}
					}
				}
				for (int i=0;i<eventData.size()-1;i++)
					str +=eventData.get(i)+ ", ";
				str +=eventData.get(eventData.size()-1)+ "\n";
			}
		}

		
		String fn= input;
		System.out.println(fn);
		BufferedWriter writer = null;
		try {
			writer = new BufferedWriter( new FileWriter(fn));
			writer.write( str);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (writer!= null)
			try {
				writer.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

	}

	public static void main(String[] args) throws Exception{
		String dossier = "http://decipher-research.eu/dossiers/1533";
//		String queryURI = "http://localhost:8088/openrdf-sesame/repositories/RecommenderDemo?query=";
		String queryURI = "http://storyscope5.ssl.co.uk:8080/openrdf-sesame/repositories/test?query=";
		SectionEventATMS pCluster= new SectionEventATMS(queryURI, dossier);
		pCluster.clusterAction();
	}	
}
