package uk.ac.open.kmi.decipher.reasoner;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import org.apache.commons.lang.StringEscapeUtils;
import org.armedbear.lisp.JavaObject;

import uk.ac.open.kmi.decipher.classifier.ID3Classifier;
import uk.ac.open.kmi.decipher.classifier.J48Classifier;
import uk.ac.open.kmi.decipher.reasoner.facility.LispBridge;
import uk.ac.open.kmi.decipher.repository.SPARQLClient;
import uk.ac.open.kmi.decipher.util.FileUtil;
import uk.ac.open.kmi.decipher.util.QueryResult;
import uk.ac.open.kmi.decipher.util.QueryRow;
import uk.ac.open.kmi.decipher.util.SPARQLQueryResultParser;
import uk.ac.open.kmi.decipher.util.SPARQLUtil;

public class NarrativeRecommender {
	
	private static final String NARRATIVE_ROOT_ELEMENT = "narrative";//"plot_element";
	private static final String NARRATIVE_TYPE = "type";
	private static final String PLOT = "storyline";
	private static final String PLOT_ID = "storyline_id";
	private static final String PLOT_NAME = "storyline_name";
	private static final String PLOT_TAGS = "storyline_tags";
	private static final String PLOT_ELEMENT = "section";
	private static final String PLOT_ELEMENT_ID = "section_id";
	private static final String PLOT_ELEMENT_WEIGHT = "weight";
	private static final String PLOT_ELEMENT_PARENT = "parent";

    private static final String TEMP_DIR = File.separator + "data" + File.separator + "Decipher" + File.separator +"tmp"; 
//    private static final String TEMP_DIR = File.separator + "Decipher" + File.separator +"tmp"; 
	    
	String queryEndPoint;
	String dossierID,storyID;
	private String cacheFileName;
	String plotDesc_domainURI="";
	String te_domainURI="";
	Map<String,String> plotTimeMap = new HashMap<String,String>(); //the earliest time for each plotElement
	LispBridge LispATMS;
	
	public NarrativeRecommender(String repQueryURI, String dossier, LispBridge Lisp_atms) {
		this.queryEndPoint = repQueryURI;
		this.dossierID = dossier;
		this.cacheFileName = TEMP_DIR + File.separator + dossierID.substring(dossierID.lastIndexOf("/")+1) + "_" + storyID.substring(storyID.lastIndexOf("/")+1) +"_recommendedNarratives.xml";
		LispATMS = Lisp_atms;
	}

	public NarrativeRecommender(String repQueryURI, String dossier, String story, LispBridge Lisp_atms) {
		this.queryEndPoint = repQueryURI;
		this.dossierID = dossier;
		this.storyID = story;
		this.cacheFileName = TEMP_DIR + File.separator + dossierID.substring(dossierID.lastIndexOf("/")+1) + "_" + storyID.substring(storyID.lastIndexOf("/")+1) +"_recommendedNarratives.xml";
		LispATMS = Lisp_atms;
	}

	
	public String returnCacheOrEmpty() {
	      File cacheFile = new File(cacheFileName);
	        if(!cacheFile.exists())
	        {
	        	String respStr = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?> <" + NARRATIVE_ROOT_ELEMENT +"s>" + "</" + NARRATIVE_ROOT_ELEMENT +"s>";
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
	        	String respStr = recommendNarratives();
		    	FileUtil.write2File(respStr, cacheFileName);
	        	return respStr;
	        	}
	        else
	        {
	        	String respStr = FileUtil.readFile(cacheFileName);
	        	return respStr;
	        }
	}
	
	public String recommendNarratives() {		
		
		String sparqlXML = this.getSectionValues();		
		String input = TEMP_DIR + File.separator + dossierID.substring(dossierID.lastIndexOf("/")+1) + "_" + storyID.substring(storyID.lastIndexOf("/")+1) +"_input.arff";
		String inputStr = generateArff(sparqlXML, input);
		String ATMS_result = LispATMS.bridge(inputStr);
	   	FileUtil.write2File(ATMS_result, cacheFileName);
		return ATMS_result;		
	}
	
	//hasObjectCreationDate hasCreditLine hasIdentifier
	//hasClassification hasMaterials hasCurrentLocation  hasLocationOfOrigin hasDimension hasObjectCreator

	private String getSectionValues(){
		
		String query = SPARQLUtil.PREFIX 
				+ "SELECT distinct ?title ?section  ?object ?event ?prop ?val "
				+ "WHERE {<" + dossierID +"> curate:tellsStoryOfDossier <" + storyID +">. "
	 		    + "<" + storyID + "> curate:containsStorySection ?section. "
	 		    + "optional {?section dc:title ?title.} "
	 		   	+ "{" 
	 		    +  "{?section curate:hasAssociatedObject ?object." 			
	 			+   "optional {{?object curate:hasObjectCreationDate ?val. BIND(curate:hasObjectCreationDate as ?prop)} UNION "			
	 			+   " {?object curate:hasClassification ?classiFB. ?classiFB rdfs:label ?val. BIND(curate:hasClassification as ?prop)}	UNION "		
	 			+   " {?object curate:hasMaterials ?matFB. ?matFB rdfs:label ?val. BIND(curate:hasMaterials as ?prop)}	UNION "
	 			+	" {?object curate:hasCurrentLocation ?currLocFB. ?currLocFB rdfs:label ?val. BIND(curate:hasCurrentLocation as ?prop)} 	UNION "		
	 			+   " {?object curate:hasLocationOfOrigin ?origLocFB. ?origLocFB rdfs:label ?val. BIND(curate:hasLocationOfOrigin as ?prop)} UNION "		
	 			+   " {?object curate:hasDimension ?dimFB. ?dimFB rdfs:label ?val. BIND(curate:hasDimension as ?prop)} 	UNION "
	 			+   " {?object curate:hasObjectCreator ?creatorFB. ?creatorFB rdfs:label ?val. BIND(curate:hasObjectCreator as ?prop)} UNION "
	 			+   " {?object rdfs:seeAlso ?alsoFB . ?alsoFB rdfs:label ?val . BIND(rdfs:seeAlso as ?prop)}}"
	 			+   "}"
                +    "UNION"         
	 			+   "{?section curate:hasAssociatedEvent ?event. "			
	 			+   "optional {{?event curate:hasStartTime ?val. BIND(curate:hasStartTime as ?prop)} UNION "	
	 			+   "  {?event curate:hasEndTime ?val. BIND(curate:hasStartTime as ?prop)} UNION "	 
	 			+   "  {?event curate:hasAgent ?agentFB. ?agentFB rdfs:label ?val. BIND(curate:hasAgent as ?prop)} UNION "	
	 			+   "  {?event curate:hasActivity ?actFB. ?actFB rdfs:label ?val. BIND(curate:hasActivity as ?prop)} UNION "		
	 			+   "  {?event curate:hasStyleMovement ?styleFB. ?styleFB rdfs:label ?val. BIND(curate:hasStyleMovement as ?prop)} UNION "		
	 			+   "  {?event curate:hasGenre ?genreFB. ?genreFB rdfs:label ?val. BIND(curate:hasGenre as ?prop)} 	UNION "		
	 			+   "  {?event curate:hasLocation ?locFB. ?locFB rdfs:label ?val. BIND(curate:hasLocation as ?prop)} UNION "		
	 			+   "  {?event curate:hasObservedValue ?valFB. ?valFB rdfs:label ?val. BIND(curate:hasObservedValue as ?prop)} UNION "		
//	 			+   "  {?event curate:includesPhysicalObject ?objFB. ?objFB rdfs:label ?val. BIND(curate:includesPhysicalObject as ?prop)} UNION "
	 			+   "  {?event rdfs:seeAlso ?alsoFB . ?alsoFB rdfs:label ?val . BIND(rdfs:seeAlso as ?prop)}}"
	 			+   "}"
				+ "}"
	 			+"}"; 
         System.out.println(query);
		 long start = System.currentTimeMillis();
		 SPARQLClient sClient = new SPARQLClient();
		 String results = sClient.httpQuery(queryEndPoint, query, "application/sparql-results+xml");
		 long duration = System.currentTimeMillis() -start;		 
         System.out.println("The time to spend " + duration);
//		 System.out.println(results);
		 return results;
		}
	
	public String generateArff(String sparqlResult, String input){
		
		//for all the values of each property (across all objects)
		Set<String> date=new HashSet<String>(), classi=new HashSet<String>(), mat=new HashSet<String>(), 
				currLoc=new HashSet<String>(), origLoc=new HashSet<String>(), dim=new HashSet<String>(), creator=new HashSet<String>(); 
		//for all the values of each property (across all events)		
		Set<String> sTime=new HashSet<String>(), eTime=new HashSet<String>(), agent=new HashSet<String>(), act=new HashSet<String>(), 
				style=new HashSet<String>(), genre=new HashSet<String>(), loc=new HashSet<String>(), value=new HashSet<String>(); 
		//for both event's and object's seeAlso
		Set<String> also=new HashSet<String>(); 
		
		//for all the values of each property (across all objects)
		Set<String> objVal=new HashSet<String>();		
		//for all the values of each property of one object
		Set<String> o_val=new HashSet<String>();
	    //name value pair of each property of one object
		Map<String,Set<String>> objectPairs = new HashMap<String,Set<String>>();

		
		//for all the values of each property (across all events)
		Set<String> eveVal=new HashSet<String>();		
		//for all the values of each property of one event
		Set<String> e_val=new HashSet<String>();		
	    //name value pair of each property of one event
		Map<String,Set<String>> eventPairs = new HashMap<String,Set<String>>();
		
        //all the sections of one story		
		Set<String> sections = new HashSet<String>();
		//all the events of one section
		Set<String> taggedEventsPerSection = new HashSet<String>();
		//all the objects of one section
		Set<String> objectsPerSection = new HashSet<String>();
				
		Map<String, Set<String>> sectionObjects = new HashMap<String, Set<String>>();
        Map<String, Set<String>> sectionEvents = new HashMap<String, Set<String>>();
        Map<String, String> sectionTitle = new HashMap<String, String>();
 		
		Map<String, Set<String>> propertiesValues = new HashMap<String,Set<String>>(); //for every single property, property is the key
		Map<String, Map<String,Set<String>>> eventsPropertiesValues = new HashMap<String, Map<String,Set<String>>>(); //for every event, event is the key
		Map<String, Map<String,Set<String>>> objectsPropertiesValues = new HashMap<String, Map<String,Set<String>>>(); //for every object, object is the key
       
       
		boolean FIRST_SECTION = true; String prev_section=null;
		boolean FIRST_OBJECT = true; String prev_object=null;
		boolean FIRST_EVENT = true; String prev_event=null;
        int count=0;
		QueryResult qr = SPARQLQueryResultParser.parse(sparqlResult);	
		List<QueryRow> rows = qr.getQueryRows();
		Iterator<QueryRow> rows_iter = rows.iterator();
		while (rows_iter.hasNext()){
			QueryRow row = rows_iter.next();
			String section = row.getValue("section");	
			String sectionID = section.substring(section.lastIndexOf("/")+1); 
            count++;
            
			if (row.getValue("title")!=null)
			{
				String title = row.getValue("title");
				if (!sectionTitle.containsKey(sectionID)) sectionTitle.put(sectionID, title);
			}
			
			if(!FIRST_SECTION && !sections.contains(sectionID)) //until it meets the new section
			{
	        //    System.err.println("recorded: " + prev_section);
				sectionObjects.put(prev_section, new HashSet<String>(objectsPerSection)); //It is important to create a new Hashset because of how Collection allocates memories.
				sectionEvents.put(prev_section, new HashSet<String>(taggedEventsPerSection));
				objectsPerSection.clear(); 
				taggedEventsPerSection.clear();
				if (!sectionTitle.containsKey(prev_section)) sectionTitle.put(prev_section, "");			
			}			
			FIRST_SECTION = false;
			
			if (row.getValue("object")!=null)
			{
				String object = row.getValue("object");
				String objectID = object.substring(object.lastIndexOf("/")+1);
								
				if (!FIRST_OBJECT && !objectsPerSection.contains(objectID))
				{				
					objectPairs.put("val", new HashSet<String>(o_val));					
					objectsPropertiesValues.put(prev_object, new HashMap<String,Set<String>>(objectPairs));
					o_val.clear();
					objectPairs.clear();
				}	
				FIRST_OBJECT = false;
				if (row.getValue("val") !=null)					
				{   
					String tmp_val = row.getValue("val");
					String prop = row.getValue("prop");
					if (prop.contains("hasObjectCreationDate"))
					{
					    String temp = tmp_val.substring(0,tmp_val.indexOf("-")); 
						objVal.add(temp); o_val.add(temp); 	
						date.add(temp);
					}
					else 
					{
						String temp = row.getValue("val");
						objVal.add(temp); o_val.add(temp);
						if (prop.contains("hasClassification")) classi.add(temp);
						if (prop.contains("hasMaterials")) mat.add(temp);
						if (prop.contains("hasCurrentLocation")) currLoc.add(temp);
						if (prop.contains("hasLocationOfOrigin")) origLoc.add(temp);
						if (prop.contains("hasDimension")) dim.add(temp);
						if (prop.contains("hasObjectCreator")) creator.add(temp);
						if (prop.contains("seeAlso")) also.add(temp); 
					}					
				}				
					
				objectsPerSection.add(objectID);
				prev_object = objectID;
			}
			
			if (row.getValue("event")!=null)
			{	String event = row.getValue("event");
				String eventID = event.substring(event.lastIndexOf("/")+1);
				
				if (!FIRST_EVENT && !taggedEventsPerSection.contains(eventID))
				{				
					eventPairs.put("val", new HashSet<String>(e_val));
					eventsPropertiesValues.put(prev_event, new HashMap<String,Set<String>>(eventPairs));
					e_val.clear();
					eventPairs.clear();
				}	
				FIRST_EVENT = false;
                //no differentiation of start time and end time
				
				if (row.getValue("val") !=null)					
				{   
					String tmp_val = row.getValue("val");
					String prop = row.getValue("prop");
					if (prop.contains("hasStartTime") || row.getValue("prop").contains("hasEndTime"))
					{
					    String temp = tmp_val.substring(0,tmp_val.indexOf("-")); 
						eveVal.add(temp); e_val.add(temp);
						if (prop.contains("hasStartTime")) sTime.add(temp);
						if (prop.contains("hasEndTime")) eTime.add(temp);
					}
					else 
					{
						String temp = row.getValue("val");//.replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", ""); //for the uniqueness, same below
						eveVal.add(temp); e_val.add(temp);
						if (prop.contains("hasAgent")) agent.add(temp);
						if (prop.contains("hasLocation")) loc.add(temp);
						if (prop.contains("hasActivity")) act.add(temp);
						if (prop.contains("hasGenre")) genre.add(temp);
						if (prop.contains("hasStyleOrMovement")) style.add(temp);
						if (prop.contains("hasObservedValue")) value.add(temp);
						if (prop.contains("seeAlso")) also.add(temp); 						
					}
				}	
				taggedEventsPerSection.add(eventID);
				prev_event = eventID;
			}
			
			sections.add(sectionID);
			prev_section =sectionID;

		}		
		// for the last object		
		objectPairs.put("val", new HashSet<String>(o_val));		
		objectsPropertiesValues.put(prev_object, new HashMap<String,Set<String>>(objectPairs));
		// for the last event		
		eventPairs.put("val", new HashSet<String>(e_val));
		eventsPropertiesValues.put(prev_event, new HashMap<String,Set<String>>(eventPairs));		
		//for the last section
		sectionObjects.put(prev_section, new HashSet<String>(objectsPerSection));
		sectionEvents.put(prev_section, new HashSet<String>(taggedEventsPerSection));
		objectsPerSection.clear(); 
		taggedEventsPerSection.clear();
		
		propertiesValues.put("oVal", objVal);
		propertiesValues.put("eVal", eveVal);
		
		//mapping of property to values
	    Map<String, Set<String>> tMap = new HashMap<String, Set<String>>();
	       //for event properties
	        tMap.put("hasStartTime", sTime);
	        tMap.put("hasEndTime", eTime);
	        tMap.put("hasAgent", agent);
	        tMap.put("hasLocation", loc);
	        tMap.put("hasActivity", act);
	        tMap.put("hasGenre", genre);
	        tMap.put("hasStyleOrMovement", style);
	        tMap.put("hasObservedValue", value);
	        //for object properties       
	        tMap.put("hasObjectCreationDate", date);
	        tMap.put("hasClassification", classi);
	        tMap.put("hasMaterials", mat);
	        tMap.put("hasCurrentLocation", currLoc);
	        tMap.put("hasLocationOfOrigin", origLoc);
	        tMap.put("hasDimension", dim);
	        tMap.put("hasObjectCreator", creator);  
	        //for rdfs:seeAlso
	        tMap.put("seeAlso", also);  

		String str="%The Mohan WEKA cluster test file\n";
		str += "@relation " + dossierID + "\n";
		str += "@attribute sectionID numeric\n";
		str += "@attribute eventID/objectID numeric\n";

		String inputAsStr= "("; //starting bracket
		String sectionTitleStr="(";
		String propValMapAsStr = "(";		
		String valueListAsStr = "(";		

		List<String> propertyValues_list = new ArrayList<String>(); 
		propertyValues_list.add(0,"sectionID");
		propertyValues_list.add(1,"eventID");
		
		// Processing the section title
		Set<String> sects = sectionTitle.keySet(); //eVal and oVal
		Iterator sectsIter = sects.iterator();
		while (sectsIter.hasNext()){
			String sect = (String) sectsIter.next();
			String title= sectionTitle.get(sect);
			sectionTitleStr +="("+sect + " \"" + title +"\") ";	
		}
		if (sectionTitleStr.endsWith(") ")) sectionTitleStr = sectionTitleStr.substring(0, sectionTitleStr.length()-1) + ") ";
		inputAsStr +=sectionTitleStr;
		
		// Processing the property value map
		Set<String> allProps = tMap.keySet(); //eVal and oVal
		Iterator allPropsIter = allProps.iterator();
		while (allPropsIter.hasNext()){
			String prop = (String) allPropsIter.next();
			Set<String> propVals= tMap.get(prop);
			if (!propVals.isEmpty())
			{		
				propValMapAsStr +="(" + prop + " (";
				for (String propVal:propVals)				
					propValMapAsStr += "\"" + propVal + "\" ";
				if (propValMapAsStr.endsWith(" ")) propValMapAsStr = propValMapAsStr.substring(0, propValMapAsStr.length()-1) + ")) ";
			}
		}
		if (propValMapAsStr.endsWith(" ")) propValMapAsStr = propValMapAsStr.substring(0, propValMapAsStr.length()-1) + ") "; //closing thepropValMapAsStr = "("
		inputAsStr +=propValMapAsStr;

		// Processing the vector value template
		Set<String> props = propertiesValues.keySet(); //eVal and oVal
		Iterator propsIter = props.iterator();
		while (propsIter.hasNext()){
			String prop = (String) propsIter.next();
			Set<String> propVals= propertiesValues.get(prop);
			for (String propVal:propVals)
			{  
				if (!propertyValues_list.contains(propVal))
				{
					str += "@attribute " +  propVal + "{1,0}\n";
					propertyValues_list.add(propVal);
					valueListAsStr += "\"" + propVal + "\" ";
				}
			}			
		}
		if (valueListAsStr.endsWith("\" ")) valueListAsStr = valueListAsStr.substring(0, valueListAsStr.length()-1) + ") ";
		inputAsStr +=valueListAsStr;
		
		str += "\n @data \n";
		Iterator secKey_iter = sections.iterator();
		while (secKey_iter.hasNext())
		{
			String sectionAsStr = "(";
			String valueVectorAsStr = "(";		
			String objectListAsStr = "(";		

			String sec = (String) secKey_iter.next();
			sectionAsStr +=sec+ " ";

			//events
			Set<String> sec_events = sectionEvents.get(sec);
			if (!sec_events.isEmpty())
			{
				valueVectorAsStr +="(events ";
				Iterator te_iter = sec_events.iterator();
				while (te_iter.hasNext()){
					String te = (String) te_iter.next();
					List<String> eventData = new ArrayList<String>();
					//the first two are section ID and eventID
					eventData.add(sec);eventData.add(te);
					for (int i=2;i< propertyValues_list.size();i++)
						eventData.add("0"); //initialise the event value vector with all 0s

					Map<String,Set<String>> eventDataPairs = eventsPropertiesValues.get(te);

					Set<String> eDataKeys = eventDataPairs.keySet();
					Iterator eDataKeysIter = eDataKeys.iterator();
					while (eDataKeysIter.hasNext()){
						String eDataKey = (String) eDataKeysIter.next();
						Set<String> eData= eventDataPairs.get(eDataKey);
						Iterator eDataIter = eData.iterator();
						while (eDataIter.hasNext()){
							String eVal = (String) eDataIter.next();
							//		System.out.println(eDataKey+ "_" + eVal);
							if(propertyValues_list.contains(eVal)) 
							{
								//		  System.err.println("MATCHED" + eDataKey+ "_" + eVal);	
								eventData.set(propertyValues_list.indexOf(eVal), "1");
							}						
						}
					}
					for (int i=0;i<eventData.size()-1;i++)
						str +=eventData.get(i)+ ", ";
					str +=eventData.get(eventData.size()-1)+ "\n";

					valueVectorAsStr += "(" + eventData.get(1) + " (";
					for (int i=2;i<eventData.size()-1;i++)
						valueVectorAsStr +=eventData.get(i)+ " ";
					valueVectorAsStr +=eventData.get(eventData.size()-1)+ ")) ";
				}
				if (valueVectorAsStr.endsWith(" ")) valueVectorAsStr = valueVectorAsStr.substring(0,valueVectorAsStr.length()-1) + ") "; //closing (events
			} //end of if (!sec_events.isEmpty())
			//objects
			Set<String> sec_objects = sectionObjects.get(sec);
			if (!sec_objects.isEmpty())
			{			
				valueVectorAsStr +="(objects ";
				Iterator obj_iter = sec_objects.iterator();
				while (obj_iter.hasNext()){
					String object = (String) obj_iter.next();			

					List<String> objectData = new ArrayList<String>();
					//the first two are section ID and objectID
					objectData.add(sec);objectData.add(object);
					for (int i=2;i< propertyValues_list.size();i++)
						objectData.add("0"); //initialise the object value vectors with all 0s 

					Map<String,Set<String>> objectDataPairs = objectsPropertiesValues.get(object);

					Set<String> oDataKeys = objectDataPairs.keySet();
					Iterator oDataKeysIter = oDataKeys.iterator();
					while (oDataKeysIter.hasNext()){
						String oDataKey = (String) oDataKeysIter.next();
						Set<String> oData= objectDataPairs.get(oDataKey);
						Iterator oDataIter = oData.iterator();
						while (oDataIter.hasNext()){
							String oVal = (String) oDataIter.next();
							//		System.out.println(oDataKey+ "_" + oVal);
							if(propertyValues_list.contains(oVal)) //must do this to match values
							{
								//		  System.err.println("MATCHED" + oDataKey+ "_" + oVal);	
								objectData.set(propertyValues_list.indexOf(oVal), "1");
							}						
						}
					}
					for (int i=0;i<objectData.size()-1;i++)
						str +=objectData.get(i)+ ", ";
					str +=objectData.get(objectData.size()-1)+ "\n";

					valueVectorAsStr += "(" + objectData.get(1) + " (";
					for (int i=2;i<objectData.size()-1;i++)
						valueVectorAsStr +=objectData.get(i)+ " ";
					valueVectorAsStr +=objectData.get(objectData.size()-1)+ ")) ";
					objectListAsStr += objectData.get(1) + " ";				
				}			
				if (valueVectorAsStr.endsWith(" ")) valueVectorAsStr = valueVectorAsStr.substring(0,valueVectorAsStr.length()-1) + ") "; //closing (objects
			} 	//end of if (!sec_objects.isEmpty())
			if (valueVectorAsStr.endsWith(" ")) valueVectorAsStr = valueVectorAsStr.substring(0,valueVectorAsStr.length()-1) + ") "; //closing ( of valueVectorAsStr
			if (objectListAsStr.endsWith(" ")) objectListAsStr = objectListAsStr.substring(0,objectListAsStr.length()-1) + ")";
			if (objectListAsStr.endsWith("(")) objectListAsStr=""; //no objects at all
			
			sectionAsStr += valueVectorAsStr + objectListAsStr + ") ";	//closing ( of sectionAsStr		
//			if (sectionAsStr.endsWith(" (() ")) sectionAsStr =""; //ignore sections that have neither events or objects
			
			inputAsStr +=sectionAsStr;
		}
		if (inputAsStr.endsWith(" ")) inputAsStr = inputAsStr.substring(0, inputAsStr.length()-1) + ")";


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
		System.out.println(inputAsStr);
		System.err.println(count);
		return inputAsStr;

	}
	
	public static void main(String[] args) throws Exception{
		LispBridge lispATMS = LispBridge.getInstance();

		String dossID = "http://decipher-research.eu/dossiers/1313";//1533";
		String storID = "2443"; //"2527";
//		String storID = "3676"; //"2682";"2849"; "2933"; "3676"; //for dossier 1533
//		String queryURI = "http://localhost:8088/openrdf-sesame/repositories/RecommenderDemo?query=";
		String queryURI = "http://decipher.open.ac.uk/openrdf-sesame/repositories/Decipher?query=";
//		String queryURI = "http://storyscope5.ssl.co.uk:8080/openrdf-sesame/repositories/test?query=";
		NarrativeRecommender nr= new NarrativeRecommender(queryURI,dossID,storID, lispATMS);
//		nr.getCount();
		long start = System.currentTimeMillis();
		nr.recommendNarratives();
		long duration = System.currentTimeMillis() -start;
		System.out.println("the time for the whole process is: " + duration);

	}
	
}
