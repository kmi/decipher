package uk.ac.open.kmi.decipher.reasoner;

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

import org.apache.commons.lang.StringEscapeUtils;

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

public class PlotCluster {

	private static final int MAX_OS_NUM = 3;//maximum number of object stories per section;
	private static final int MAX_EVENT_NUM = 12;//maximum number of events per section;
	private static final String PLOT_ROOT_ELEMENT = "story_section";//"plot_element";
	private static final String PLOT_ID = "id";
	private static final String PLOT_TYPE = "type";
	private static final String PLOT_TITLE = "title";
	private static final String PLOT_DESC = "description";
	private static final String PLOT_EVENTS = "events";
	private static final String PLOT_EVENT = "event";
	private static final String PLOT_OBJECT_STORY = "object_story";
	private static final String PLOT_OBJECT_STORIES = "object_stories";
//	private static final String PLOT_CONSEQUENCE = "consequence_events";
    private static final String TEMP_DIR = File.separator + "data" + File.separator + "Decipher" + File.separator +"tmp"; 	
//	private static final String TEMP_DIR = File.separator + "Decipher" + File.separator +"tmp"; 
	   
	private String queryEndPoint;
	private String dossierID;
	private String cacheFileName;

	List<String> taggedEvents_list= new ArrayList<String>();

	public PlotCluster(String repQueryURI, String dossier) {
		this.queryEndPoint = repQueryURI;
		this.dossierID = dossier;
		this.cacheFileName = TEMP_DIR + File.separator + dossierID.substring(dossierID.lastIndexOf("/")+1) +"_recommendedRelatedPlots.xml";
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
	public String recommend(int clu_num){
		File cacheFile = new File(cacheFileName);
		if(!cacheFile.exists())
		{
			String respStr = clusterPlots(clu_num);
	    	FileUtil.write2File(respStr, cacheFileName);
			return respStr;
		} else
		{
			String respStr = FileUtil.readFile(cacheFileName);
			return respStr;
		}
	}
	private String getEventProperties (){
			String queryEventProperities = SPARQLUtil.PREFIX 
//					+ "SELECT ?event ?sTime ?eTime ?agent ?act ?style ?theme ?genre ?loc ?dim ?val ?obj ?material "
//		 		    + "WHERE {?event dul:satisfies <" + dossierID +">. "
					+ "SELECT DISTINCT ?event ?sTime ?eTime ?agent ?act ?style ?genre ?loc ?val ?obj "
		 		    + "WHERE {<" + dossierID +"> curate:containsEvent ?event. "
					+ "		 ?event curate:hasStartTime ?sTime."
					+ "			optional {?event curate:hasEndTime ?eTime.}"
					+ "			optional {?event curate:hasAgent ?agentFB. ?agentFB rdfs:label ?agent.}"
					+ "			optional {?event curate:hasActivity ?actFB. ?actFB rdfs:label ?act. }" 
					+ "			optional {?event curate:hasStyleMovement ?styleFB. ?styleFB rdfs:label ?style.} "
					+ "			optional {?event curate:hasGenre ?genreFB. ?genreFB rdfs:label ?genre.} "
					+ "			optional {?event curate:hasLocation ?locFB. ?locFB rdfs:label ?loc.} "
					+ "			optional {?event curate:hasObservedValue ?valFB. ?valFB rdfs:label ?val. } "
					+ "			optional {?event curate:includesPhysicalObject ?objFB. ?objFB rdfs:label ?obj.}" 
					+ "}";	 
			System.out.println(queryEventProperities);
			System.out.println(queryEndPoint);
		 SPARQLClient sClient = new SPARQLClient();
		 String results = sClient.httpQuery(queryEndPoint, queryEventProperities, "application/sparql-results+xml");
//		 System.out.println(results);
		 return results;
		}
	
	public String clusterPlots(int clusterNum){
		String relPlots = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><" + PLOT_ROOT_ELEMENT +"s>";
		relPlots += clusterAction(clusterNum);
		relPlots += "</" + PLOT_ROOT_ELEMENT +"s>";  		
    	FileUtil.write2File(relPlots, cacheFileName);
		return relPlots;

	}
	public String clusterAction(int clusterNum){
		
		String input = TEMP_DIR + File.separator + dossierID.substring(dossierID.lastIndexOf("/")+1) +"_input.arff";
//		String output = "file:///Decipher/Documents/cluster/event_output.arff";
//		File input_file = new File(input);
//		if (!input_file.exists())
//		{
			String sparqlResults = getEventProperties(); 
			generateArff(sparqlResults, input);		
//		}

		
		KMeansCluster clusterAlg = new KMeansCluster();
		clusterAlg.setNumberOfCluster(clusterNum);
		ClusterResult clusterResult = clusterAlg.cluster(input, null);
		int[] assignments = clusterResult.getAssignments();
		int numOfClusters = clusterResult.getNumberOfClusters();
//		System.out.println("\n" + numOfClusters + "\n");
		Instances centroids = clusterResult.getCentroids();

		String resultStr="";
		String[] plotStr = new String[numOfClusters];
		int[] yesPropNum = new int[numOfClusters];
		for (int i=0;i<plotStr.length;i++)
		{
			Set<String> object_story_set_per_section = new HashSet<String>(); //to avoid duplicate
			yesPropNum[i]=0;
			String objectStoryStr="<" + PLOT_OBJECT_STORIES +">";
			
			plotStr[i]="";			
			plotStr[i] +="<" + PLOT_ROOT_ELEMENT + ">";
			plotStr[i] +="<"+ PLOT_ID +">" + UUID.randomUUID().toString() + "</" + PLOT_ID +">";
			plotStr[i] +="<"+ PLOT_TYPE +">related</" + PLOT_TYPE +">";
            String titleStr = "<"+ PLOT_TITLE +">";
			String plotDesc = "<"+ PLOT_DESC +">This section is about ";//events related to ";			
			
//			Instance centroid = centroids.get(i); //weka-dev-3.7.5
			Instance centroid = centroids.instance(i); //weka-3.6.8
			String[] yesnos= centroid.toString().split(",");
			for (int iyn=0;iyn<yesnos.length;iyn++)
				if (yesnos[iyn].equalsIgnoreCase("yes"))	
				{
					//System.out.println("Ning test:" + centroid.attribute(i));
					String yesProp = centroid.attribute(iyn).name();
//					plotStr[i] += yesProp + ", ";
					String[] yesPropValues= yesProp.split("_");
					titleStr += yesPropValues[1] + ", ";
					plotDesc += yesPropValues[1] + ", "; //+ " (" + yesPropValues[0] +")" + ", ";
					yesPropNum[i]+=1;					
				}else if(!yesnos[iyn].equalsIgnoreCase("no")){ //for StartTime and EndTime
					String timeAttr = centroid.attribute(iyn).name();
//					plotStr[i] += timeAttr + "_" + yesnos[iyn] +", ";
					titleStr += yesnos[iyn] + ", ";
					plotDesc += yesnos[iyn] +", "; //+ " (" + timeAttr + ")" +", ";
					yesPropNum[i]+=1;
				}
			titleStr = titleStr.substring(0,titleStr.lastIndexOf(", "));
			plotDesc = plotDesc.substring(0,plotDesc.lastIndexOf(", ")) + ". ";
			String eventStr ="<" + PLOT_EVENTS +">";
			int eventCounter=0;
			for (int j=0;j<assignments.length; j++)
			{	
				if(assignments[j]==i)
				{
//					plotStr[i] += "&lt;" + taggedEvents_list.get(j) + "&gt; &#13;";
					eventStr += "<" + PLOT_EVENT + ">" + taggedEvents_list.get(j) + "</" + PLOT_EVENT +">";
					String tmpOSStr = getOSofEvent(taggedEvents_list.get(j), object_story_set_per_section);
					if (!objectStoryStr.contains(tmpOSStr))
					objectStoryStr += tmpOSStr; 
					eventCounter +=1;
				}
			}
			titleStr +="</" + PLOT_TITLE +">";
			eventStr +="</" + PLOT_EVENTS +">";
			objectStoryStr += "</" + PLOT_OBJECT_STORIES +">";
			int noOfEvents = eventStr.split("</"+ PLOT_EVENT +">").length -1;
			int noOfOSs = objectStoryStr.split("</"+ PLOT_OBJECT_STORY + ">").length -1;
			plotDesc += "It contains " + noOfEvents + " events and " + noOfOSs +" object stories";	
			plotDesc +="</" + PLOT_DESC +">";
			plotStr[i] += titleStr + plotDesc + eventStr + objectStoryStr;
			plotStr[i] +="</" +  PLOT_ROOT_ELEMENT + ">";
			
			// remove the cluster that has more than 5 object stories only
			if (noOfOSs> MAX_OS_NUM || noOfEvents > MAX_EVENT_NUM)
			 plotStr[i] =""; 						
			// remove the cluster that has one event only
			if (eventCounter<=1)
			 plotStr[i] =""; 
		}
		//Ordered according to the number of centroid values
		int temp=0; String tempStr="";
		for (int i=0;i<yesPropNum.length;i++)
			for (int j=1;j<yesPropNum.length-i;j++)
			if(yesPropNum[j] > yesPropNum[j-1]){
				temp=yesPropNum[j]; tempStr=plotStr[j];
				yesPropNum[j]=yesPropNum[j-1]; plotStr[j] = plotStr[j-1];
				yesPropNum[j-1]=temp; plotStr[j-1] =tempStr;
			}
		
		for (int i=0;i<plotStr.length;i++)
			resultStr +=plotStr[i];			

		return resultStr;

	}
	
	private String getOSofEvent(String eventID, Set<String> osSet) {
		String resultStr="";
	   /*	
		String queryEventOS = SPARQLUtil.PREFIX 
				+ "SELECT ?object_story "
				+ "WHERE { <" + dossierID +"> curate:containsObjectStory ?object_story." 
				+ " ?object_story curate:containsEvent <" + eventID +">. "
				+ "}";	 
		*/
		String queryEventOS = SPARQLUtil.PREFIX
				+ "SELECT ?object_story WHERE {" 
			    + "<" + dossierID +"> curate:containsObjectStory ?object_story." 
			    + "<" + dossierID +"> curate:containsEvent <" + eventID +">. "
		        + "<" + eventID +"> curate:isInterpretationOfEvent ?source." 
			    + "?event1 curate:isInterpretationOfEvent ?source." 
			    + "?object_story curate:containsEvent ?event1. " 
			    + "}"; 
		SPARQLClient sClient = new SPARQLClient();
		String results = sClient.httpQuery(queryEndPoint, queryEventOS, "application/sparql-results+xml");
		QueryResult qr = SPARQLQueryResultParser.parse(results);	
		List<QueryRow> rows = qr.getQueryRows();
		Iterator<QueryRow> rows_iter = rows.iterator();
		while (rows_iter.hasNext())
		{
			QueryRow row = rows_iter.next();
			if (row.getValue("object_story") !=null)
			{	
				String osValue = row.getValue("object_story");
				if (!osSet.contains(osValue))
				{  
					resultStr +="<" + PLOT_OBJECT_STORY +">";
					//	resultStr +="&lt;"+ row.getValue("object_story") + "&gt; &#13;";
					resultStr += osValue;
					osSet.add(osValue);
					resultStr +="</" + PLOT_OBJECT_STORY +">";
				}
			}
		}
		return resultStr;
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
        
		//all the event of one dossier
		Set<String> taggedEvents = new HashSet<String>();
		
		Map<String, Set<String>> propertiesValues = new HashMap<String,Set<String>>(); //for every single property, property is the key
		Map<String, Map<String,Set<String>>> eventsPropertiesValues = new HashMap<String, Map<String,Set<String>>>(); //for every event, event is the key

		QueryResult qr = SPARQLQueryResultParser.parse(sparqlResult);	
		List<QueryRow> rows = qr.getQueryRows();
		Iterator<QueryRow> rows_iter = rows.iterator();
		boolean FIRST_EVENT = true; String prev_event=null;
		while (rows_iter.hasNext()){
			QueryRow row = rows_iter.next();
			
			System.out.println(row.getValue("event"));

			if (!FIRST_EVENT && !taggedEvents.contains(row.getValue("event")))
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
			    String temp = tmp_sTime.substring(0,tmp_sTime.indexOf("-"));
				sTime.add(temp); e_sTime.add(temp);
			}  
			if (row.getValue("eTime") !=null && !row.getValue("eTime").contains("freebase.com")) 
			{
				String tmp_eTime=row.getValue("eTime");
				String temp = tmp_eTime.substring(0,tmp_eTime.indexOf("-"));
				eTime.add(temp); e_eTime.add(temp); 
			}
			if (row.getValue("agent") !=null && !row.getValue("agent").contains("freebase.com")) {String temp = row.getValue("agent").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				agent.add(row.getValue("agent")); e_agent.add(row.getValue("agent")); }  
			if (row.getValue("act") !=null && !row.getValue("act").contains("freebase.com")) {String temp = row.getValue("act").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
			    act.add(row.getValue("act")); e_act.add(row.getValue("act")); } 
			if (row.getValue("style") !=null && !row.getValue("style").contains("freebase.com")) {String temp = row.getValue("style").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				style.add(row.getValue("style")); e_style.add(row.getValue("style"));}  
	//		if (row.getValue("theme") !=null ) {String temp = row.getValue("theme").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
	//		   theme.add(temp); e_theme.add(row.getValue("theme"));} //System.out.println ("theme: " + row.getValue("theme")); }
			if (row.getValue("genre") !=null && !row.getValue("genre").contains("freebase.com")) {String temp = row.getValue("genre").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
			   genre.add(row.getValue("genre")); e_genre.add(row.getValue("genre")); }  
			if (row.getValue("loc") !=null && !row.getValue("loc").contains("freebase.com")) {String temp = row.getValue("loc").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				loc.add(row.getValue("loc")); e_loc.add(row.getValue("loc")); }  
	//		if (row.getValue("dim") !=null ) {String temp = row.getValue("dim").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
	//		   dim.add(temp); e_dim.add(row.getValue("dim")); }  
			if (row.getValue("val") !=null && !row.getValue("val").contains("freebase.com")) {String temp = row.getValue("val").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				val.add(row.getValue("val")); e_val.add(row.getValue("val")); }
			if (row.getValue("obj") !=null && !row.getValue("obj").contains("freebase.com")) {String temp = row.getValue("obj").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				obj.add(row.getValue("obj")); e_obj.add(row.getValue("obj")); }  
	//		if (row.getValue("material") !=null ) {String temp = row.getValue("material").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
	//			material.add(temp); e_material.add(row.getValue("material")); } //System.out.println ("materail: " + row.getValue("material")); }

			taggedEvents.add(row.getValue("event"));
			prev_event = row.getValue("event");
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
//		str += "@attribute start_date numeric\n";
//		str += "@attribute end_date numeric\n";

		String othersStr="", sTimeStr="", eTimeStr="";

		List<String> propertyValues_list = new ArrayList<String>(); 
		
		Set<String> props = propertiesValues.keySet();
		Iterator propsIter = props.iterator();
		while (propsIter.hasNext()){
			String prop = (String) propsIter.next();
			Set<String> propVals= propertiesValues.get(prop);
			
			if (prop.equalsIgnoreCase("sTime"))
			{ sTimeStr = "@attribute start_date {";
			  propertyValues_list.add(0, prop);			
			  for (String propVal:propVals)
			  {  
				  sTimeStr += propVal + ",";
			   }
			  sTimeStr += "}\n";
			 }
			else if (prop.equalsIgnoreCase("eTime"))
			{    eTimeStr = "@attribute end_date {";
			    propertyValues_list.add(1, prop);
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
					othersStr += "@attribute \"" + prop +"_" + propVal + "\"{yes,no}\n";
					propertyValues_list.add(prop +"_" + propVal);
				}	
			}
		}

		   str += sTimeStr + eTimeStr + othersStr;
		   str += "\n @data \n";
		
		Iterator te_iter = taggedEvents.iterator();
		while (te_iter.hasNext()){
			String te = (String) te_iter.next();
//			System.err.println(te);
			
			taggedEvents_list.add(te); //keep track of the order for later use;
			
		/*	Random random = new Random();
			List<Integer> eventTime = new ArrayList<Integer>();
			eventTime.add(random.nextInt(10000)); eventTime.add(random.nextInt(10000));
		*/	
			List<String> eventData = new ArrayList<String>();
			eventData.add("?");eventData.add("?");
			for (int i=2;i< propertyValues_list.size();i++)
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
			/*			if(propertyValues_list.contains(eDataKey+ "_" + eVal.replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "")))
						{
							eventData.set(propertyValues_list.indexOf(eDataKey+ "_" + eVal.replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "")), "yes");
						}	*/					
						if(propertyValues_list.contains(eDataKey+ "_" + eVal))
						{
							eventData.set(propertyValues_list.indexOf(eDataKey+ "_" + eVal), "yes");
						}						
					}
				}
			}
			for (int i=0;i<eventData.size()-1;i++)
				str +=eventData.get(i)+ ", ";
			str +=eventData.get(eventData.size()-1)+ "\n";
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
//		String queryURI = "http://storyscope5.ssl.co.uk:8080/openrdf-sesame/repositories/test?query=";
		String queryURI = "http://decipher.open.ac.uk/openrdf-sesame/repositories/Decipher?query=";
		PlotCluster pCluster= new PlotCluster(queryURI, dossier);
		pCluster.clusterPlots(25);
	}	
}
