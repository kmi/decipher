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
import java.util.Set;

import weka.core.Instance;
import weka.core.Instances;

import uk.ac.open.kmi.decipher.cluster.ClusterResult;
import uk.ac.open.kmi.decipher.cluster.IAlgorithm;
import uk.ac.open.kmi.decipher.cluster.KMeansCluster;

import uk.ac.open.kmi.decipher.repository.RDFRepositoryConnector;
import uk.ac.open.kmi.decipher.repository.SPARQLClient;
import uk.ac.open.kmi.decipher.util.QueryResult;
import uk.ac.open.kmi.decipher.util.QueryRow;
import uk.ac.open.kmi.decipher.util.SPARQLQueryResultParser;
import uk.ac.open.kmi.decipher.util.SPARQLUtil;


public class PlotEventCluster {

	private static final String TEMP_DIR = File.separator + "data" + File.separator + "Decipher" + File.separator +"tmp"; 
//	private static final String TEMP_DIR = File.separator + "Decipher" + File.separator +"tmp"; 

	String queryEndPoint;
	String dossierID;
	String plotID;  //Story Outline ID
	
    Map<String,Set<String>> setting = new HashMap<String,Set<String>>();

	//List<String> propertyValues_list = new ArrayList<String>(); 

	public PlotEventCluster(String repQueryURI, String dossier, String plot) {
		this.queryEndPoint = repQueryURI;
		this.dossierID = dossier;
		this.plotID = plot;
        setting.put("startTime", null);
	    setting.put("endTime", null);
	    setting.put("location", null);
	}
		
	private String getEventTime(){
	
		String query = SPARQLUtil.PREFIX 
/*				+ "SELECT DISTINCT ?event ?sTime ?eTime "
				+ " WHERE {<" + plotID + "> curate:containsPlotDescription ?plotDesc. "
				+ "	?plotDesc  curate:definesEventSituationType ?eveSituationType. "
				+ " ?eveSituationType curate:classifiesEventSituation ?event."
				+ " ?event curate:hasStartTime ?sTime. "
				+ " optional {?event curate:hasEndTime ?eTime.}"
				+ "}";
*/
				+ "SELECT DISTINCT ?event ?sTime ?eTime "
				+ " WHERE {<" + plotID + "> curate:containsStorySection ?sSection. "
				+ "	?sSection  curate:hasRelatedEvent ?event. "
				+ " ?event curate:hasStartTime ?sTime. "
				+ " optional {?event curate:hasEndTime ?eTime.}"
				+ "}";
		return queryRDF(query);		
	}
	
	private String getEventLocation (String time){
		  	 
		 String query = SPARQLUtil.PREFIX 
					+ "SELECT ?event ?loc "
					+ " WHERE {<" + plotID + "> curate:containsStorySection ?sSection. "
					+ "	?sSection  curate:hasRelatedEvent ?event. "
					+ "?event curate:hasStartTime ?sTime. "
//					+ "optional {?event curate:hasEndTime ?eTime.} "
					+ "optional {?event curate:hasLocation ?loc.} "
					+ "FILTER regex(STR(?sTime), \""+ time + "\", \"i\")"
					+ "}";	
		 System.err.println(query);
		  return queryRDF(query);
		}
/*	private String getAll(){
		String queryEventProperities = SPARQLUtil.PREFIX 
				+ "SELECT ?event ?sTime ?eTime ?agent ?act ?style ?theme ?genre ?loc ?dim ?val ?obj ?material"
				+ " WHERE {<" + plotID + "> curate:containsPlotDescription ?plotDesc. "
				+ "	?plotDesc  curate:definesEventSituationType ?eveSituationType. "
				+ " ?eveSituationType curate:classifiesEventSituation ?event."
				+ "?event curate:hasStartTime ?sTime. "
				+ "optional {?event curate:hasEndTime ?eTime.}"
				+ "optional {?event curate:hasAgent ?agent. }"
				+ "optional {?event curate:hasActivity ?act. }" 
				+ "optional {?event curate:hasStyleOrMovement ?style.} "
				+ "optional {?event curate:hasTheme ?theme.} "
				+ "optional {?event curate:hasGenre ?genre.} "
				+ "optional {?event curate:hasLocation ?loc.} "
				+ "optional {?event curate:hasObservedDimension ?dim.} "
				+ "optional {?event curate:hasObservedValue ?val.} "
				+ "optional {?event curate:includesObject ?obj.} "
				+ "optional {?event curate:usesMaterial ?material.} "
				+ "}";	 	
	 String results = queryRDF(queryEventProperities);
	 return results;
	}
*/	
	private String queryRDF(String queryStr){		 
	 SPARQLClient sClient = new SPARQLClient();
	 String results = sClient.httpQuery(queryEndPoint, queryStr, "application/sparql-results+xml");
	 return results;
}

	public Map<String, Set<String>> clusterEvents(int clusterNum){

		String inputAll = TEMP_DIR + File.separator + dossierID.substring(dossierID.lastIndexOf("/")+1)+"-"+ plotID.substring(plotID.lastIndexOf("/")+1)+"_input.arff";
		String inputTimeLoc = TEMP_DIR + File.separator + dossierID.substring(dossierID.lastIndexOf("/")+1)+"-" + plotID.substring(plotID.lastIndexOf("/")+1) +"_time_loc.arff";
		String inputLocTime = TEMP_DIR + File.separator + dossierID.substring(dossierID.lastIndexOf("/")+1)+"-" + plotID.substring(plotID.lastIndexOf("/")+1) +"_loc_time.arff";
		//		String output = "file:///Decipher/Documents/cluster/event_output.arff";
		Set<String> locations = new HashSet<String>();
		Set<String> times = new HashSet<String>();
		
    	KMeansCluster clusterAlg = new KMeansCluster();

	/*	File input_file = new File(input);
		if (!input_file.exists())
		{
	*/	
		String sparqlResults = getEventTime(); 
	    if(generateTimeLocationArff(sparqlResults, inputTimeLoc))
		{
	    	clusterAlg.setNumberOfCluster(clusterNum);
	    	//		ClusterResult clu_res = clusterAlg.cluster(input, null);
	    	ClusterResult clu_res = clusterAlg.cluster(inputTimeLoc, null);
	    	int[] cluster_size = clu_res.getClusterSize();
	    	int[] clusterSizeCopy = new int[cluster_size.length];
        	for (int i=0;i<cluster_size.length;i++)
        		clusterSizeCopy[i]=cluster_size[i];
         
        	//ordered according to the size of the cluster
	    	int temp=0;
			for (int i=0;i<cluster_size.length;i++)
				for (int j=1;j<cluster_size.length-i;j++)
				if(cluster_size[j] > cluster_size[j-1]){
					temp=cluster_size[j]; 
					cluster_size[j]=cluster_size[j-1]; 
					cluster_size[j-1]=temp; 
				}
			//find the index of the biggest cluter in order to get its centroid
			int index=0;			 
			for (int i=0;i<clusterSizeCopy.length;i++)
				if (clusterSizeCopy[i]==cluster_size[0])
				index=i;					 
	    	
	    	Instances centroids = clu_res.getCentroids();
	    	
	    	int counter =0;
	    	Instance centroid = centroids.instance(index);
//	    	for (Instance centroid:centroids)
//	    	{   
	    		String[] yesnos= centroid.toString().split(",");
	    		for (int i=0;i<yesnos.length;i++)
	    			if (yesnos[i].equalsIgnoreCase("yes"))	
	    			{
	    				//System.out.println("Ning test:" + centroid.attribute(i));
	    				String yes = centroid.attribute(i).name();
	    				String[] namevalue = yes.split("_");
	    				String prop_name = namevalue[0];
	    				String prop_value = namevalue[1];
	    				System.err.println("centroid" + counter + ":" + prop_name + ":" + prop_value);			                    					
	    			}
	    			else if(!yesnos[i].equalsIgnoreCase("no")){
	    				String timeAttr = centroid.attribute(i).name();
	    				System.err.println("centroid" + counter + ":" + timeAttr + ":" + yesnos[i]);
	    				if(timeAttr.equalsIgnoreCase("start_date")) times.add(yesnos[i]);
	    			}
	    		counter++;			
	    	//}
		}
	    for (String time:times)
	    {	
	    	System.err.println("using this " + time + "to check location ");
	    	String sparqlResults2 = getEventLocation(time); 
	    	if (generateTimeLocationArff(sparqlResults2, inputLocTime))
	    	{
//	    		int[] ignoredAttrs2 = new int[]{0,1};
	    		ClusterResult clu_res2 = clusterAlg.cluster(inputLocTime, null);
	    		//		ClusterResult clu_res2 = clusterAlg.clusterIgnoreAttributes(inputLocTime, null, null,ignoredAttrs2);
	    		
	        	int[] cluster_size = clu_res2.getClusterSize();
	        	int[] clusterSizeCopy = new int[cluster_size.length];
	        	for (int i=0;i<cluster_size.length;i++)
	        		clusterSizeCopy[i]=cluster_size[i];
                int temp=0;
				for (int i=0;i<cluster_size.length;i++)
					for (int j=1;j<cluster_size.length-i;j++)
					if(cluster_size[j] > cluster_size[j-1]){
						temp=cluster_size[j]; 
						cluster_size[j]=cluster_size[j-1]; 
						cluster_size[j-1]=temp; 
					}
				
				int index=0;				
				for (int i=0;i<clusterSizeCopy.length;i++)
					if (clusterSizeCopy[i]==cluster_size[0])
					index=i;	
	    		    		
	    		Instances centroids2 = clu_res2.getCentroids();
	    		int counter =0;
		    	Instance centroid = centroids2.instance(index);

	 //   		for (Instance centroid:centroids2)
	 //   		{   
	    			String[] yesnos= centroid.toString().split(",");
	    			for (int i=0;i<yesnos.length;i++)
	    				if (yesnos[i].equalsIgnoreCase("yes"))	
	    				{
	    					//System.out.println("Ning test:" + centroid.attribute(i));
	    					String yes = centroid.attribute(i).name();
	    					String[] namevalue = yes.split("_");
	    					String prop_name = namevalue[0];
	    					String prop_value = namevalue[1];
	    					System.err.println("centroid" + counter + ":" + prop_name + ":" + prop_value);
	    					if (prop_name.equalsIgnoreCase("loc")) locations.add(prop_value);
	    				}
	    			/*	else if(!yesnos[i].equalsIgnoreCase("no")){
	    					String timeAttr = centroid.attribute(i).name();
	    					System.err.println("centroid" + counter + ":" + timeAttr + ":" + yesnos[i]);
	    					if (timeAttr.equalsIgnoreCase("start_date"))
	    						times.add(yesnos[i]);
	    				}*/
	    			counter++;			
	    	//	}
	    	}
	    }
		
		setting.put("loc", locations);
		setting.put("time", times);
		
		return setting;

	}
	
	public Map<String, Set<String>> getClusterSettings() {
        return setting;		
	}

	
	public String getSettinginHTTPRepsonse(){
		Map<String,Set<String>> setting = getClusterSettings();
		String response = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?> <results>\n";
		String[] subStr = new String[setting.size()];
		Object[] keys_array = setting.keySet().toArray();
		for (int i=0;i<subStr.length;i++)
		{   System.out.println(keys_array[i]);
			Set<String> values = setting.get(keys_array[i]);
			if (values !=null){
				Iterator<String> values_iter = values.iterator();
				while (values_iter.hasNext())
				{	
					subStr[i] +="<" + keys_array[i].toString() + ">";				
					subStr[i] += values_iter.next();
					subStr[i] +="</" + keys_array[i].toString() + ">";
				}
			}
		}
		for (int i=0;i<subStr.length;i++)
			response +=subStr[i];
		response += "</results>";
		
		return response;
	}
	public boolean generateTimeLocationArff(String sparqlResult, String input){
		//for all the values of each property (across all events)
		Set<String> sTime=new HashSet<String>(), eTime=new HashSet<String>(), loc=new HashSet<String>();				
		//for all the values of each property of one event
		Set<String> e_sTime=new HashSet<String>(), e_eTime=new HashSet<String>(), e_loc=new HashSet<String>();
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
				pairs.put("loc", new HashSet<String>(e_loc));
				eventsPropertiesValues.put(prev_event, new HashMap<String,Set<String>>(pairs));
				e_sTime.clear(); e_eTime.clear(); e_loc.clear(); 
				pairs.clear();
			}	
			FIRST_EVENT = false;

			if (row.getValue("sTime") !=null ) { String temp = row.getValue("sTime").substring(0,4).replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
			   sTime.add(temp); e_sTime.add(row.getValue("sTime").substring(0,4));} //System.out.println ("sTime: " + row.getValue("sTime")); 
			if (row.getValue("eTime") !=null ) {String temp = row.getValue("eTime").substring(0,4).replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				eTime.add(temp); e_eTime.add(row.getValue("eTime").substring(0,4)); }//System.out.println ("eTime: " + row.getValue("eTime")); }
			if (row.getValue("loc") !=null ) {String temp = row.getValue("loc").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				loc.add(temp); e_loc.add(row.getValue("loc")); } //System.out.println("loc: " + row.getValue("loc"));} 
			
			taggedEvents.add(row.getValue("event"));
			prev_event = row.getValue("event");
		}		
		// for the last event		
		pairs.put("sTime", new HashSet<String>(e_sTime)); 
		pairs.put("eTime", new HashSet<String>(e_eTime));
		pairs.put("loc", new HashSet<String>(e_loc));
		eventsPropertiesValues.put(prev_event, new HashMap<String,Set<String>>(pairs));
			
		propertiesValues.put("sTime", sTime); 
		propertiesValues.put("eTime", eTime);
		propertiesValues.put("loc", loc);
		
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
			{ if (!propVals.isEmpty())
			{sTimeStr = "@attribute start_date {";
			propertyValues_list.add(0, prop);			
			for (String propVal:propVals)
			{  
				sTimeStr += propVal + ",";
			}
			sTimeStr += "}\n";
			}
			}
			else if (prop.equalsIgnoreCase("eTime"))
			{  if( !propVals.isEmpty()) 
			{
				eTimeStr = "@attribute end_date {";
			    propertyValues_list.add(1, prop);
			    for (String propVal:propVals)
				{  
					eTimeStr += propVal + ",";
					
				}
				eTimeStr += "}\n";
				}
			}
			else //if (! prop.equalsIgnoreCase("sTime") && !prop.equalsIgnoreCase("eTime"))
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
		String dataStr="";
		List<String> taggedEvents_list = new ArrayList<String>(taggedEvents);
		
		Iterator te_iter = taggedEvents_list.iterator();
		while (te_iter.hasNext()){
			String te = (String) te_iter.next();
//			System.err.println(te);
			
			List<String> eventData = new ArrayList<String>();
			
			///BUG here, just for fixing arff with both time and location did not work
			if(propertyValues_list.contains("sTime") &&propertyValues_list.contains("eTime")) 
			{	eventData.add("?"); eventData.add("?");
			   for (int i=2;i< propertyValues_list.size();i++)
				eventData.add("no");
			}
			else
			{for (int i=0;i< propertyValues_list.size();i++)
				eventData.add("no");}
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
				else
				{ 
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
				dataStr +=eventData.get(i)+ ", ";
			dataStr +=eventData.get(eventData.size()-1)+ "\n";
		}
		if(dataStr.isEmpty()) return false;
		else{
			str +=dataStr;	

			String fn= input;
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
			return true;
		}
	}
/*	
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
				pairs.put("theme", new HashSet<String>(e_theme));
				pairs.put("genre", new HashSet<String>(e_genre));
				pairs.put("loc", new HashSet<String>(e_loc));
				pairs.put("dim", new HashSet<String>(e_dim));
				pairs.put("val", new HashSet<String>(e_val));
				pairs.put("obj", new HashSet<String>(e_obj));
				pairs.put("material", new HashSet<String>(e_material));
				eventsPropertiesValues.put(prev_event, new HashMap<String,Set<String>>(pairs));
				e_sTime.clear(); e_eTime.clear(); e_agent.clear(); e_act.clear();e_style.clear();e_theme.clear();e_genre.clear();
				e_loc.clear(); e_dim.clear();e_val.clear();e_obj.clear();e_material.clear();
				pairs.clear();
			}	
			FIRST_EVENT = false;

			if (row.getValue("sTime") !=null ) { String temp = row.getValue("sTime").substring(0,4).replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
			   sTime.add(temp); e_sTime.add(row.getValue("sTime").substring(0,4));} //System.out.println ("sTime: " + row.getValue("sTime")); 
			if (row.getValue("eTime") !=null ) {String temp = row.getValue("eTime").substring(0,4).replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				eTime.add(temp); e_eTime.add(row.getValue("eTime").substring(0,4)); }//System.out.println ("eTime: " + row.getValue("eTime")); }
			if (row.getValue("agent") !=null ) {String temp = row.getValue("agent").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				agent.add(temp); e_agent.add(row.getValue("agent")); } //System.out.println ("agent: " + row.getValue("agent"));} 
			if (row.getValue("act") !=null ) {String temp = row.getValue("act").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
			    act.add(temp); e_act.add(row.getValue("act")); } //System.out.println ("act: " + row.getValue("act")); }
			if (row.getValue("style") !=null ) {String temp = row.getValue("style").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				style.add(temp); e_style.add(row.getValue("style"));}  //System.out.println ("style: " + row.getValue("style")); }
			if (row.getValue("theme") !=null ) {String temp = row.getValue("theme").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
			   theme.add(temp); e_theme.add(row.getValue("theme"));} //System.out.println ("theme: " + row.getValue("theme")); }
			if (row.getValue("genre") !=null ) {String temp = row.getValue("genre").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
			   genre.add(temp); e_genre.add(row.getValue("genre")); } //System.out.println ("genre: " + row.getValue("genre")); } 
			if (row.getValue("loc") !=null ) {String temp = row.getValue("loc").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				loc.add(temp); e_loc.add(row.getValue("loc")); } //System.out.println("loc: " + row.getValue("loc"));} 
			if (row.getValue("dim") !=null ) {String temp = row.getValue("dim").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
			   dim.add(temp); e_dim.add(row.getValue("dim")); } //System.out.println("dim: " + row.getValue("dim"));} 
			if (row.getValue("val") !=null ) {String temp = row.getValue("val").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				val.add(temp); e_val.add(row.getValue("val")); }//System.out.println("val: " + row.getValue("val")); }
			if (row.getValue("obj") !=null ) {String temp = row.getValue("obj").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				obj.add(temp); e_obj.add(row.getValue("obj")); } //System.out.println("obj: " + row.getValue("obj"));} 
			if (row.getValue("material") !=null ) {String temp = row.getValue("material").replaceAll(" ", "").replaceAll("[^A-Za-z0-9]", "");
				material.add(temp); e_material.add(row.getValue("material")); } //System.out.println ("materail: " + row.getValue("material")); }

			taggedEvents.add(row.getValue("event"));
			prev_event = row.getValue("event");
		}		
		// for the last event		
		pairs.put("sTime", new HashSet<String>(e_sTime)); 
		pairs.put("eTime", new HashSet<String>(e_eTime));
		pairs.put("agent", new HashSet<String>(e_agent));
		pairs.put("act", new HashSet<String>(e_act));
		pairs.put("style", new HashSet<String>(e_style));
		pairs.put("theme", new HashSet<String>(e_theme));
		pairs.put("genre", new HashSet<String>(e_genre));
		pairs.put("loc", new HashSet<String>(e_loc));
		pairs.put("dim", new HashSet<String>(e_dim));
		pairs.put("val", new HashSet<String>(e_val));
		pairs.put("obj", new HashSet<String>(e_obj));
		pairs.put("material", new HashSet<String>(e_material));
		eventsPropertiesValues.put(prev_event, new HashMap<String,Set<String>>(pairs));
			
		propertiesValues.put("sTime", sTime); 
		propertiesValues.put("eTime", eTime);
		propertiesValues.put("agent", agent);
		propertiesValues.put("act", act);
		propertiesValues.put("style", style);
		propertiesValues.put("theme",theme);
		propertiesValues.put("genre", genre);
		propertiesValues.put("loc", loc);
		propertiesValues.put("dim", dim);
		propertiesValues.put("val", val);
		propertiesValues.put("obj", obj);
		propertiesValues.put("material", material);

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
			else //if (! prop.equalsIgnoreCase("sTime") && !prop.equalsIgnoreCase("eTime"))
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
		List<String> taggedEvents_list = new ArrayList<String>(taggedEvents);
		
		Iterator te_iter = taggedEvents_list.iterator();
		while (te_iter.hasNext()){
			String te = (String) te_iter.next();
//			System.err.println(te);
			
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
				else
				{ 
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
*/
	public static void main(String[] args) throws Exception{
		String dossier = "http://mckm219286.open.ac.uk/decipher/trunk/decipher-3/cstory/2727";
		String queryURI = "http://localhost:8080/openrdf-sesame/repositories/Decipher?query=";
		String plot = "http://mckm219286.open.ac.uk/decipher/trunk/decipher-3/plot/3820";
		PlotEventCluster eCluster= new PlotEventCluster(queryURI, dossier,plot);
		eCluster.clusterEvents(3);
	}	
}
