package uk.ac.open.kmi.decipher.reasoner;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringEscapeUtils;

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



public class EventClusterLocTime {

//	private enum Facets {sTime, eTime, agent, act, style, theme, genre, loc, dim, val, obj, material};
//	private RDFRepositoryConnector RDF_rep_conn;
	String queryEndPoint;
	String dossierID;
	static private final String PROP_VAL_CONNECTOR="::";
	static private final int SETTING_NUM=3;
 //   Map<String,List<String>> setting = new HashMap<String,List<String>>();

	//List<String> propertyValues_list = new ArrayList<String>(); 

	public EventClusterLocTime(String repQueryURI, String dossier) {
		this.queryEndPoint = repQueryURI;
		this.dossierID = dossier;
 	}
		
	private String getEventTime(Set<String> locations){
		String query = SPARQLUtil.PREFIX 
				+ "SELECT DISTINCT ?event ?sTime ?eTime "
//	 		    + "WHERE {?event dul:satisfies <" + dossierID +">. "
				+ "WHERE {<" + dossierID +"> curate:containsEvent ?event. "
				+ "?event curate:hasStartTime ?sTime. "
				+ "optional {?event curate:hasEndTime ?eTime.}"
//				+ "FILTER regex(STR(?sTime), \"^"+ time + "\", \"i\")"
				+ "}";
		return queryRDF(query);		
	}
	
	private String getEventLocation (String time){		  	 
		 String query = SPARQLUtil.PREFIX 
					+ "SELECT ?event ?loc ?loc_FBL "
//					+ "WHERE {?event dul:satisfies <" + dossierID +">. "
					+ "WHERE {<" + dossierID +"> curate:containsEvent ?event. "
					+ "?event curate:hasStartTime ?sTime. "
					+ "optional {?event curate:hasLocation ?loc. "
					+ "			optional {?loc rdfs:label ?loc_FBL.}} "
					+ "}";	
		 System.err.println(query);
		  return queryRDF(query);
		}

	private String queryRDF(String queryStr){		 
	 SPARQLClient sClient = new SPARQLClient();
	 String results = sClient.httpQuery(queryEndPoint, queryStr, "application/sparql-results+xml");
	 return results;
	}

	public Map<String, Set<String>> clusterEvents(int clusterNum){

//		String inputAll = File.separator + "Decipher" + File.separator +"tmp" + File.separator + dossierID.substring(dossierID.lastIndexOf("/")+1) +"_input.arff";
		String inputTimeLoc = File.separator + "Decipher" + File.separator +"tmp" + File.separator + dossierID.substring(dossierID.lastIndexOf("/")+1) +"_time_loc.arff";
		String inputLocTime = File.separator + "Decipher" + File.separator +"tmp" + File.separator + dossierID.substring(dossierID.lastIndexOf("/")+1) +"_loc_time.arff";

		Map<String,Set<String>> setting = new HashMap<String,Set<String>>();

		Set<String> locations = new HashSet<String>();
		Set<String> times = new HashSet<String>();
		
    	KMeansCluster clusterAlg = new KMeansCluster();

		String sparqlResults = getEventTime(null); 
	    if(generateTimeLocationArff(sparqlResults, inputTimeLoc))
		{
	    	clusterAlg.setNumberOfCluster(clusterNum);
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
			int K=cluster_size.length>SETTING_NUM ? SETTING_NUM:cluster_size.length;
			int[] index=new int[K];

			for (int j=0;j<K;j++)
				for (int i=0;i<clusterSizeCopy.length;i++)
				{	
					if (clusterSizeCopy[i]==cluster_size[j])
						index[j]=i;					 					
				}
	    	Instances centroids = clu_res.getCentroids();
	    	for (int k=0;k<index.length;k++)
	    	{	
	    		Instance centroid = centroids.instance(index[k]);
	    		String[] yesnos= centroid.toString().split(",");
	    		for (int i=0;i<yesnos.length;i++)
	    			if (yesnos[i].equalsIgnoreCase("yes"))	
	    			{
	    				//System.out.println("Ning test:" + centroid.attribute(i));
	    				String yes = centroid.attribute(i).name();
	    				String[] namevalue = yes.split(PROP_VAL_CONNECTOR);
	    				String prop_name = namevalue[0];
	    				String prop_value = namevalue[1];
	    				System.err.println("centroid" + index[k] + ":" + prop_name + ":" + prop_value);			                    					
	    			}
	    			else if(!yesnos[i].equalsIgnoreCase("no")) //special case for time
	    			{
	    				String timeAttr = centroid.attribute(i).name();
	    				System.err.println("centroid" + index[k] + ":" + timeAttr + ":" + yesnos[i]);
	    				if(timeAttr.equalsIgnoreCase("start_date")) 
	    					times.add(yesnos[i]);
	    			}
	    	}

		}
	    int count=0;
	    for (String time:times)
	    {	count++;
	    	System.err.println("using this " + time + "to check location ");
	    	String sparqlResults2 = getEventLocation(time); 
	    	if (generateTimeLocationArff(sparqlResults2, inputLocTime))
	    	{
//	    		int[] ignoredAttrs2 = new int[]{0,1};
	    		ClusterResult clu_res2 = clusterAlg.cluster(inputLocTime, null);
//				ClusterResult clu_res2 = clusterAlg.clusterIgnoreAttributes(inputLocTime, null, null,ignoredAttrs2);
	    		
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
				/*int index=0;
				 for (int i=0;i<clusterSizeCopy.length;i++)
					if (clusterSizeCopy[i]==cluster_size[0])
					index=i;	
				*/ 
				int K=cluster_size.length>SETTING_NUM ? SETTING_NUM:cluster_size.length;
				int[] index=new int[K];

				for (int j=0;j<K;j++)
					for (int i=0;i<clusterSizeCopy.length;i++)
					{	
						if (clusterSizeCopy[i]==cluster_size[j])
							index[j]=i;					 					
					}
				  		
	    		Instances centroids2 = clu_res2.getCentroids();
	    		for (int k=0;k<index.length;k++)
		    	{
	    			Instance centroid = centroids2.instance(index[k]);
	    			//	    		int counter =0;
	    			//   		for (Instance centroid:centroids2)
	    			//   		{   
	    			String[] yesnos= centroid.toString().split(",");
	    			for (int i=0;i<yesnos.length;i++)
	    				if (yesnos[i].equalsIgnoreCase("yes"))	
	    				{
	    					//System.out.println("Ning test:" + centroid.attribute(i));
	    					String yes = centroid.attribute(i).name();
	    					String[] namevalue = yes.split(PROP_VAL_CONNECTOR);
	    					String prop_name = namevalue[0];
	    					String prop_value = namevalue[1];
	    					System.err.println("centroid" + index[k] + ":" + prop_name + ":" + prop_value);
	    					if (prop_name.equalsIgnoreCase("loc")) 
	    						locations.add(URLEncoder.encode(prop_value)); //e.g. "O'Connell" will become "O%27Connell" for its later using in URL;
	    				}
	    			//    			counter++;			
	    			//	}
		    	}
	    	}
	   // 	setting.put(time, locations);
	    }		
		setting.put("time", times);
		setting.put("loc", locations);
		return setting;

	}
/*	
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
	*/
	public boolean generateTimeLocationArff(String sparqlResult, String input){
		//for all the values of each property (across all events) avoiding duplicates
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

			if (!FIRST_EVENT && !taggedEvents.contains(row.getValue("event"))) //Not for the first event
			{				
				pairs.put("sTime", new HashSet<String>(e_sTime)); 
				pairs.put("eTime", new HashSet<String>(e_eTime));				
				pairs.put("loc", new HashSet<String>(e_loc));
				eventsPropertiesValues.put(prev_event, new HashMap<String,Set<String>>(pairs));
				e_sTime.clear(); e_eTime.clear(); e_loc.clear(); 
				pairs.clear();
			}	
			FIRST_EVENT = false;

			if (row.getValue("sTime") !=null) { 
				String tmp_sTime = row.getValue("sTime");
				String temp = tmp_sTime.substring(0,tmp_sTime.indexOf("-")).replaceAll(" ","_");//.replaceAll("[^A-Za-z0-9_']", "");
			   sTime.add(temp); e_sTime.add(tmp_sTime.substring(0,tmp_sTime.indexOf("-")));
			} //System.out.println ("sTime: " + row.getValue("sTime")); 
			if (row.getValue("eTime") !=null) {
				String tmp_eTime=row.getValue("eTime");
				String temp = tmp_eTime.substring(0,tmp_eTime.indexOf("-")).replaceAll(" ","_");//.replaceAll("[^A-Za-z0-9_']", "");
				eTime.add(temp); e_eTime.add(tmp_eTime.substring(0,tmp_eTime.indexOf("-"))); 
			}//System.out.println ("eTime: " + row.getValue("eTime")); }
			if (row.getValue("loc_FBL") !=null) {
				String temp_FBL = StringEscapeUtils.unescapeXml(row.getValue("loc_FBL")); // e.g. "O&apos;Connell" back to "O'Connell"
				String[] temp_FBLs = temp_FBL.split(", ");
				for (int i=0;i<temp_FBLs.length;i++)
				{String temp=temp_FBLs[i].replaceAll(" ","_");//.replaceAll("[^A-Za-z0-9_']", "");
				loc.add(temp); e_loc.add(temp_FBLs[i]); 
				}
			//	System.out.println("loc_FBL: " + row.getValue("loc_FBL")); 
			} 
			/*else if (row.getValue("loc") !=null && !row.getValue("loc").contains("freebase.com")) {String temp = row.getValue("loc").replaceAll(" ","_").replaceAll("[^A-Za-z0-9_]", "");
			loc.add(temp); e_loc.add(row.getValue("loc")); } //System.out.println("loc: " + row.getValue("loc"));} */
			else if (row.getValue("loc") !=null && !row.getValue("loc").contains("freebase.com")) {
				String temp_loc = StringEscapeUtils.unescapeXml(row.getValue("loc"));
				String[] temp_locs = temp_loc.split(", ");
				for (int i=0;i<temp_locs.length;i++)
				{String temp=temp_locs[i].replaceAll(" ","_");//.replaceAll("[^A-Za-z0-9_']", "");
				loc.add(temp); e_loc.add(temp_locs[i]); 
				}
			}
		
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
			{ 
				if (!propVals.isEmpty())
				{
					sTimeStr = "@attribute start_date {";
					propertyValues_list.add(0, prop);			
					for (String propVal:propVals)
					{  
						sTimeStr += propVal + ",";
					}
					sTimeStr += "}\n";
				}
			}
			else if (prop.equalsIgnoreCase("eTime"))
			{  
				if( !propVals.isEmpty()) 
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
					othersStr += "@attribute \"" + prop + PROP_VAL_CONNECTOR + propVal + "\" {yes,no}\n";
					propertyValues_list.add(prop + PROP_VAL_CONNECTOR + propVal);
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
			
			List<String> eventData = new ArrayList<String>();
			boolean VALID = false;
			
			///BUG here, just for fixing arff with only location
			if(propertyValues_list.contains("sTime") && propertyValues_list.contains("eTime")) 
			{	eventData.add("?"); eventData.add("?");
			   for (int i=2;i< propertyValues_list.size();i++)
				eventData.add("no");
			}
			else
			{
				for (int i=0;i< propertyValues_list.size();i++)
				eventData.add("no");
			}
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
					 VALID=true;
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
					if(propertyValues_list.contains(eDataKey + PROP_VAL_CONNECTOR + eVal.replaceAll(" ","_")));//.replaceAll("[^A-Za-z0-9_]", "")))
					 {
			//		  System.err.println("MATCHED" + eDataKey+ "_" + eVal);	
					  eventData.set(propertyValues_list.indexOf(eDataKey+ PROP_VAL_CONNECTOR + eVal.replaceAll(" ","_")), "yes");
					  VALID = true;
					 }
				    }
				}
			}
            if(VALID) //exclude all nos.
            {	
            	for (int i=0;i<eventData.size()-1;i++)
            		dataStr +=eventData.get(i)+ ", ";
            	if (eventData.size()>=1)
            		dataStr +=eventData.get(eventData.size()-1) + "\n";
            }
		}
		if(dataStr.isEmpty()) 
			return false;
		else{
			str +=dataStr;	
			String fn= input;
			BufferedWriter writer = null;
			try {
				writer = new BufferedWriter( new FileWriter(fn));
				writer.write(str);
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

	public static void main(String[] args) throws Exception{
		String dossier = "http://decipher-research.eu/dossiers/1308";
		String queryURI = "http://localhost:8088/openrdf-sesame/repositories/RecommenderDemo?query=";
		EventCluster eCluster= new EventCluster(queryURI, dossier);
		eCluster.clusterEvents(3);
	}	
}
