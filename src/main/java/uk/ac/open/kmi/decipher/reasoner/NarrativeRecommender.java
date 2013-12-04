package uk.ac.open.kmi.decipher.reasoner;

import java.io.File;
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

    private static final String TEMP_DIR = "/Decipher/tmp/"; 
	
/*	private static final Map<String, String> propertiesMap;
    static {
        Map<String, String> tMap = new HashMap<String, String>();
        tMap.put("time", "hasStartTime");
        tMap.put("location", "hasLocation");
        tMap.put("agent", "hasAgent");
        tMap.put("activity", "hasActivity");
        tMap.put("object", "includesObject");
        tMap.put("theme", "hasTheme");
        tMap.put("Genre", "hasGenre");
        tMap.put("Style", "hasStyleOrMovement");
        tMap.put("dimension", "hasObservedDimension");
        tMap.put("value", "hasObservedValue");
        tMap.put("material", "usesMaterial");
        propertiesMap = Collections.unmodifiableMap(tMap);
    }
*/	
	private static final Map<String, String> propertiesMap;
    static {
        Map<String, String> tMap = new HashMap<String, String>();
        tMap.put("time", "hasStartTime");
        tMap.put("location", "hasLocation");
        tMap.put("agent", "hasAgent");
        tMap.put("activity", "hasActivity");
        tMap.put("object", "includesPhysicalObject");
   //     tMap.put("theme", "hasTheme");
        tMap.put("Genre", "hasGenre");
        tMap.put("Style", "hasStyleOrMovement");
   //     tMap.put("dimension", "hasObservedDimension");
        tMap.put("value", "hasObservedValue");
   //     tMap.put("material", "usesMaterial");
        propertiesMap = Collections.unmodifiableMap(tMap);
    }
    
	String queryEndPoint;
	String dossierID;
	private String cacheFileName;
	String plotDesc_domainURI="";
	String te_domainURI="";
	Map<String,String> plotTimeMap = new HashMap<String,String>(); //the earliest time for each plotElement
	LispBridge LispATMS;
	
	public NarrativeRecommender(String repQueryURI, String dossier, LispBridge Lisp_atms) {
		this.queryEndPoint = repQueryURI;
		this.dossierID = dossier;
		this.cacheFileName = File.separator + "Decipher" + File.separator +"tmp" + File.separator + dossierID.substring(dossierID.lastIndexOf("/")+1) +"_recommendedNarratives.xml";
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
		
		String narrativesStr = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?> <" + NARRATIVE_ROOT_ELEMENT +"s>";
		String linearSimpleStr= "<" + NARRATIVE_ROOT_ELEMENT + ">";
		String linearCompleteStr = linearSimpleStr, layeredStr =linearSimpleStr, multiRouteStr = linearSimpleStr;
		String sparqlXML = this.getPlotElements();
//		System.out.println(sparqlXML);		
		Map<String, Map<String, Set<String>>> plotElements = parseSparqlXML(sparqlXML);	//e.g. <PE1 <source <te1, te2>>>		
		Set<Set<String>> ATMS_result = LispATMS.bridge(plotElements);

 /*    //mock-up input for testing
		Set<Set<String>> ATMS_result = new HashSet<Set<String>>(); //e.g. {{PE4,PE2}, {PE1, PE3}}
		String[] pe_keys = plotElements.keySet().toArray(new String[0]);
		int size = pe_keys.length;
		System.out.println(size/3);
		for(int i=0;i<3; i++)
		{	
			Set<String> temp = new HashSet<String>();
			for(int j=size/3*i;j<size/3*(i+1); j++)				
				temp.add(pe_keys[j]);
			
			ATMS_result.add(temp);
		}
	*/				
		String[][] results = sortNarratives(ATMS_result, true);//[[chosenProperties, PE4, PE5, PE3, PE1, PE2], [chosenProperties, PE1, PE2, PE3, PE4, PE5]]
		
		//linear simple /*only show the first group of plot elements*/
		linearSimpleStr += "<" + NARRATIVE_TYPE + "> linear simple </"+ NARRATIVE_TYPE + ">";
		linearSimpleStr += "<" + PLOT + ">";
		linearSimpleStr += "<" + PLOT_ID + ">" + UUID.randomUUID().toString() + "</"+ PLOT_ID + ">";
		linearSimpleStr += "<" + PLOT_NAME + "></"+ PLOT_NAME + ">";
		linearSimpleStr += "<" + PLOT_TAGS + ">" + results[0][0] + "</"+ PLOT_TAGS + ">";
		linearSimpleStr += "<" + PLOT_ELEMENT + "s>";
		for (int j=1;j<results[0].length; j++)
		{
			String pe = results[0][j];
			linearSimpleStr +="<" + PLOT_ELEMENT + ">";
			linearSimpleStr +="<" + PLOT_ELEMENT_ID + ">" + plotDesc_domainURI + pe + "</" + PLOT_ELEMENT_ID + ">";
			linearSimpleStr +="<" + PLOT_ELEMENT_WEIGHT + ">" + j + "</" + PLOT_ELEMENT_WEIGHT + ">";
			linearSimpleStr +="</" + PLOT_ELEMENT + ">";

		}
		linearSimpleStr += "</" + PLOT_ELEMENT + "s>";
		linearSimpleStr += "</" + PLOT + ">";
		linearSimpleStr += "</" + NARRATIVE_ROOT_ELEMENT + ">";
		
		//multi-route  /*show each group of plot elements one by one*/
		multiRouteStr += "<" + NARRATIVE_TYPE + "> multi-route </"+ NARRATIVE_TYPE + ">";
		multiRouteStr += "<" + PLOT + "s>";
		for (int i=0; i<results.length;i++)
		{ 
			multiRouteStr += "<" + PLOT + ">";
			multiRouteStr += "<" + PLOT_ID + ">" + UUID.randomUUID().toString() + "</"+ PLOT_ID + ">";
			multiRouteStr += "<" + PLOT_NAME + "></"+ PLOT_NAME + ">";
			multiRouteStr += "<" + PLOT_TAGS + ">" + results[0][0] +"</"+ PLOT_TAGS + ">";
			multiRouteStr += "<" + PLOT_ELEMENT + "s>";

			for (int j=1;j<results[i].length; j++)
			{
				String pe = results[i][j];
			//	System.out.println(pe);
				multiRouteStr +="<" + PLOT_ELEMENT + ">";
				multiRouteStr +="<" + PLOT_ELEMENT_ID + ">" + plotDesc_domainURI + pe + "</" + PLOT_ELEMENT_ID + ">";
				multiRouteStr +="<" + PLOT_ELEMENT_WEIGHT + ">" + j + "</" + PLOT_ELEMENT_WEIGHT + ">";
				multiRouteStr +="</" + PLOT_ELEMENT + ">";
			}
			multiRouteStr += "</" + PLOT_ELEMENT + "s>";
			multiRouteStr += "</" + PLOT + ">";
		}	
		multiRouteStr += "</" + PLOT + "s>";
		multiRouteStr += "</" + NARRATIVE_ROOT_ELEMENT + ">";

		//layered
		layeredStr += "<" + NARRATIVE_TYPE + "> layered </"+ NARRATIVE_TYPE + ">";
		layeredStr += "<" + PLOT + ">";
		layeredStr += "<" + PLOT_ID + ">" + UUID.randomUUID().toString() + "</"+ PLOT_ID + ">";
		layeredStr += "<" + PLOT_NAME + "></"+ PLOT_NAME + ">";
		layeredStr += "<" + PLOT_TAGS + ">" + results[0][0] + "</"+ PLOT_TAGS + ">";
		layeredStr += "<" + PLOT_ELEMENT + "s>";
		for (int j=1;j<results[0].length; j++)
		{
			String pe = results[0][j];
			layeredStr +="<" + PLOT_ELEMENT + ">";
			layeredStr +="<" + PLOT_ELEMENT_ID + ">" + plotDesc_domainURI + pe + "</" + PLOT_ELEMENT_ID + ">";
			layeredStr +="<" + PLOT_ELEMENT_WEIGHT + ">" + j + "</" + PLOT_ELEMENT_WEIGHT + ">";
			layeredStr +="</" + PLOT_ELEMENT + ">";
		}
		int layer=0;
		String[] layeredResults = layeredNarrative(plotElements, results[0]);
		while (layeredResults!=null)
		{	
			for (int j=1;j<layeredResults.length; j++)		
			{
				String pe = layeredResults[j];
				if (pe!="")
				{
					layeredStr +="<" + PLOT_ELEMENT + ">";
					layeredStr +="<" + PLOT_ELEMENT_ID + ">" + plotDesc_domainURI + pe + "</" + PLOT_ELEMENT_ID + ">";
					layeredStr +="<" + PLOT_ELEMENT_PARENT + ">" + plotDesc_domainURI + results[layer][j] + "</" + PLOT_ELEMENT_PARENT + ">";
					layeredStr +="</" + PLOT_ELEMENT + ">";
				}
			}
			layer+=1;
			if (layer < results.length)
			{
				results[layer]=layeredResults;	//replace the next layers with the new results
				layeredResults = layeredNarrative(plotElements, layeredResults);
			}
		}
			
		layeredStr += "</" + PLOT_ELEMENT + "s>";
		layeredStr += "</" + PLOT + ">";
		layeredStr += "</" + NARRATIVE_ROOT_ELEMENT + ">";
        //results now contain the layered sections,but we only need the first few top layers
		
		//linear complete
		linearCompleteStr += "<" + NARRATIVE_TYPE + "> linear complete </"+ NARRATIVE_TYPE + ">";
		linearCompleteStr += "<" + PLOT + ">";
		linearCompleteStr += "<" + PLOT_ID + ">" + UUID.randomUUID().toString() + "</"+ PLOT_ID + ">";
		linearCompleteStr += "<" + PLOT_NAME + "></"+ PLOT_NAME + ">";
		linearCompleteStr += "<" + PLOT_TAGS + ">" + results[0][0]!=null? results[0][0]:"no outstanding properties" + "</"+ PLOT_TAGS + ">";
		linearCompleteStr += "<" + PLOT_ELEMENT + "s>";
		for (int i=1;i<results[0].length; i++)
			for (int j=0;j<layer+1; j++) //using the first top few layers from layered results
			{
				String pe = results[j][i];
				if (pe!="")
				{   int weight =i+j;
					linearCompleteStr +="<" + PLOT_ELEMENT + ">";
					linearCompleteStr +="<" + PLOT_ELEMENT_ID + ">" + plotDesc_domainURI + pe + "</" + PLOT_ELEMENT_ID + ">";
					linearCompleteStr +="<" + PLOT_ELEMENT_WEIGHT + ">" + weight + "</" + PLOT_ELEMENT_WEIGHT + ">";
					linearCompleteStr +="</" + PLOT_ELEMENT + ">";
				}
			}
		linearCompleteStr += "</" + PLOT_ELEMENT + "s>";
		linearCompleteStr += "</" + PLOT + ">";
		linearCompleteStr += "</" + NARRATIVE_ROOT_ELEMENT + ">";
		
		narrativesStr += linearSimpleStr + multiRouteStr + layeredStr + linearCompleteStr ;
		narrativesStr += "</" + NARRATIVE_ROOT_ELEMENT + "s>";;
		FileUtil.write2File(narrativesStr, cacheFileName);	
		return narrativesStr;		
	}
	/*********************************************************************************
	 * 
	 * @param plotElements
	 * @param toplayer_results:  the top layer of last result, e.g. [chosenProperties, PE5, PE4, PE3, PE1, PE2], 
	 * 		 the purpose is to remove this top layer plot elements from plotElements before being put to ATMS.
	 * @return
	 */
	private String[] layeredNarrative(Map<String, Map<String, Set<String>>> plotElements,String[] toplayer_results) {
		String[] return_val = new String[toplayer_results.length]; //[space for chosen properties, peX, "", "", peY, ""];
		Arrays.fill(return_val, "");
		//remove the plot elements that are at the top layer
		Set<String> peKeys = plotElements.keySet();
		for (int j=1;j<toplayer_results.length; j++)
		{
			String pe = toplayer_results[j];
            if (peKeys.contains(pe))
            	plotElements.remove(pe);
		}
		
		if (plotElements.size()>1)
		{
			Set<Set<String>> ATMS_result = LispATMS.bridge(plotElements);
			String[][] sub_results = sortNarratives(ATMS_result, true);
			//do a match of the top layer of results and sub_results
			String[][] eventsPerPlot = new String[toplayer_results.length][];
			String[][] eventsPerPlotSub = new String[sub_results[0].length][];
//			int[][] overlapNum = new int[results[0].length][sub_results[0].length];

			//for each plot element in the top layer of the sub_results, calculate the overlapping events with every element of the top layer of last results
			for (int i=1; i<toplayer_results.length;i++)
			{
				String pe = toplayer_results[i];
				Set<String> eventPerPE=this.getEventsPerPE(plotDesc_domainURI + pe);
				eventsPerPlot[i]=eventPerPE.toArray(new String[0]);
			}
			for (int i=1; i<sub_results[0].length;i++)
			{
				String pe = sub_results[0][i];
				Set<String> eventPerPE=this.getEventsPerPE(plotDesc_domainURI + pe);
				eventsPerPlotSub[i]=eventPerPE.toArray(new String[0]);
			}

			for (int j=1; j<sub_results[0].length;j++)
			{ 
				int[] num=new int[toplayer_results.length];				
				int max=-1, maxCol=-1;

				for (int i=1; i<toplayer_results.length; i++)
				{	
					Collection<String> listOne = Arrays.asList(eventsPerPlot[i]);
					//	Collection<String> listTwo = Arrays.asList(eventsPerPlotSub[j]);
					//	listOne.retainAll(listTwo);
					for (int m=0;m<eventsPerPlotSub[j].length;m++)
						if(listOne.contains(eventsPerPlotSub[j][m]))
							num[i]++;
				}
				for (int m=1;m<num.length;m++)
					if(num[m]>max)
					{	maxCol=m; max=num[m];}
				return_val[maxCol]=sub_results[0][j];
			}		
			return return_val;
		}		
        return null;
	}

	private String getPlotElements(){
		
		String query = SPARQLUtil.PREFIX 
				+ "SELECT  ?plot ?plotDesc ?te "   //now plot is story, plotDesc is story section
	 		    + "WHERE {<" + dossierID +"> curate:tellsStoryOfDossier ?plot. "
	 		    + "?plot curate:containsStorySection ?plotDesc. "
	 		   // + "?plotElement dc:title ?peTitle"
	 		    + "?plotDesc curate:hasAssociatedEvent ?te"
				+ "}";		 
		 SPARQLClient sClient = new SPARQLClient();
		 String results = sClient.httpQuery(queryEndPoint, query, "application/sparql-results+xml");
		 return results;
		}
	
	private Set<String> getEventsPerPE(String peURI) {
		 Set<String> events = new HashSet<String>();
		 String query = SPARQLUtil.PREFIX 
					+ "SELECT  ?te "
		 		    + "WHERE {"
					+ "<" + peURI +">  curate:hasAssociatedEvent ?te. "
					+ " } ";
		 SPARQLClient sClient = new SPARQLClient();
		 String results = sClient.httpQuery(queryEndPoint, query, "application/sparql-results+xml");
		 QueryResult qr = SPARQLQueryResultParser.parse(results);	
		 List<QueryRow> rows = qr.getQueryRows();
		 Iterator<QueryRow> rows_iter = rows.iterator();
		 while (rows_iter.hasNext()){
				QueryRow row = rows_iter.next();
				String event = row.getValue("te");
				events.add(event);
		}
		 return events;
	}	
	private int getEventNumPerPE(String peURI) {
		 String query = SPARQLUtil.PREFIX 
					+ "SELECT  ?te "
//					+ "SELECT  (COUNT(?te) AS ?count) "
		 		    + "WHERE {"
					+ "<" + peURI +">  curate:hasAssociatedEvent ?te. "
					+ " } ";
		 SPARQLClient sClient = new SPARQLClient();
		 String results = sClient.httpQuery(queryEndPoint, query, "application/sparql-results+xml");
	//	 System.out.println(results);
		
		 QueryResult qr = SPARQLQueryResultParser.parse(results);	
		 List<QueryRow> rows = qr.getQueryRows();
		 return rows.size();
		/* Iterator<QueryRow> rows_iter = rows.iterator();
		 while (rows_iter.hasNext()){
				QueryRow row = rows_iter.next();
				String count = row.getValue("count");
				return Integer.parseInt(count);
		}
		 return 0;*/
	}	
	
    private Map<String, Map<String, Set<String>>> parseSparqlXML(String sparqlXML) {
		
		Map<String, Map<String, Set<String>>> plotElements = new HashMap<String,Map<String, Set<String>>>();		
		
		QueryResult qr = SPARQLQueryResultParser.parse(sparqlXML);	
		List<QueryRow> rows = qr.getQueryRows();
		Iterator<QueryRow> rows_iter = rows.iterator();
		while (rows_iter.hasNext()){
			QueryRow row = rows_iter.next();
			String plotDesc = row.getValue("plotDesc");
			
			plotDesc_domainURI= plotDesc.substring(0,plotDesc.lastIndexOf("/")+1); //4 digits now//8 digits plus one forward slash
	//		System.out.println(domainURL);			
			String plotElementID = plotDesc.substring(plotDesc.lastIndexOf("/")+1); 
	//		System.out.println("Key is: " + plotElementID);
			String subKey="RELATED";
			String te = row.getValue("te");
			te_domainURI = te.substring(0,te.lastIndexOf("/")+1);
			String teID = te.substring(te.lastIndexOf("/")+1);
			
			if (plotElements.keySet().contains(plotElementID))
			{	
				Map<String, Set<String>> iddedMap = plotElements.get(plotElementID);
				if (plotElements.get(plotElementID).keySet().contains(subKey))  
					plotElements.get(plotElementID).get(subKey).add(teID);
				else
				{   
					Set<String> tempSet = new HashSet<String>();
				    tempSet.add(teID);
				    iddedMap.put(subKey, tempSet);
				    plotElements.put(plotElementID, iddedMap);
				}
			}
			else
			{
				Set<String> tempSet = new HashSet<String>();
				tempSet.add(teID);
				Map<String, Set<String>> tempMap = new HashMap<String, Set<String>>();
				tempMap.put(subKey, tempSet);	
			    plotElements.put(plotElementID, tempMap);
			}

		}
		return plotElements;
	}
		
    /************************************************************************************************
     Inter-plotElements are sorted according to the number of events contained in each group of plot elements
     Intra-plotElements are sorted as follows: 
        1: The domainant properties for each group of plot elements are decided using R48 (for time) or ID3 algorithms
        2: Sort each group of elements according to the chosen properties. 
            PS, at the moments, the chosen properties are ignored, and event start time are used to sort the plot elements within each group.
     **********************************************************************************************/
	private String[][] sortNarratives(Set<Set<String>> randomOrderedResult, boolean SORT_EACH_GROUP){
		
		int[] event_nums = new int[randomOrderedResult.size()];
		String[][] plot_eles = new String[randomOrderedResult.size()][];
		int counter=0;
		
		Iterator ror_iter = randomOrderedResult.iterator();
		while (ror_iter.hasNext())
		{ 			
			Set<String> randomPlotElements = (Set<String>)ror_iter.next();
			plotTimeMap.clear(); //clear it for every group of plotElements
			List<String> plotElements = new ArrayList<String>();
			plotElements.addAll(randomPlotElements);
			if (SORT_EACH_GROUP) //intra-plotElement sorting
				plotElements = sortPlotElements(randomPlotElements);	//the first element is the property chosen to organize the narrative				
			
			plot_eles[counter]=plotElements.toArray(new String[0]);
		
			int eventNumPerNarrative=0;
			for(int i=1; i< plotElements.size();i++)
			{
				String pe = plotElements.get(i);			
	//			System.out.println(pe + ":" + getEventNumPerNarrative(plotDesc_domainURI + pe));
				eventNumPerNarrative += getEventNumPerPE(plotDesc_domainURI + pe);								
			}
			event_nums[counter] = eventNumPerNarrative;
	//		System.out.println("the total number of events per narrative: " + eventNumPerNarrative);
		    counter++;
		}	
			
			//Ordered according to the number of events in each narrative group
			int temp=0; String[] tempObj=null;
			for (int i=0;i<counter;i++)
				for (int j=1;j<counter-i;j++)
					if(event_nums[j] > event_nums[j-1])
					{
						temp=event_nums[j]; tempObj=plot_eles[j];
						event_nums[j]=event_nums[j-1]; plot_eles[j] = plot_eles[j-1];
						event_nums[j-1]=temp; plot_eles[j-1] =tempObj;
					}
          
		return plot_eles;
	}
	
	private List<String> sortPlotElements(Set<String> plotElements) {
		
		List<String> sortedPlotElements = new ArrayList<String>();
		String chosenProperty = chooseSortingEventProperties(prepareData4Classifier(plotElements));
        System.out.println("the chose ordering property is: " + chosenProperty); 
      /*  if (chosenProperty != "time") //no ordering for other type of properties yet
        {
        	sortedPlotElements.addAll(plotElements);
        	sortedPlotElements.add(0, chosenProperty); //put the chosen property as the first element
        }
        else //sorting the PEs in the order of time
        {  */
    /*****new change in 22nd, July, order the elements according to the start_time regardless of the ID3 results)    	
        	/* This was used when lisp did not work properly, may be redundant now
        	List<String> noTimePropPE =new ArrayList<String>();
  			for (String pe_key : plotElements)
				if (!plotTimeMap.containsKey(pe_key))
				    noTimePropPE.add(pe_key);
            for (String noStr:noTimePropPE)
            	plotElements.remove(noStr);
  			*/
  			String[] toBeSortedPEs = plotElements.toArray(new String[0]);
        	String[] toBeSortedTime = new String[toBeSortedPEs.length];
        	for (int i=0; i<toBeSortedPEs.length; i++)
        		{
        			toBeSortedTime[i]=plotTimeMap.get(toBeSortedPEs[i]);
              	    if (toBeSortedTime[i]==null) toBeSortedTime[i]="0000"; //special cases of no start_time at all in the whole section, seen in dossier 1533 story 3676.
        		}
        	String temp=null; String tempPE=null;
        	for (int i=0; i<toBeSortedPEs.length-1; i++)
        		for (int j=1;j<toBeSortedPEs.length-i;j++)
        			if(Integer.valueOf(toBeSortedTime[j]) < Integer.valueOf(toBeSortedTime[j-1]))
        			{
        				temp=toBeSortedTime[j]; tempPE=toBeSortedPEs[j];
        				toBeSortedTime[j]=toBeSortedTime[j-1]; toBeSortedPEs[j] = toBeSortedPEs[j-1];
        				toBeSortedTime[j-1]=temp; toBeSortedPEs[j-1] =tempPE;
        			}
        	List<String> tempList = Arrays.asList(toBeSortedPEs);
        	sortedPlotElements.addAll(tempList);
        //	sortedPlotElements.addAll(noTimePropPE);
        	sortedPlotElements.add(0, chosenProperty); //put the chosen property as the first element
    //    }
        return sortedPlotElements;
	}
		
	private Map<String,Set<String>> getEventProperties(String peURI, String var, String property){

		Map<String, Set<String>> eventValuesMap = new HashMap<String,Set<String>>(); //e.g. <te1, {london, rome}>
		String queryEventProperities = SPARQLUtil.PREFIX; 
		if (property.equalsIgnoreCase("hasStartTime"))
			queryEventProperities += "SELECT ?te ?" + var
				+ " WHERE {"
				+ "<" + peURI +"> curate:hasAssociatedEvent ?te."   //using the new schema
				+ "?te curate:"+ property +" ?"+ var +". "
				+ "}";	 
		else queryEventProperities = SPARQLUtil.PREFIX 
				+ "SELECT ?te ?" + var
				+ " WHERE {"
				+ "<" + peURI +"> curate:hasAssociatedEvent ?te."   //using the new schema
				+ "optional {?te curate:"+ property + "?varFB. ?varFB rdfs:label " + " ?"+ var +".} "
				+ "}";
		
		SPARQLClient sClient = new SPARQLClient();
		String results = sClient.httpQuery(queryEndPoint, queryEventProperities, "application/sparql-results+xml");
//      System.out.println(results);
		QueryResult qr = SPARQLQueryResultParser.parse(results);	
		List<QueryRow> rows = qr.getQueryRows();
		Iterator<QueryRow> rows_iter = rows.iterator();
		while (rows_iter.hasNext())
		{   
			String varValue=null,eventID=null; 
			QueryRow row = rows_iter.next();
			if (row.getValue(var)!=null && !row.getValue(var).contains("freebase.com")) //Ning noted
				{  
					if (property.equalsIgnoreCase("hasStartTime"))
						varValue= row.getValue(var);
					else
						varValue= row.getValue(var).replaceAll("[^A-Za-z0-9]", "");
				}
			if (row.getValue("te")!=null) 
				eventID= row.getValue("te");

			if (varValue!=null && eventID!=null)			
				if (eventValuesMap.keySet().contains(eventID))
				{	
					Set<String> iddedSet = eventValuesMap.get(eventID);
					if (varValue !=null) iddedSet.add(varValue);
				}
				else
				{
					Set<String> tempSet = new HashSet<String>();
					if (varValue !=null) tempSet.add(varValue);
					eventValuesMap.put(eventID, tempSet);
				}
		}
		return eventValuesMap;
	}
	
	private Map<String,String> prepareData4Classifier(Set<String> plotElements){
		
		Map<String,String> result = new HashMap<String,String>(); //e.g <agent, /Decipher/temp/agent_XXXx....csv>

		Iterator prop_iter = propertiesMap.keySet().iterator(); //e.g. <agent,hasAgent>
		while (prop_iter.hasNext()) //per event property
		{ 
			Map<String, Map<String, Set<String>>>  plotEventValuesMap = new HashMap<String, Map<String,Set<String>>>(); // e.g. <PE1,<te1 {lodnon, rome}>>
			Set<String>  allValuesPerProp = new HashSet<String>(); 	// {london, rome,italy, england}
			
			String propName = (String) prop_iter.next();
			String fileID=TEMP_DIR + propName + "_";
			Iterator pe_iter = plotElements.iterator();
			while (pe_iter.hasNext())
			{ 
				String pe = (String)pe_iter.next();
				fileID +=  pe.replace("/", "-") + "_"; //Ning noted

				Map<String,Set<String>> eventPropValuesMap = getEventProperties(plotDesc_domainURI + pe, propName, propertiesMap.get(propName));
				plotEventValuesMap.put(pe, eventPropValuesMap); 
				for (String event_key:eventPropValuesMap.keySet())
					allValuesPerProp.addAll(eventPropValuesMap.get(event_key));
			}
			if (allValuesPerProp.size()>1) //ID3 don't handle unary class
			{
				fileID +=".csv"; 
				String csvStr="", attrStr="", dataStr="";
				if (propName == "time")
				{
					attrStr += "start_time,plot\n";

					Set<String> PEs= plotEventValuesMap.keySet();
					for(String PE:PEs)
					{ 
						Map<String, Set<String>> valuesPerPlot = plotEventValuesMap.get(PE);
						Set<String> eventKeys =valuesPerPlot.keySet();
						for(String eventKey:eventKeys)
						{
							Set<String> valuesPerEvent= valuesPerPlot.get(eventKey);				  				  
							for (String value:valuesPerEvent)
							{	//System.err.println(value + ":" + value.indexOf("-"));
								String val = value.substring(0,value.indexOf("-")); //Time is not necessarily 4 digits anymore, sometimes 3
								dataStr += val+",";
							    if (plotTimeMap.containsKey(PE))
							    {	
							    	if (Integer.valueOf(plotTimeMap.get(PE)) > Integer.valueOf(val))
							    		plotTimeMap.put(PE, val);
							    }
							    else
							    	plotTimeMap.put(PE, val);							    	
							}							
							dataStr += "plot"+ PE+"\n";		
						}
					}						
				}
				else
				{
					for (String prop_value:allValuesPerProp)   	
						attrStr += prop_value + ",";
					//attrStr += prop_value.replaceAll("[^A-Za-z0-9]", "") + ",";
					attrStr+="plot\n";

					Set<String> PEs= plotEventValuesMap.keySet();
					for(String PE:PEs)
					{ 
						Map<String, Set<String>> valuesPerPlot = plotEventValuesMap.get(PE);
						Set<String> eventKeys =valuesPerPlot.keySet();
						for(String eventKey:eventKeys)
						{
							Set<String> valuesPerEvent= valuesPerPlot.get(eventKey);				  				  
							for (String prop_value:allValuesPerProp)   	
								if (valuesPerEvent.contains(prop_value))
									dataStr += "yes,";
								else dataStr += "no,";
							dataStr += "plot"+ PE+"\n";		
						}
					}
				}
				csvStr = attrStr + dataStr;
				FileUtil.write2File_O(csvStr, fileID);
				result.put(propName, fileID);
			}
		}	
		return result;			
	}
		
	private String chooseSortingEventProperties(Map<String,String> propFileMap){
//		System.out.println("the total of properties are: " + propFileMap.keySet().size());
		
		String[] properties = new String[propFileMap.keySet().size()];
		Double[] correctRates = new Double[propFileMap.keySet().size()];		
		int counter=0;
		Double correctRate=0.0;

		for (String propName: propFileMap.keySet())
		{
			if (propName=="time")
			{
				J48Classifier classifier = new J48Classifier();
				correctRate = classifier.classify(propFileMap.get(propName));
			}
			else
			{
				ID3Classifier classifier = new ID3Classifier();
				correctRate = classifier.classify(propFileMap.get(propName));
			}
			if (correctRate!=null)
			{ 
				correctRates[counter] = correctRate;
				properties[counter]=propName;
				counter++;
			}
			System.out.println("The correct rate for " + propName +" is: " + correctRate);
		}
		
		//Ordered according to the number of events in each narrative group
		Double temp=0.0; String tempStr=null;
		for (int i=0;i<counter;i++)
			for (int j=1;j<counter-i;j++)
				if(correctRates[j] > correctRates[j-1])
				{
					temp=correctRates[j]; tempStr=properties[j];
					correctRates[j]=correctRates[j-1]; properties[j] = properties[j-1];
					correctRates[j-1]=temp; properties[j-1] =tempStr;
				}
				
        return properties[0];
	}
	
	public static void main(String[] args) throws Exception{
		LispBridge lispATMS = LispBridge.getInstance();
		String dossID = "http://decipher-research.eu/dossiers/1533";
//		String queryURI = "http://localhost:8088/openrdf-sesame/repositories/RecommenderDemo?query=";
		String queryURI = "http://storyscope5.ssl.co.uk:8080/openrdf-sesame/repositories/test?query=";
		NarrativeRecommender nr= new NarrativeRecommender(queryURI,dossID,lispATMS);
//		nr.getCount();
		nr.recommendNarratives();
	}
	
}
