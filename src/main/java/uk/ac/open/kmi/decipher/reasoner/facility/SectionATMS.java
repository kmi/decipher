package uk.ac.open.kmi.decipher.reasoner.facility;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
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

public class SectionATMS{

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
	private String queryEndPoint;
	private String dossierID;
	private String cacheFileName;

	List<String> taggedEvents_list= new ArrayList<String>();

	public SectionATMS(String repQueryURI, String dossier) {
		this.queryEndPoint = repQueryURI;
		this.dossierID = dossier;
		this.cacheFileName = File.separator + "Decipher" + File.separator +"tmp" + File.separator + dossierID.substring(dossierID.lastIndexOf("/")+1) +"_recommendedRelatedPlots.xml";
	}

	private String getEventProperties (){
			String queryEventProperities = SPARQLUtil.PREFIX 
					+ "SELECT  ?plotDesc ?object ?event ?sTime ?eTime ?agent ?act ?style ?genre ?loc ?val ?obj "   //now plot is story, plotDesc is story section
//					+ "WHERE {<" + dossierID +"> curate:tellsStoryOfDossier ?plot. "
//					+ "?plot curate:containsStorySection ?plotDesc. "  "2682, 2849, 2933, 3676, 3098"
					+ "WHERE {<http://decipher-research.eu/stories/3676> curate:containsStorySection ?plotDesc. "
					+ "?plotDesc curate:hasAssociatedObject ?object. "
/*					+ "optional {?plotDesc curate:hasAssociatedEvent ?event. "
					+ "optional {?event curate:hasStartTime ?sTime.} "
					+ "optional {?event curate:hasEndTime ?eTime.}"
					+ "optional {?event curate:hasAgent ?agent. }"
					+ "optional {?event curate:hasActivity ?act. }" 
					+ "optional {?event curate:hasStyleOrMovement ?style.} "
			//		+ "optional {?event curate:hasTheme ?theme.} "
					+ "optional {?event curate:hasGenre ?genre.} "
					+ "optional {?event curate:hasLocation ?loc.} "
			//		+ "optional {?event curate:hasObservedDimension ?dim.} "
					+ "optional {?event curate:hasObservedValue ?val.} "
					+ "optional {?event curate:includesPhysicalObject ?obj.}} "
			//		+ "optional {?event curate:usesMaterial ?material.} "
*/					+ "}";	 
					
			SPARQLClient sClient = new SPARQLClient();
			String results = sClient.httpQuery(queryEndPoint, queryEventProperities, "application/sparql-results+xml");
			System.out.println(results);
			return results;
		}
	
	  private Map<String, Map<String, List<Boolean>>> parseSparqlXML(String sparqlXML) {
			
			Map<String, Map<String, List<Boolean>>> plotElements = new HashMap<String,Map<String, List<Boolean>>>(); //<S7 <E3,<1,0,0,1..> <E4, <>>;		
	//		List<Boolean> reset = Arrays.asList(false,false,false,false,false,false,false,false,false);
	//		List<String> properties = Arrays.asList("sTime","eTime","agent","act","style","genre","loc","val","obj");
			
			QueryResult qr = SPARQLQueryResultParser.parse(sparqlXML);	
			List<QueryRow> rows = qr.getQueryRows();
			Iterator<QueryRow> rows_iter = rows.iterator();
			while (rows_iter.hasNext()){
				QueryRow row = rows_iter.next();
				String plotDesc = row.getValue("plotDesc");		
				String plotDesc_domainURI= plotDesc.substring(0,plotDesc.lastIndexOf("/")+1); //4 digits now//8 digits plus one forward slash
				String plotElementID = plotDesc.substring(plotDesc.lastIndexOf("/")+1); 
				
			//	String te_subKey =null; 
				String object_subKey=null;
				if (row.getValue("object")!=null)
				{
					String object = row.getValue("object");
					String objectID = object.substring(object.lastIndexOf("/")+1);
					object_subKey=objectID;		
				}
			/*	
				if (row.getValue("event")!=null)
				{
					String te = row.getValue("event");
					String teID = te.substring(te.lastIndexOf("/")+1);
					te_subKey=teID;							
				}
		     */	
				if (plotElements.keySet().contains(plotElementID))
				{	
					Map<String, List<Boolean>> iddedMap = plotElements.get(plotElementID);
				/*	if (plotElements.get(plotElementID).keySet().contains(te_subKey))  
					{
						List<Boolean> propertiesVal = (List<Boolean>) plotElements.get(plotElementID).get(te_subKey);
						if (row.getValue("sTime")!=null) propertiesVal.set(properties.indexOf("sTime"), true);
						if (row.getValue("eTime")!=null) propertiesVal.set(properties.indexOf("eTime"), true);
						if (row.getValue("agent")!=null) propertiesVal.set(properties.indexOf("agent"), true);
						if (row.getValue("act")!=null) propertiesVal.set(properties.indexOf("act"), true);
						if (row.getValue("style")!=null) propertiesVal.set(properties.indexOf("style"), true);
						if (row.getValue("genre")!=null) propertiesVal.set(properties.indexOf("genre"), true);
						if (row.getValue("loc")!=null) propertiesVal.set(properties.indexOf("loc"), true);
						if (row.getValue("val")!=null) propertiesVal.set(properties.indexOf("val"), true);
						if (row.getValue("obj")!=null) propertiesVal.set(properties.indexOf("obj"), true);
						
						plotElements.get(plotElementID).put(te_subKey, propertiesVal);
					}
					else
					{   
						List<Boolean> tempSet = new ArrayList<Boolean>(reset);						
						if (row.getValue("sTime")!=null) tempSet.set(properties.indexOf("sTime"), true);
						if (row.getValue("eTime")!=null) tempSet.set(properties.indexOf("eTime"), true);
						if (row.getValue("agent")!=null) tempSet.set(properties.indexOf("agent"), true);
						if (row.getValue("act")!=null) tempSet.set(properties.indexOf("act"), true);
						if (row.getValue("style")!=null) tempSet.set(properties.indexOf("style"), true);
						if (row.getValue("genre")!=null) tempSet.set(properties.indexOf("genre"), true);
						if (row.getValue("loc")!=null) tempSet.set(properties.indexOf("loc"), true);
						if (row.getValue("val")!=null) tempSet.set(properties.indexOf("val"), true);
						if (row.getValue("obj")!=null) tempSet.set(properties.indexOf("obj"), true);

					    iddedMap.put(te_subKey, tempSet);
					    plotElements.put(plotElementID, iddedMap);
					}
					*/
					if (!plotElements.get(plotElementID).keySet().contains(object_subKey))  						
					{	List<Boolean> tempSet = null;
					    iddedMap.put(object_subKey, tempSet);
					    plotElements.put(plotElementID, iddedMap);
					}
				}
				else
				{
					/*
					if (te_subKey !=null && object_subKey !=null)
					{
						List<Boolean> tempSet = new ArrayList<Boolean>(reset);						
						if (row.getValue("sTime")!=null) tempSet.set(properties.indexOf("sTime"), true);
						if (row.getValue("eTime")!=null) tempSet.set(properties.indexOf("eTime"), true);
						if (row.getValue("agent")!=null) tempSet.set(properties.indexOf("agent"), true);
						if (row.getValue("act")!=null) tempSet.set(properties.indexOf("act"), true);
						if (row.getValue("style")!=null) tempSet.set(properties.indexOf("style"), true);
						if (row.getValue("genre")!=null) tempSet.set(properties.indexOf("genre"), true);
						if (row.getValue("loc")!=null) tempSet.set(properties.indexOf("loc"), true);
						if (row.getValue("val")!=null) tempSet.set(properties.indexOf("val"), true);
						if (row.getValue("obj")!=null) tempSet.set(properties.indexOf("obj"), true);

						Map<String, List<Boolean>> tempMap = new HashMap<String, List<Boolean>>();
						tempMap.put(te_subKey, tempSet);
						tempSet = null;
						tempMap.put(object_subKey, tempSet);	
						plotElements.put(plotElementID, tempMap);
					}
					else if (te_subKey !=null)
					{
						List<Boolean> tempSet = new ArrayList<Boolean>(reset);						
						if (row.getValue("sTime")!=null) tempSet.set(properties.indexOf("sTime"), true);
						if (row.getValue("eTime")!=null) tempSet.set(properties.indexOf("eTime"), true);
						if (row.getValue("agent")!=null) tempSet.set(properties.indexOf("agent"), true);
						if (row.getValue("act")!=null) tempSet.set(properties.indexOf("act"), true);
						if (row.getValue("style")!=null) tempSet.set(properties.indexOf("style"), true);
						if (row.getValue("genre")!=null) tempSet.set(properties.indexOf("genre"), true);
						if (row.getValue("loc")!=null) tempSet.set(properties.indexOf("loc"), true);
						if (row.getValue("val")!=null) tempSet.set(properties.indexOf("val"), true);
						if (row.getValue("obj")!=null) tempSet.set(properties.indexOf("obj"), true);

						Map<String, List<Boolean>> tempMap = new HashMap<String, List<Boolean>>();
						tempMap.put(te_subKey, tempSet);	
						plotElements.put(plotElementID, tempMap);
					}
					else */
					if (object_subKey !=null)
					{
						List<Boolean> tempSet = null;
						Map<String, List<Boolean>> tempMap = new HashMap<String, List<Boolean>>();
						tempMap.put(object_subKey, tempSet);	
						plotElements.put(plotElementID, tempMap);
					}
				}

			}
			return plotElements;
		}
		
	private String Map2String(Map<String, Map<String, List<Boolean>>> in_param)
	{
		 	String inputAsStr="(";
		    Set<String> secKeys = in_param.keySet();
		    for (String secKey:secKeys)
		    {   
		    	String secStr="("+ secKey; //+ " ("; //brackets all of the events
		    	String objectStr=" (";
		    	Map<String, List<Boolean>> sectionValues = in_param.get(secKey);
		    /*	
		    	Set<String> events = sectionValues.keySet();
		    	for (String event:events)
		    	{   List<Boolean> values = sectionValues.get(event);
		    	
		    		if (values!=null) //event values
		    		{
		    			String eventStr = "(" + event + " (";
		    			for (Boolean eventValue:values)
		    			{  
		    				String exist = eventValue==true? Integer.toString(1):Integer.toString(0);
		    				eventStr += exist + " ";
		    			}
		    			eventStr =eventStr.substring(0,eventStr.length()-1) + ")) ";
		    			secStr += eventStr;

		    		}
		    		else //object values
		    		{
				    	objectStr +=event + " "; 
		    		}
		    	}
		    	*/
		    	Set<String> objects = sectionValues.keySet();
		    	for (String object:objects)		    		
				    	objectStr +=object + " "; 
		 
	  			if (objectStr.endsWith(" "))
    				objectStr =objectStr.substring(0,objectStr.length()-1) + ")";           
		    	secStr +=objectStr + ")";;
		    	inputAsStr +=secStr +" "; //prepare for next section
		    }
			if (inputAsStr.endsWith(" "))
		    inputAsStr = inputAsStr.substring(0,inputAsStr.length()-1) + ")";
		    System.out.println(inputAsStr);
		    return inputAsStr;
	}
	public static void main(String[] args) throws Exception{
		String dossier = "http://decipher-research.eu/dossiers/1533";
//		String queryURI = "http://localhost:8088/openrdf-sesame/repositories/RecommenderDemo?query=";
		String queryURI = "http://storyscope5.ssl.co.uk:8080/openrdf-sesame/repositories/test?query=";
		SectionATMS sATMS = new SectionATMS(queryURI, dossier);
		sATMS.Map2String(sATMS.parseSparqlXML(sATMS.getEventProperties()));
		
	}	
}
