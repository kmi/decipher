package uk.ac.open.kmi.decipher.reasoner;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.apache.commons.lang.StringEscapeUtils;

import uk.ac.open.kmi.decipher.repository.SPARQLClient;
import uk.ac.open.kmi.decipher.util.QueryResult;
import uk.ac.open.kmi.decipher.util.QueryRow;
import uk.ac.open.kmi.decipher.util.SPARQLQueryResultParser;
import uk.ac.open.kmi.decipher.util.SPARQLUtil;

class ValueComparator implements Comparator<String> {

    Map<String, Integer> base;
    public ValueComparator(Map<String, Integer> base) {
        this.base = base;
    }

    // Note: this comparator imposes orderings that are inconsistent with equals.    
    public int compare(String a, String b) {
        if (base.get(a) >= base.get(b)) {
            return -1;
        } else {
            return 1;
        } // returning 0 would merge keys
    }
}


public class EventSettingCalculator {

	static private final String CONNECTOR="::";
	String queryEndPoint;
	String dossierID;

	public EventSettingCalculator(String repQueryURI, String dossier) {
		this.queryEndPoint = repQueryURI;
		this.dossierID = dossier;
	}

	public Set<String> clusterSettings() {

		List<String> pairs = new ArrayList<String>();
		Map<String, Integer> pairs_count = new HashMap<String,Integer>();
        ValueComparator bvc =  new ValueComparator(pairs_count);
        TreeMap<String,Integer> sorted_pairs_count = new TreeMap<String,Integer>(bvc);

		String sparqlResult = getEventTimeLocation(); 
		QueryResult qr = SPARQLQueryResultParser.parse(sparqlResult);	
		List<QueryRow> rows = qr.getQueryRows();
		Iterator<QueryRow> rows_iter = rows.iterator();

		while (rows_iter.hasNext()){
			QueryRow row = rows_iter.next();			
			
			String tmp_sTime = row.getValue("sTime");
			String sTime = tmp_sTime.substring(0,tmp_sTime.indexOf("-"));			    
			String location = row.getValue("loc");
//			System.out.println(sTime + "   " + location);
            if (!pairs.contains(sTime + CONNECTOR + location))
            {
            	pairs.add(sTime + CONNECTOR + location);
            	pairs_count.put(sTime + CONNECTOR + location, Integer.valueOf(1));
            }
            else
            {
            	Integer count = pairs_count.get(sTime + CONNECTOR + location);
            	int newCount = count.intValue()+1;
            	pairs_count.put(sTime + CONNECTOR + location, Integer.valueOf(newCount));
            }
            
		}
		//sorting the pairs according to their counts
	       System.out.println("unsorted map: "+ pairs_count);
	       sorted_pairs_count.putAll(pairs_count);
	       System.out.println("sorted map: "+sorted_pairs_count);
	      
	   Set<String> keys=sorted_pairs_count.keySet();
		for (String key:keys)
			System.out.println(key);
		return keys;
	}

	private String getEventTimeLocation(){
		String query = SPARQLUtil.PREFIX 
				+ "SELECT ?sTime ?loc "//?loc_FBL"
				+ "WHERE {<" + dossierID +"> curate:containsEvent ?event. "
				+ "?event curate:hasStartTime ?sTime. "
				+ "?event curate:hasLocation ?loc."
//				+ "			optional {?loc rdfs:label ?loc_FBL.}"
				+ "}";
		return queryRDF(query);		
	}
	
	private String queryRDF(String queryStr){		 
		 SPARQLClient sClient = new SPARQLClient();
		 String results = sClient.httpQuery(queryEndPoint, queryStr, "application/sparql-results+xml");
		 return results;
		}

	public static void main(String[] args) throws Exception{
		String dossier = "http://decipher-research.eu/dossiers/1533";
//		String queryURI = "http://localhost:8088/openrdf-sesame/repositories/RecommenderDemo?query=";
		String queryURI = "http://storyscope5.ssl.co.uk:8080/openrdf-sesame/repositories/test?query=";
//		String plot = "http://mckm219286.open.ac.uk/decipher/trunk/decipher-3/plot/4413";
        EventSettingCalculator esc= new EventSettingCalculator(queryURI, dossier);
        esc.clusterSettings();
	}
}

