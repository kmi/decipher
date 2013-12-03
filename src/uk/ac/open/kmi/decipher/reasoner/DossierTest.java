package uk.ac.open.kmi.decipher.reasoner;

import uk.ac.open.kmi.decipher.repository.SPARQLClient;
import uk.ac.open.kmi.decipher.util.SPARQLUtil;

public class DossierTest {

    String queryEndPoint;
    String dossierID;
 
	public DossierTest(String repQueryURI, String dossID) {
		this.queryEndPoint = repQueryURI;
		this.dossierID = dossID;
	}	
	String queryRelated ="  ?plotDesc rdf:type curate:Related."
					+ "   ?plotDesc curate:definesEventSituationType ?EveSituationType. "
		            + "   ?EveSituationType curate:classifiesEventSituation ?event."
//					+ "   {?EveSituationType rdf:type curate:RelatedEvents. ?EveSituationType curate:classifiesEventSituation ?event.} UNION"
//					+ "   {?EveSituationType rdf:type curate:RelatedFrom. ?EveSituationType curate:classifiesEventSituation ?event.} UNION"
//					+ "   {?EveSituationType rdf:type curate:RelatedTo. ?EveSituationType curate:classifiesEventSituation ?event.}"
					;
	
	String queryInfluenced ="  ?plotDesc rdf:type curate:Influenced."
			+ "   ?plotDesc curate:definesEventSituationType ?EveSituationType. "
            + "   ?EveSituationType curate:classifiesEventSituation ?event."
//			+ "   {?EveSituationType rdf:type curate:InfluencedFrom. ?EveSituationType curate:classifiesEventSituation ?event.} UNION"
//			+ "   {?EveSituationType rdf:type curate:InfluencedTo. ?EveSituationType curate:classifiesEventSituation ?event.} UNION"
//			+ "   {?EveSituationType rdf:type curate:SourceOfInfluence. ?EveSituationType curate:classifiesEventSituation ?event.} UNION"
//			+ "   {?EveSituationType rdf:type curate:ConsquenceOfInfluence. ?EveSituationType curate:classifiesEventSituation ?event.}"
			;
	
	
	String queryInspired =" FILTER (?plotDescType = curate:Inspired)."
					+ "   ?plotDesc curate:definesEventSituationType ?EveSituationType. "
		            + "   ?EveSituationType curate:classifiesEventSituation ?event."
//					+ "   {?EveSituationType rdf:type curate:InspiredFrom. ?EveSituationType curate:classifiesEventSituation ?event.} UNION"
//					+ "   {?EveSituationType rdf:type curate:InspiredTo. ?EveSituationType curate:classifiesEventSituation ?event.}"
					;

	String queryMotivated =" ?plotDesc rdf:type curate:Motivated."
					+ "   ?plotDesc curate:definesEventSituationType ?EveSituationType. "
		            + "   ?EveSituationType curate:classifiesEventSituation ?event."
//					+ "   {?EveSituationType rdf:type curate:MotivatedFrom. ?EveSituationType curate:classifiesEventSituation ?event.} UNION"
//					+ "   {?EveSituationType rdf:type curate:MotivatedTo. ?EveSituationType curate:classifiesEventSituation ?event.}"
					;
	
	String queryInReactionTo =" ?plotDesc rdf:type curate:InReactionTo."
					+ "   ?plotDesc curate:definesEventSituationType ?EveSituationType. "
		            + "   ?EveSituationType curate:classifiesEventSituation ?event."
//					+ "   {?EveSituationType rdf:type curate:SourceOfReaction. ?EveSituationType curate:classifiesEventSituation ?event.} UNION"
//					+ "   {?EveSituationType rdf:type curate:ConsequenceOfReaction. ?EveSituationType curate:classifiesEventSituation ?event.}"
					;

	private String getEventAgents (){
			
		 String queryEventProperities = SPARQLUtil.PREFIX 
					+ "SELECT ?agent (COUNT(?agent) AS ?count)"
					+ "WHERE { ?event curate:hasAgent ?agent. "
					+ "{ " 
					+ "SELECT DISTINCT ?event "
					    + "WHERE {<" + dossierID +"> curate:isStoryPlottedBy ?plot. "
						+ "?plot curate:containsPlotDescription ?plotDesc. "
//						+ " ?plotDesc rdf:type ?plotDescType. "		 		    
						+ queryRelated
					+	"} "
					+	"} "
					+ "} GROUP BY ?agent";	 
		 
	System.err.println(queryEventProperities);	
		 SPARQLClient sClient = new SPARQLClient();
		 String results = sClient.httpQuery(queryEndPoint, queryEventProperities, "application/sparql-results+xml");
		 System.out.println(results);
		 return results;
		}
	
	public static void main(String[] args) throws Exception{
		String dossID = "http://mckm219286.open.ac.uk/decipher/trunk/decipher-3/cstory/2727";
		String queryURI = "http://localhost:8080/openrdf-sesame/repositories/Decipher?query=";
		DossierTest dt= new DossierTest(queryURI,dossID);
		dt.getEventAgents();
	}
	
}
