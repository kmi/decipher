package uk.ac.open.kmi.decipher;

import java.io.File;
import java.util.Iterator;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.openrdf.repository.RepositoryException;

import uk.ac.open.kmi.decipher.reasoner.CombPlotRecommender;
import uk.ac.open.kmi.decipher.reasoner.EventCluster;
import uk.ac.open.kmi.decipher.reasoner.EventEventRecommender;
import uk.ac.open.kmi.decipher.reasoner.EventRecommender;
import uk.ac.open.kmi.decipher.reasoner.EventSettingCalculator;
import uk.ac.open.kmi.decipher.reasoner.NarrativeRecommender;
import uk.ac.open.kmi.decipher.reasoner.PlotCluster;
import uk.ac.open.kmi.decipher.reasoner.PlotEventCluster;
import uk.ac.open.kmi.decipher.reasoner.PlotRecommender;
import uk.ac.open.kmi.decipher.reasoner.facility.LispBridge;
import uk.ac.open.kmi.decipher.repository.RDFRepositoryConnector;


//@Path("/dossier/{dossierID}")
@Path("/{action}")
public class SECservice {

	private final static Logger log = Logger.getLogger(SECservice.class);

	SEC_Config config = SEC_Config.getConfig();
	LispBridge lispATMS = LispBridge.getInstance();

//	RDFRepositoryConnector RDF_rep_conn = null;

	public SECservice() {				
		log.setLevel(Level.DEBUG);
	/*	try {
			RDF_rep_conn = new RDFRepositoryConnector(config.getProperty("REPOSITORY_URL"), config.getProperty("REPOSITORY_NAME"));
		} catch (RepositoryException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	 */		
	}
	
	@GET
	@Produces("application/xml")
//	public Response doGet(@PathParam("dossierID") String sID, @PathParam("action") String action, @Context UriInfo ui){
	public Response doGet(@PathParam("action") String action, @QueryParam("dossierID") String sID, @QueryParam("eventID") String eID, @QueryParam("storyID") String stID, @Context UriInfo ui){
        Response resp = null;
		//String dossierID = config.getProperty("DOSSIER_NS") + sID;
		String dossierID = sID;
		String eventID = eID;
		String storyID = stID;
		String repQueryURI = config.getProperty("REP_QUERY_ENDPOINT");
		String plotCluNum = config.getProperty("PLOT_CLUSTER_NUMBER");
		String eventCluNum = config.getProperty("EVNET_CLUSTER_NUMBER");
		
		// clusterEvents is not seperatly used yet
	/*	if (action.equalsIgnoreCase("clusterEvents")){
			EventCluster eveCluster = new EventCluster(repQueryURI,dossierID);			
			eveCluster.clusterEvents(Integer.parseInt(eventCluNum));
			String response = eveCluster.getSettinginHTTPRepsonse();
			resp = Response.ok(response, MediaType.APPLICATION_XML).build();										
		}		
		else */
		if (action.equalsIgnoreCase("recommendEventEvents")){
			EventEventRecommender eveEventRecomm = new EventEventRecommender(repQueryURI, eventID);
			String recommendedEventEvents = eveEventRecomm.recommend();
			resp = Response.ok(recommendedEventEvents, MediaType.APPLICATION_XML).build();
		}
		else if (action.equalsIgnoreCase("recommendEvents")){
			EventRecommender eveRecomm = new EventRecommender(repQueryURI, dossierID, Integer.parseInt(eventCluNum));
			String recommendedEvents = eveRecomm.recommend();
			resp = Response.ok(recommendedEvents, MediaType.APPLICATION_XML).build();
		}
		else if (action.equalsIgnoreCase("recommendSections")){
			CombPlotRecommender combPlotRecomm = new CombPlotRecommender(repQueryURI, dossierID);
			String recommendedPlots = combPlotRecomm.recommend(Integer.parseInt(plotCluNum));
			resp = Response.ok(recommendedPlots, MediaType.APPLICATION_XML).build();
		}
		else if (action.equalsIgnoreCase("recommendBinarySections")){
			PlotRecommender plotRecomm = new PlotRecommender(repQueryURI, dossierID);
			String binaryPlots = plotRecomm.recommend();
			resp = Response.ok(binaryPlots, MediaType.APPLICATION_XML).build();
		}
		
		else if (action.equalsIgnoreCase("recommendRelatedSections")){
			PlotCluster plotRelated = new PlotCluster(repQueryURI, dossierID);
			String relatedPlots = plotRelated.recommend(Integer.parseInt(plotCluNum));
			resp = Response.ok(relatedPlots, MediaType.APPLICATION_XML).build();
		}
		
		else if (action.equalsIgnoreCase("recommendNarratives")){
			NarrativeRecommender narrRecommen= new NarrativeRecommender(repQueryURI, dossierID, storyID, lispATMS);
			String recommendedNarrs = narrRecommen.recommend();
			resp = Response.ok(recommendedNarrs, MediaType.APPLICATION_XML).build();
		}
		return resp;
	}
	@GET
	@Path("{cache_or_recalculate}")
	@Produces("application/xml")
//	public Response doGet(@PathParam("dossierID") String sID, @PathParam("action") String action, @Context UriInfo ui, String recalulate){
	public Response doGet(@PathParam("action") String action, @QueryParam("dossierID") String sID, @QueryParam("eventID") String eID, @QueryParam("storyID") String stID, @Context UriInfo ui, @PathParam("cache_or_recalculate") String mechanism){ 
        Response resp = null;		 
//		String dossierID = config.getProperty("DOSSIER_NS") + sID;
//		String dossierID = "http://decipher-research.eu/dossiers/1305";
		String dossierID = sID;
		String eventID = eID;
		String storyID = stID;
		String repQueryURI = config.getProperty("REP_QUERY_ENDPOINT");
		String plotCluNum = config.getProperty("PLOT_CLUSTER_NUMBER");
		String eventCluNum = config.getProperty("EVNET_CLUSTER_NUMBER");	
		/*
		if (action.equalsIgnoreCase("clusterEvents")){
			EventCluster eveCluster = new EventCluster(repQueryURI,dossierID);			
			eveCluster.clusterEvents(Integer.parseInt(eventCluNum));
			String response = eveCluster.getSettinginHTTPRepsonse();
			resp = Response.ok(response, MediaType.APPLICATION_XML).build();										
		}		
		else */ 
		if (action.equalsIgnoreCase("recommendEventEvents")){
			EventEventRecommender eveEventRecomm = new EventEventRecommender(repQueryURI, eventID);
			String recommendedEventEvents="<?xml version=\"1.0\" encoding=\"UTF-8\"?>;";
			if (mechanism.equalsIgnoreCase("cache"))
				recommendedEventEvents = eveEventRecomm.returnCacheOrEmpty();
			else if (mechanism.equalsIgnoreCase("recalculate"))
				recommendedEventEvents = eveEventRecomm.recommendEvents(eveEventRecomm.getAgents());
			resp = Response.ok(recommendedEventEvents, MediaType.APPLICATION_XML).build();
		}
		else if (action.equalsIgnoreCase("recommendEvents")){
			EventRecommender eveRecomm = new EventRecommender(dossierID);
			String recommendedEvents="<?xml version=\"1.0\" encoding=\"UTF-8\"?>;";
			if (mechanism.equalsIgnoreCase("cache"))
					recommendedEvents = eveRecomm.returnCacheOrEmpty();
			else if (mechanism.equalsIgnoreCase("recalculate"))
			{
			/*	EventCluster eveCluster = new EventCluster(repQueryURI, dossierID);
				Map<String, Set<String>> setting = eveCluster.clusterEvents(Integer.parseInt(eventCluNum));
				recommendedEvents = eveRecomm.recommendEvents(setting);
			*/     
				EventSettingCalculator eveSetCalculator = new EventSettingCalculator(repQueryURI, dossierID);
				Set<String> settingsSet = eveSetCalculator.clusterSettings();
				recommendedEvents = eveRecomm.recommendEvents(settingsSet);
			}
			else
				recommendedEvents = "<error>wrong URIs, please check with API supplier</error>";
			resp = Response.ok(recommendedEvents, MediaType.APPLICATION_XML).build();
		}
		else if (action.equalsIgnoreCase("recommendSections")){
			CombPlotRecommender combPlotRecomm = new CombPlotRecommender(repQueryURI, dossierID);
			String recommendedPlots="<?xml version=\"1.0\" encoding=\"UTF-8\"?>;";
			if (mechanism.equalsIgnoreCase("cache"))
				recommendedPlots = combPlotRecomm.returnCacheOrEmpty();
			else if (mechanism.equalsIgnoreCase("recalculate"))
				recommendedPlots = combPlotRecomm.recommendCombPlots(Integer.parseInt(plotCluNum));
			else
				recommendedPlots = "<error>wrong URIs, please check with API supplier</error>";
			resp = Response.ok(recommendedPlots, MediaType.APPLICATION_XML).build();
		}
		else if (action.equalsIgnoreCase("recommendBinarySections")){
			PlotRecommender plotRecomm = new PlotRecommender(repQueryURI, dossierID);
			String binaryPlots="<?xml version=\"1.0\" encoding=\"UTF-8\"?>;";
			if (mechanism.equalsIgnoreCase("cache"))
				binaryPlots = plotRecomm.returnCacheOrEmpty();
			else if (mechanism.equalsIgnoreCase("recalculate"))
				binaryPlots = plotRecomm.recommendPlots();
			else
				binaryPlots = "<error>wrong URIs, please check with API supplier</error>";
			resp = Response.ok(binaryPlots, MediaType.APPLICATION_XML).build();
		}		
		else if (action.equalsIgnoreCase("recommendRelatedSections")){
			PlotCluster plotRelated = new PlotCluster(repQueryURI, dossierID);
			String relatedPlots="<?xml version=\"1.0\" encoding=\"UTF-8\"?>;";
			if (mechanism.equalsIgnoreCase("cache"))
				relatedPlots = plotRelated.returnCacheOrEmpty();
			else if (mechanism.equalsIgnoreCase("recalculate"))
				relatedPlots = plotRelated.clusterPlots(Integer.parseInt(plotCluNum));
			else
				relatedPlots = "<error>wrong URIs, please check with API supplier</error>";
			resp = Response.ok(relatedPlots, MediaType.APPLICATION_XML).build();
		}		
		else if (action.equalsIgnoreCase("recommendNarratives")){
			NarrativeRecommender narrRecommen= new NarrativeRecommender(repQueryURI, dossierID, storyID, lispATMS);
			String recommendedNarrs="<?xml version=\"1.0\" encoding=\"UTF-8\"?>;";
			if (mechanism.equalsIgnoreCase("cache"))
				recommendedNarrs = narrRecommen.returnCacheOrEmpty();
			else if (mechanism.equalsIgnoreCase("recalculate"))
				recommendedNarrs = narrRecommen.recommendNarratives();
			else
				recommendedNarrs = "<error>wrong URIs, please check with API supplier</error>";
			resp = Response.ok(recommendedNarrs, MediaType.APPLICATION_XML).build();
		}
		return resp;
	}
/*	
	@GET
	@Path("/plot/{plotID}/{action}")
	@Produces("application/xml")
	public Response doGet(@PathParam("dossierID") String sID, @PathParam("plotID") String pID, @PathParam("action") String action, @Context UriInfo ui){
		Response resp = null;		 
		String dossierID = config.getProperty("DOSSIER_NS") + sID;
		String repQueryURI = config.getProperty("REP_QUERY_ENDPOINT");
		String plotCluNum = config.getProperty("PLOT_CLUSTER_NUMBER");
		String eventCluNum = config.getProperty("EVNET_CLUSTER_NUMBER");
		String plotID = config.getProperty("PLOT_NS") + pID;
		
		if (action.equalsIgnoreCase("recommendEvents")){
			EventRecommender eveRecomm = new EventRecommender(repQueryURI, dossierID, plotID,Integer.parseInt(eventCluNum));
			String recommendedEvents = eveRecomm.recommend();
			resp = Response.ok(recommendedEvents, MediaType.APPLICATION_XML).build();
		}
		else
			resp = Response.ok("No such opertation is supported for plot", MediaType.APPLICATION_XML).build();
		return resp;
		
	}

	@GET
	@Path("/plot/{plotID}/{action}/recalculate")
	@Produces("application/xml")
	public Response doGet(@PathParam("dossierID") String sID, @PathParam("plotID") String pID, @PathParam("action") String action){
		Response resp = null;		 
		String dossierID = config.getProperty("DOSSIER_NS") + sID;
		String repQueryURI = config.getProperty("REP_QUERY_ENDPOINT");
		String plotCluNum = config.getProperty("PLOT_CLUSTER_NUMBER");
		String eventCluNum = config.getProperty("EVNET_CLUSTER_NUMBER");
		String plotID = config.getProperty("PLOT_NS") + pID;
		
		if (action.equalsIgnoreCase("recommendEvents")){
			PlotEventCluster plotEveCluster = new PlotEventCluster(repQueryURI, dossierID, plotID);
			EventRecommender eveRecomm = new EventRecommender(dossierID);
			Map<String, Set<String>> setting = plotEveCluster.clusterEvents(Integer.parseInt(eventCluNum));
//			String recommendedEvents = eveRecomm.recommendEvents(setting);
//			resp = Response.ok(recommendedEvents, MediaType.APPLICATION_XML).build();
		}
		else
			resp = Response.ok("No such opertation is supported for plot", MediaType.APPLICATION_XML).build();
		return resp;		
	}
  */
}
