package uk.ac.open.kmi.decipher.reasoner;

import java.io.File;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import uk.ac.open.kmi.decipher.repository.SPARQLClient;
import uk.ac.open.kmi.decipher.util.FileUtil;
import uk.ac.open.kmi.decipher.util.QueryResult;
import uk.ac.open.kmi.decipher.util.QueryRow;
import uk.ac.open.kmi.decipher.util.SPARQLQueryResultParser;
import uk.ac.open.kmi.decipher.util.SPARQLUtil;

public class CombPlotRecommender {

	private static final String PLOT_ROOT_ELEMENT = "story_section";//"plot_element";

		private String queryEndPoint;
	    private String dossierID;
	    private String cacheFileName;
//		public static final String ff_queryEndPoint ="http://factforge.net/sparql?query=";
	    private static final String TEMP_DIR = File.separator + "data" + File.separator + "Decipher" + File.separator +"tmp"; 
//	    private static final String TEMP_DIR = File.separator + "Decipher" + File.separator +"tmp"; 
	    
		public CombPlotRecommender(String repQueryURI, String dossID) {
			super();
			this.queryEndPoint = repQueryURI;
			this.dossierID = dossID;
			this.cacheFileName = TEMP_DIR + File.separator + dossierID.substring(dossierID.lastIndexOf("/")+1) +"_recommendedCombPlots.xml";
		}	
		
		public String returnCacheOrEmpty(){
			File cacheFile = new File(cacheFileName);
			if(!cacheFile.exists())
			{
				String respStr ="<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?> <"+ PLOT_ROOT_ELEMENT+"s>"+"</"+ PLOT_ROOT_ELEMENT+"s>";
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
				String respStr =recommendCombPlots(clu_num);
		    	FileUtil.write2File(respStr, cacheFileName);
				return respStr;
				
			} else
			{
				String respStr = FileUtil.readFile(cacheFileName);
				return respStr;
			}
		}
		public String recommendCombPlots(int clusterNum){
			String combPlots = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?> <"+ PLOT_ROOT_ELEMENT+"s>";
			PlotRecommender plotRecomm = new PlotRecommender(queryEndPoint, dossierID);
			combPlots += plotRecomm.recommendAction();
			PlotCluster plotRelated = new PlotCluster(queryEndPoint, dossierID);
			combPlots += plotRelated.clusterAction(clusterNum);
			combPlots +="</"+ PLOT_ROOT_ELEMENT+"s>";
	    	FileUtil.write2File(combPlots, cacheFileName);
			return combPlots;
		}
		
		public static void main(String[] args) throws Exception{
			String dossID = "http://decipher-research.eu/dossiers/1305";
			String queryURI = "http://localhost:8080/openrdf-sesame/repositories/Decipher2?query=";
			CombPlotRecommender pr= new CombPlotRecommender(queryURI,dossID);
			pr.recommendCombPlots(25);
		}
}
