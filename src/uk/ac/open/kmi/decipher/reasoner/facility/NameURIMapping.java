package uk.ac.open.kmi.decipher.reasoner.facility;

import java.io.IOException;
import java.net.URLEncoder;
import java.util.Iterator;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.methods.GetMethod;
import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;


public class NameURIMapping {

//	private static final String FB_API_URL ="https://api.freebase.com/api/service/search?query=";
	private static final String FB_API_URL ="https://www.googleapis.com/freebase/v1/search?query=";
	
	private String freebaseQuery(String name){
		
		HttpClient client = new HttpClient(); 
	//  client.getHostConfiguration().setProxy("wwwcache.open.ac.uk", 80);
		GetMethod queryServicesMethod = null;
		String queryResult="";
		try {
			queryServicesMethod = new GetMethod(FB_API_URL + URLEncoder.encode(name, "UTF-8"));
			queryServicesMethod.setRequestHeader("Accept", "application/json");
			client.executeMethod(queryServicesMethod);
			queryResult=queryServicesMethod.getResponseBodyAsString();	    	
            System.out.println(queryResult);
		} catch (HttpException httpe) {
			// TODO Auto-generated catch block
			httpe.printStackTrace();
		} catch (IOException ioe) {
			// TODO Auto-generated catch block
			ioe.printStackTrace();
		}finally{
			queryServicesMethod.releaseConnection();
		}
		return queryResult;
	}
		
	public String mapName2URI(String name){
		String queryRes = this.freebaseQuery(name);
		String uri="http://rdf.freebase.com/ns";
		
		try {
			JSONObject jsonObj = new JSONObject(queryRes);
			JSONArray result = jsonObj.getJSONArray("result");
			JSONObject keyInfo = result.getJSONObject(0);
			if (keyInfo !=null)
			{   Object id = keyInfo.get("id");
			    if (id!=null)
				  uri += id.toString().replace("en.", "en/");
			    else //mid at index 0
			      {   
			    	 Object mid = keyInfo.get("mid");
			         uri += mid.toString().replace("m.", "m/");
			      }
			}

		} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		System.out.println(uri);
		return uri;
	}
	
	public static void main(String[] args) throws Exception{
		NameURIMapping numapping= new NameURIMapping();
		numapping.mapName2URI("denis mahon");	
	}				
	
}
