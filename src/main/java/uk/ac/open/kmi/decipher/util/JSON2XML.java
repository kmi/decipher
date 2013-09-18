package uk.ac.open.kmi.decipher.util;

import java.util.Iterator;

import org.apache.commons.lang.StringEscapeUtils;
import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

public class JSON2XML {

	public static String transform(String jsonIn)
	{
		String res="<?xml version=\"1.0\"?><responseData mediaType=\"application/xml\"><body>";
	    res += transformJSON(jsonIn);
		res += "</body></responseData>";		    
	    return res;		

	
	}
	
	public static String transformJSON(String jsonIn)
    {
		String res = "";
//		System.out.println("in json is:" + jsonIn);
		if (jsonIn.startsWith("{"))
		 try {
			JSONObject jsonObj = new JSONObject(jsonIn);			
				Iterator keys = jsonObj.keys();
				while (keys.hasNext()) 
				{
					Object key = keys.next();	
					Object val = jsonObj.optJSONArray(key.toString());
					
					if (val == null) //value or object
					{
						val = jsonObj.get(key.toString());
						if (val != null)
							res += "<" + key.toString()	+ ">" + transformJSON(val.toString()) + "</" + key.toString() + ">";
					} else { // an array
						JSONArray valArray = (JSONArray) val;
						for (int i = 0; i < valArray.length(); i++) 
						{
							res += "<" + key.toString() + ">"
									+ transformJSON(valArray.get(i).toString())
									+ "</" + key.toString() + ">";
						}
					}
				}
							
		  return res;
		} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		else 
			return StringEscapeUtils.escapeXml(jsonIn);
		
		return null;
	}

	
	public static String transform_old(String jsonIn)
	{
		String res="<?xml version=\"1.0\"?><responseData><body>";
		System.out.println("in json is:" + jsonIn);
		try {
			JSONObject jsonObj = new JSONObject(jsonIn);
			Iterator keys = jsonObj.keys();
			while (keys.hasNext())
			{	Object key = keys.next();
			    Object val =jsonObj.optJSONArray(key.toString());
			    if (val == null)
			    {
			        val =jsonObj.get(key.toString());
			        res +="<" + key.toString()+">" + val.toString() +"</" + key.toString() +">";
			    }
			    else 
			    {
				    JSONArray valArray= (JSONArray)val;
			    	for (int i=0;i< valArray.length();i++)
			           res +="<" + key.toString()+">" + valArray.get(i).toString() +"</" + key.toString() +">";
					
			    }
					
			}
		    res += "</body></responseData>";		    
		    return res;		
		} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}		 	
	  return null;
	}

}
