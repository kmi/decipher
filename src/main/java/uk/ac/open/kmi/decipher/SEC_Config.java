package uk.ac.open.kmi.decipher;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.apache.log4j.Logger;


public class SEC_Config {
	
	   private final static Logger log = Logger.getLogger(SEC_Config.class);	  	   
	   private static Properties properties;
	   private static SEC_Config config = null;

	   static {
		   config = new SEC_Config();
	   }

	   private SEC_Config ()  {
	    	InputStream in= null; 
	    	try {
	            in = this.getClass().getClassLoader().getResourceAsStream("config/config.properties");
	            properties = new Properties();
	            properties.load(in);
	        } catch (IOException e) {
	            e.printStackTrace();
	        }
	        finally{
	            if(in!=null) try {
	                in.close();
	                in=null;
	            } catch (IOException e) {
	                e.printStackTrace();
	            }
	        }

	    }
	    public static SEC_Config getConfig () {
	            return config;
	    }
	    
	    public String getProperty (String propName) {
	        return properties.getProperty(propName, "Null");
	    }      
}

