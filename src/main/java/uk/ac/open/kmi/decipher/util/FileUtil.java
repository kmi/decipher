package uk.ac.open.kmi.decipher.util;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

public class FileUtil {

	public static void testFile (String inFile){
		 BufferedReader br = null;
			try {
				String str;
				br=  new BufferedReader(new FileReader(inFile));
				while ((str= br.readLine()) !=null)						
			    System.out.println("the test File is:" + str);			
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}finally{
		        if(br!=null) {try{br.close();} catch(Exception e){} }
		    }	
   }
	public static String readFile (String inFile){
		BufferedReader br = null;
		String resultStr = "", str;

		try {
			br=  new BufferedReader(new InputStreamReader(new FileInputStream(inFile),"UTF-8"));
			while ((str= br.readLine()) !=null)
				resultStr +=str;

		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}finally{
			if(br!=null) {try{br.close();} catch(Exception e){} }
		}	
		return resultStr;
	}
   public static void write2File_O (String input, String inputFile){
		 BufferedWriter bw = null;
			try {
				bw=  new BufferedWriter(new FileWriter(new File(inputFile)));
			    bw.write(input);
			    
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}finally{
		        if(bw!=null) {try{bw.flush();bw.close();} catch(Exception e){} }
		    }
			
   }
   
   public static void write2File (String input, String inputFile){
		 BufferedWriter bw = null;
			try {
				bw=  new BufferedWriter(new OutputStreamWriter(new FileOutputStream(inputFile),"UTF-8"));
			    bw.write(input);
			    
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}finally{
		        if(bw!=null) {try{bw.flush();bw.close();} catch(Exception e){} }
		    }
 }
   
   public static String convertStreamToString(InputStream inputStream)
	        throws IOException {
	    ByteArrayOutputStream baos = new ByteArrayOutputStream();
	    byte[] buffer = new byte[1024];
	    int length = 0;
	    while ((length = inputStream.read(buffer)) != -1) {
	        baos.write(buffer, 0, length);
	    }
	    return new String(baos.toByteArray());
	}


	public static String checkExistance(String fileName)
	{
	 File folder = new File(File.separator + "tmp");
	
	 if ( folder.isDirectory() == true ) 
	 {
		File[] docList = folder.listFiles();
		if ( docList != null ) 
		{	for ( File doc : docList ) 
         		 if (doc.getName().contains(fileName))
         			 return File.separator + "tmp" + File.separator + doc.getName();
		}
		else
			System.out.println("no files are in this directory");
	 }
	 else
			System.out.println("/tmp not a directory");
   	 
	 return null;
	}
}
