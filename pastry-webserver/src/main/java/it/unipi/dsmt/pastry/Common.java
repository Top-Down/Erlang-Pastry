package it.unipi.dsmt.pastry;

import java.io.File;

import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

public class Common {
	public static String webServerIp = "10.2.1.4";
	public static Long cleanerPeriod = 5 * 60L;
	
    private Common() {
        throw new UnsupportedOperationException("Utility class");
    }
    
    public static String getFilesDir() {
    	String tomcatBase = System.getProperty("catalina.base");

        // dir where files pulled from pastry will be saved
        String webAppsPath = tomcatBase + File.separator + "webapps" + File.separator;
        String filesPath = webAppsPath + "pastry" + File.separator + "files" + File.separator;
        File filesDirectory = new File(filesPath);
        if(!filesDirectory.exists()) filesDirectory.mkdirs();
        
        return filesPath;
    }
}
