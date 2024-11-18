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
    private Common() {
        throw new UnsupportedOperationException("Utility class");
    }
    
    public static String getFileDir() {
    	String tomcatBase = System.getProperty("catalina.base");

        // Directory where the file will be saved
        String directoryPath = tomcatBase + File.separator + "webapps" + File.separator + "pastry" + File.separator + "files" + File.separator;
        File directory = new File(directoryPath);
        if(!directory.exists()) directory.mkdirs();
        
        return directoryPath;
    }
}
