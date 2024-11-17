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
    
    public static void createFileFromBinaryData(String fileName, byte[] binaryData) throws Exception {
        String directoryPath = getFileDir();
        File file = new File(directoryPath + fileName);

        // Write binary data to file
        FileOutputStream fos = new FileOutputStream(file);
        fos.write(binaryData);
        fos.close();
    }
    
    public static ArrayList<String> getConfig(int configIndex) {
    	ArrayList<String> result = new ArrayList<>();
        try {
            // read JSON file and convert to List of Maps
            ObjectMapper objectMapper = new ObjectMapper();
            List<Map<String, String>> data = objectMapper.readValue(
            		new File("../../config/config.json"),
            		new TypeReference<List<Map<String, String>>>() {}
            		);

            if(configIndex >= 0 && configIndex < data.size()) {
                Map<String, String> map = data.get(configIndex);
                result.addAll(map.values());
            }
            else System.out.println("Index out of bounds");

        }
        catch (IOException e) {
            e.printStackTrace();
        }
        return result;
    }
}
