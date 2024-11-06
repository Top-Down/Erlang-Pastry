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
    
    public static void createFileFromBinaryData(String fileName, byte[] binaryData) throws Exception {
        // directory where the file will be saved
        String directoryPath = "files";
        File directory = new File(directoryPath);
        if(!directory.exists()) directory.mkdirs();

        // file path
        File file = new File(directoryPath + File.separator + fileName);

        // write  binary data to file
        FileOutputStream fos = new FileOutputStream(file);
        fos.write(binaryData);
        fos.close();
        
        System.out.println("File created successfully: " + file.getAbsolutePath());
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
