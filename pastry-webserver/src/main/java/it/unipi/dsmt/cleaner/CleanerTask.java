package it.unipi.dsmt.cleaner;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.concurrent.TimeUnit;
import it.unipi.dsmt.SemaphoreManager;
import it.unipi.dsmt.pastry.Common;

public class CleanerTask implements Runnable {
    @Override
    public void run() {
    	// find all files in webserver older than cleanerPeriod seconds and eliminate them
        String directoryPath = Common.getFilesDir();
        long xSecondsAgo = TimeUnit.SECONDS.toMillis(Common.cleanerPeriod);
        long currentTime = System.currentTimeMillis();

        File directory = new File(directoryPath);
        File[] files = directory.listFiles();

        if(files != null) {
            try {
                SemaphoreManager.getSemaphore().acquire();
                for(File file : files) {
                    try {
                        Path filePath = file.toPath();
                        BasicFileAttributes attrs = Files.readAttributes(filePath, BasicFileAttributes.class);
                        long fileTime = attrs.lastModifiedTime().toMillis();
                        if(currentTime - fileTime >= xSecondsAgo) file.delete();
                    }
                    catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }
            catch (InterruptedException e) {
                e.printStackTrace();
            }
            finally {
                SemaphoreManager.getSemaphore().release();
            }
        }
    }
}
