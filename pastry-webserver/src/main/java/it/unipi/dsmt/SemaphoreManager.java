package it.unipi.dsmt;

import java.util.concurrent.Semaphore;

// used to synchronize the access to files directory
public class SemaphoreManager {
    private static volatile Semaphore semaphore;

    public static Semaphore getSemaphore() {
        if(semaphore == null) {
            synchronized(SemaphoreManager.class) {
                if(semaphore == null) {
                    semaphore = new Semaphore(1);
                }
            }
        }
        return semaphore;
    }
}
