package it.unipi.dsmt.pastry.servlet;

import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;

import it.unipi.dsmt.SemaphoreManager;
import it.unipi.dsmt.pastry.Common;

@WebServlet(name="DownloadServlet", value="/download")
public class DownloadServlet extends HttpServlet {
    private static final long serialVersionUID = 1L;

    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String directoryPath = Common.getFilesDir();
        String fileName = request.getParameter("fileName");
        File file = new File(directoryPath + fileName);

        OutputStream outStream = null;
        
        try {
            SemaphoreManager.getSemaphore().acquire();
            if(file.exists() && !file.isDirectory()) {
            	response.setContentType("application/octet-stream");
                response.setHeader("Content-Disposition", "attachment;filename=\"" + file.getName() + "\"");
                outStream = response.getOutputStream();
                Files.copy(file.toPath(), outStream);
                outStream.flush();
            }
            else {
                response.setContentType("application/json");
                response.setCharacterEncoding("UTF-8");
                response.getWriter().write("{\"status\": false, \"message\": \"File not found\"}");
            }
        }
        catch (InterruptedException e) {
            e.printStackTrace();
        }
        finally {
        	SemaphoreManager.getSemaphore().release();
            if(outStream != null) {
                try {
                    outStream.close();
                }
                catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }
}
