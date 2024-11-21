package it.unipi.dsmt.pastry.servlet;

import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.MultipartConfig;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.Part;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import it.unipi.dsmt.javaerlang.JavaErlangConnector;
import it.unipi.dsmt.pastry.Common;

@WebServlet(name="UploadServlet", value="/upload")
@MultipartConfig
public class UploadServlet extends HttpServlet {
    private static final long serialVersionUID = 1L;
    
    protected void doPut(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
    	long threadId = Thread.currentThread().threadId();
        JavaErlangConnector connector = new JavaErlangConnector(
    		"control1_node1@" + Common.webServerIp,
    		"control1_node1",
    		"pastry",
    		"webserver_" + String.valueOf(threadId) + "@" + Common.webServerIp,
    		"WebserverMailBox_" + String.valueOf(threadId)
        );
        
        try {
            Part filePart = request.getPart("file");
            String fileName = Paths.get(filePart.getSubmittedFileName()).getFileName().toString();
            
            // read bytes directly from input stream
            InputStream inputStream = filePart.getInputStream();
            byte[] fileBytes = inputStream.readAllBytes();
            
            connector.store(fileName, fileBytes);
            response.setStatus(HttpServletResponse.SC_OK);
        }
        catch (Exception e) {
            e.printStackTrace();
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }
}
