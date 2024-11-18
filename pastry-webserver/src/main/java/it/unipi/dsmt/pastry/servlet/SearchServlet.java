package it.unipi.dsmt.pastry.servlet;

import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import it.unipi.dsmt.javaerlang.JavaErlangConnector;
import it.unipi.dsmt.pastry.Common;

@WebServlet(name="SearchServlet", value="/search")
public class SearchServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	
    private void createFileFromBinaryData(String fileName, byte[] binaryData) throws Exception {
        String directoryPath = Common.getFileDir();
        File file = new File(directoryPath + fileName);

        // Write binary data to file
        FileOutputStream fos = new FileOutputStream(file);
        fos.write(binaryData);
        fos.close();
    }

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        boolean fileFound = false;
        long threadId = Thread.currentThread().threadId();
        JavaErlangConnector connector = new JavaErlangConnector(
    		"node1@10.2.1.4",
    		"node1",
    		"pastry",
    		"webserver_" + String.valueOf(threadId) + "@10.2.1.4",
    		"WebserverMailBox_" + String.valueOf(threadId)
        );

        try {
            String fileName = request.getParameter("query");
            byte[] binaryData = connector.find(fileName);
            
            if(binaryData.length > 0) {
                createFileFromBinaryData(fileName, binaryData);
                fileFound = true;
            }
        }
        catch (Exception e) {
        	e.printStackTrace();
        }

        response.setContentType("application/json");
        response.setCharacterEncoding("UTF-8");
        response.getWriter().write("{\"status\": " + fileFound + "}");
    }

}
