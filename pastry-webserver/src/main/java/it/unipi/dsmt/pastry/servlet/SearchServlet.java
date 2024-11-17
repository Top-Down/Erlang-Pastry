package it.unipi.dsmt.pastry.servlet;

import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;

import it.unipi.dsmt.javaerlang.JavaErlangConnector;
import it.unipi.dsmt.pastry.Common;

@WebServlet(name="SearchServlet", value="/search")
public class SearchServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        boolean fileFound = false;
        long threadId = Thread.currentThread().threadId();
        JavaErlangConnector connector = new JavaErlangConnector(
    		"node1@127.0.0.1",
    		"node1",
    		"pastry",
    		"webserver_" + String.valueOf(threadId) + "@127.0.0.1",
    		"WebserverMailBox_" + String.valueOf(threadId)
        );

        try {
            String fileName = request.getParameter("query");
            byte[] binaryData = connector.find(fileName);
            
            if(binaryData.length > 0) {
                Common.createFileFromBinaryData(fileName, binaryData);
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
