package it.unipi.dsmt.pastry.servlet;

import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.logging.Logger;

import it.unipi.dsmt.javaerlang.JavaErlangConnector;
import it.unipi.dsmt.pastry.Common;

@WebServlet(name="FileGetterServlet", value="/allfiles")
public class FileGetterServlet extends HttpServlet {
    private static final long serialVersionUID = 1L;
    
    private static String buildJson(ArrayList<String> names) {
        StringBuilder jsonBuilder = new StringBuilder();
        jsonBuilder.append("{\"names\":[");
        
        for(int i = 0; i < names.size(); i++) {
            jsonBuilder.append("\"").append(names.get(i)).append("\"");
            if(i < names.size() - 1) jsonBuilder.append(", ");
        }
        
        jsonBuilder.append("]}");
        return jsonBuilder.toString();
    }
    
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        response.setContentType("application/json");
        response.setCharacterEncoding("UTF-8");
        
        long threadId = Thread.currentThread().threadId();
        JavaErlangConnector connector = new JavaErlangConnector(
    		"control1_node1@" + Common.webServerIp,
    		"control1_node1",
    		"pastry",
    		"webserver_" + String.valueOf(threadId) + "@" + Common.webServerIp,
    		"WebserverMailBox_" + String.valueOf(threadId)
        );

        String names = "{\"names\": []}";
        try {
        	names = buildJson(connector.find_all());
		}
        catch (Exception e) {
			e.printStackTrace();
		}

        try(PrintWriter out = response.getWriter()) {
            out.print(names);
        }
    }
}
