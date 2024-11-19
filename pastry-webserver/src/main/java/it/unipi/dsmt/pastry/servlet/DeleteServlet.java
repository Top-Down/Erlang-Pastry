package it.unipi.dsmt.pastry.servlet;

import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import it.unipi.dsmt.javaerlang.JavaErlangConnector;
import it.unipi.dsmt.pastry.Common;

@WebServlet(name="DeleteServlet", value="/delete")
public class DeleteServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;

	protected void doDelete(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		boolean status = false;
		String fileName = request.getParameter("fileName");
		
		long threadId = Thread.currentThread().threadId();
        JavaErlangConnector connector = new JavaErlangConnector(
    		"node1@" + Common.webServerIp,
    		"node1",
    		"pastry",
    		"webserver_" + String.valueOf(threadId) + "@" + Common.webServerIp,
    		"WebserverMailBox_" + String.valueOf(threadId)
        );
		
		try {
			status = connector.delete(fileName);
		}
		catch(Exception e) {
			status = false;
			e.printStackTrace();
		}

        response.setContentType("application/json");
        response.setCharacterEncoding("UTF-8");
        response.getWriter().write("{\"status\": " + status + "}");
	}
}
