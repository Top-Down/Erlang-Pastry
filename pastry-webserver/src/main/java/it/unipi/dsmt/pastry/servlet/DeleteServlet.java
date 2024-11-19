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
		int status = 1;
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
			boolean ret = connector.delete(fileName);
			if(ret) status = 0;
		}
		catch(Exception e) {
			status = -1;
			e.printStackTrace();
		}

		if(status == 0) 
            response.setStatus(HttpServletResponse.SC_OK);
		else if(status == 1) {
			response.setStatus(HttpServletResponse.SC_NOT_FOUND);
		}
		else
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
	}
}
