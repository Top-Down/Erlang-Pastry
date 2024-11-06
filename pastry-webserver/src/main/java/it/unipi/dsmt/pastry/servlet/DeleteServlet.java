package it.unipi.dsmt.pastry.servlet;

import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import it.unipi.dsmt.javaerlang.JavaErlangConnector;


@WebServlet(name="DeleteServlet", value="/delete")
public class DeleteServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
    private JavaErlangConnector connector;
	
	public DeleteServlet() throws IOException {
		connector = new JavaErlangConnector(
    		"hello_server@127.0.0.1",
    		"CoordinatorMailBox",
    		"pastry",
    		"webserver@127.0.0.1",
    		"WebserverMailBox"
        );
	}

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		boolean status = false;
		String fileName = request.getParameter("query");
		
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
