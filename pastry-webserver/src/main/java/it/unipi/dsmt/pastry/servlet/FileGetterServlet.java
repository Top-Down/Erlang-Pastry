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
    private JavaErlangConnector connector;
    
    public FileGetterServlet() throws IOException {
    	connector = new JavaErlangConnector(
    		"hello_server@127.0.0.1",
    		"CoordinatorMailBox",
    		"pastry",
    		"webserver@127.0.0.1",
    		"WebserverMailBox"
        );
    }

    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        response.setContentType("application/json");
        response.setCharacterEncoding("UTF-8");

        // Initialize the logger
        Logger logger = Logger.getLogger(getClass().getName());
        logger.info("test");

        //TODO: actually get the content and represent it as JSON
        connector.find_all();

        PrintWriter out = response.getWriter();
        out.print("{\"names\":[\"test.txt\", \"test2.txt\"]}");
        out.flush();
    }
}
