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
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import it.unipi.dsmt.javaerlang.JavaErlangConnector;

@WebServlet(name="UploadServlet", value="/upload")
@MultipartConfig
public class UploadServlet extends HttpServlet {
    private static final long serialVersionUID = 1L;
    private JavaErlangConnector connector;
    
    public UploadServlet() throws IOException {
    	connector = new JavaErlangConnector(
    		"hello_server@127.0.0.1",
    		"CoordinatorMailBox",
    		"pastry",
    		"webserver@127.0.0.1",
    		"WebserverMailBox"
        );
    }

    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        try {
            Part filePart = request.getPart("file");
            String fileName = Paths.get(filePart.getSubmittedFileName()).getFileName().toString();
            String uploadDir = getServletContext().getRealPath("") + File.separator + "uploads";
            
            File uploadDirFile = new File(uploadDir);
            if (!uploadDirFile.exists()) uploadDirFile.mkdirs();
            
            String filePath = uploadDir + File.separator + fileName;
            filePart.write(filePath);

            Path path = Paths.get(filePath);
            byte[] fileBytes = Files.readAllBytes(path);
            connector.store(fileName, fileBytes);

            response.setStatus(HttpServletResponse.SC_OK); // 200 OK
        }
        catch (Exception e) {
            e.printStackTrace();
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR); // 500
        }
    }
}
