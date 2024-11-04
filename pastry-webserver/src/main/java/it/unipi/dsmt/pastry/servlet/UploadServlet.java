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

@WebServlet(name="UploadServlet", value="/upload")
@MultipartConfig
public class UploadServlet extends HttpServlet {
    private static final long serialVersionUID = 1L;

    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String message;
        try {
            Part filePart = request.getPart("file");
            String fileName = Paths.get(filePart.getSubmittedFileName()).getFileName().toString();
            String uploadDir = getServletContext().getRealPath("") + File.separator + "uploads";
            
            File uploadDirFile = new File(uploadDir);
            if (!uploadDirFile.exists()) {
                uploadDirFile.mkdirs();
            }
            
            String filePath = uploadDir + File.separator + fileName;
            filePart.write(filePath);

            // Read all the binary data from the file
            Path path = Paths.get(filePath);
            byte[] fileBytes = Files.readAllBytes(path);

            // TODO: Pass the binary data to Erlang
            // OtpErlangBinary binary = new OtpErlangBinary(fileBytes);
            // MyJavaClass.processFile(fileName, binary);

            message = "File " + filePath + " uploaded and read successfully with size: " + String.valueOf(fileBytes.length);
        } catch (Exception e) {
            message = "Upload failed, an error occurred: " + e.getMessage();
        }

        request.setAttribute("message", message);
        request.getRequestDispatcher("/upload.jsp").forward(request, response);
    }
}
