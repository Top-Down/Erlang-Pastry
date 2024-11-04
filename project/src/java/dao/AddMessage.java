package it.unipi.dsmt;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import it.unipi.dsmt.javaee.lab_06.entity.Beer;
import it.unipi.dsmt.javaee.lab_06.dto.BeerDTO;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

public class AddMessage {

    public static void init_msg() {
        try {
            OtpErlangString fileNameErl = new OtpErlangString(fileName);
            OtpErlangBinary fileDataErl = new OtpErlangBinary(fileData);

            OtpErlangRef msgId = new OtpErlangRef();
            OtpErlangLong timestamp = new OtpErlangLong(System.currentTimeMillis());

            OtpErlangTuple selfInfo = new OtpErlangTuple(new OtpErlangObject[]{
                this.selfAddr, this.selfName
            });
            OtpErlangTuple storeFileNameTuple = new OtpErlangTuple(new OtpErlangObject[]{
                new OtpErlangAtom("store"), fileNameErl
            });
            OtpErlangTuple msg = new OtpErlangTuple(new OtpErlangObject[]{
                selfInfo, msgId, timestamp, storeFileNameTuple
            });
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static List<BeerDTO> search(String keyword){
        return beerList.stream()
                .filter(beer -> beer.getName().toLowerCase().contains(keyword.toLowerCase()))
                .map(beer -> new BeerDTO(beer.getName(), beer.getLink()))
                .collect(Collectors.toList());
    }

}
