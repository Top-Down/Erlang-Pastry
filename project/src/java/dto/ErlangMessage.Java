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

public class ErlangMessageDTO {    
    OtpErlangTuple senderAddr;
    String senderName;
    OtpErlangRef msgId;
    OtpErlangLong timestamp;
    OtpErlangAtom operation;
    OtpErlangTuple content;

    public ErlangMessageDTO(
        OtpErlangTuple senderAddr,
        String senderName,
        OtpErlangRef msgId,
        OtpErlangLong timestamp) {
            
        this.senderAddr = senderAddr;
        this.senderName = senderName;
        this.msgId = msgId;
        this.timestamp = timestamp;
    }

    public OtpErlangTuple getSenderAddr() {
        return senderAddr;
    }

    public void setSenderAddr(OtpErlangTuple senderAddr) {
        this.senderAddr = senderAddr;
    }

    public String getSenderName() {
        return senderName;
    }

    public void setSenderName(String senderName) {
        this.senderName = senderName;
    }

    public OtpErlangRef getMsgId() {
        return msgId;
    }

    public void setMsgId(OtpErlangRef msgId) {
        this.msgId = msgId;
    }

    public OtpErlangLong getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(OtpErlangLong timestamp) {
        this.timestamp = timestamp;
    }

    public OtpErlangAtom getOperation() {
        return content.elementAt(0);
    }

    public OtpErlangTuple getContent() {
        return content;
    }

    public void setContent(OtpErlangTuple content) {
        this.content = content;
    }

    public OtpErlangObject getContentElement(int i) {
        if (i >= 0 && i < content.arity()) {
            return content.elementAt(i);
        } else {
            throw new IndexOutOfBoundsException("Index " + i + " is out of bounds for content tuple with arity " + content.arity());
        }
    }
}
