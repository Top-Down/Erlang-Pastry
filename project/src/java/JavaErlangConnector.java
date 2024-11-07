package it.unipi.dsmt.javaerlang;

import com.ericsson.otp.erlang.*;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import it.unipi.dsmt.javaerlang.dao.*;

public class JavaErlangConnector {   
    String pastryMailBox;
    String pastryName;
    String cookie;
    String selfName;
    String selfMailboxName;
    OtpNode node;
    OtpMbox mailBox;
    OtpErlangTuple selfAddr;

    public JavaErlangConnector(
        String pastryNameIn,
        String pastryMailBoxIn, 
        String cookieIn,
        String selfNameIn,
        String selfMailboxNameIn) throws IOException {

        this.pastryMailBox = pastryMailBoxIn;
        this.pastryName = pastryNameIn;
        this.cookie = cookieIn;
        this.selfName = selfNameIn;
        this.selfMailboxName = selfMailboxNameIn;

        this.node = new OtpNode(this.selfName, this.cookie);
        this.mailBox = node.createMbox(this.selfMailboxName);
        this.selfAddr = new OtpErlangTuple(new OtpErlangObject[]{
            new OtpErlangAtom(this.selfMailboxName), new OtpErlangAtom(this.selfName)
        });
    }
    
    public <T extends ErlangMessage> OtpErlangObject sendRecvMsg(
        String destMailBox,
        String destName,
        T message, 
        ArrayList<OtpErlangObject> content,
        Class<T> clazz) throws Exception {

        message.setMailbox(this.mailBox);
        message.setContent(content);
        message.wrapMessage(this.node, selfAddr, selfName);
        message.sendMessage(destMailBox, destName);

        T response = clazz.getDeclaredConstructor().newInstance();
        response.setMailbox(this.mailBox);
        response.receiveMessage();
        response.unwrapMessage();
        return response.getContent(message);
    }

    public byte[] find(String fileName) throws Exception {
    	FindMessage findMsg = new FindMessage();
        OtpErlangBinary file = (OtpErlangBinary)this.sendRecvMsg(
            this.pastryMailBox, 
            this.pastryName, 
            findMsg, 
            new ArrayList<>(List.of(new OtpErlangString(fileName))),
            FindMessage.class);
        return file.binaryValue();
    }

    public ArrayList<String> find_all() throws Exception {
    	FindAllMessage findAllMsg = new FindAllMessage();
        OtpErlangList fileList = (OtpErlangList) this.sendRecvMsg(
            this.pastryMailBox, 
            this.pastryName, 
            findAllMsg, 
            new ArrayList<>(List.of()),
            FindAllMessage.class);
        
        return (ArrayList<String>) Arrays.stream(fileList.elements())
                .map(elem -> ((OtpErlangString)elem).stringValue())
                .collect(Collectors.toList());
    }

    public boolean store(String fileName, byte[] fileData) throws Exception {
    	StoreMessage findStorageMsg = new StoreMessage();
        OtpErlangTuple storeNode = (OtpErlangTuple) this.sendRecvMsg(
            this.pastryMailBox, 
            this.pastryName, 
            findStorageMsg, 
            new ArrayList<>(List.of(new OtpErlangString(fileName))),
            StoreMessage.class);
        if(storeNode == new OtpErlangTuple(new OtpErlangObject[]{})) return false;
        
        String destMailBox = ((OtpErlangAtom) storeNode.elementAt(0)).atomValue();
        String destName = ((OtpErlangAtom) storeNode.elementAt(1)).atomValue();
        StoreMessage storeMsg = new StoreMessage();
        
        OtpErlangTuple ret = (OtpErlangTuple) this.sendRecvMsg(
                destMailBox,
                destName,
                storeMsg,
                new ArrayList<>(List.of(new OtpErlangString(fileName), new OtpErlangBinary(fileData))),
                StoreMessage.class);
        return ret.elementAt(0) == new OtpErlangAtom("store_OK");
    }


    public boolean delete(String fileName) throws Exception {
    	DeleteMessage deleteMsg = new DeleteMessage();
    	OtpErlangTuple ret = (OtpErlangTuple) this.sendRecvMsg(
                this.pastryMailBox, 
                this.pastryName, 
                deleteMsg,
                new ArrayList<>(List.of(new OtpErlangString(fileName))),
                DeleteMessage.class);
    	
        return ret.elementAt(0) == new OtpErlangAtom("true");
    }
}
