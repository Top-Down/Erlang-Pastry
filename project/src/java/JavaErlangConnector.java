package it.unipi.dsmt.javaerlang;

import com.ericsson.otp.erlang.*;

import java.io.IOException;
import java.util.List;
import java.util.Scanner;

public class JavaErlangConnector
{   
    String pastryMailBox;
    String pastryName;
    String cookie;
    String selfName;
    OtpNode node;
    OtpMbox mailBox;
    OtpErlangTuple selfAddr;

    public JavaErlangConnector(
        String pastryMailBoxIn, 
        String pastryNameIn, 
        String cookieIn,
        String selfNameIn){

        this.pastryMailBox = pastryMailBoxIn;
        this.pastryName = pastryNameIn;
        this.cookie = cookieIn;
        this.selfName = selfNameIn;

        this.node = new OtpNode(this.selfName, this.cookie);
        this.mailBox = node.createMbox();
        this.selfAddr = new OtpErlangTuple(new OtpErlangObject[]{
            new OtpErlangAtom(this.mailBox.getName()), new OtpErlangAtom(this.node.node())
        });
    }

    public <T extends ErlangMessage> OtpErlangObject sendRecvMsg(
        String destMailBox,
        String destName,
        T message, 
        List<OtpErlangObject> content) throws Exception {

        message.setMailbox(this.mailBox);
        message.setContent(content);
        message.wrapMessage(selfAddr, selfName);
        message.send(destMailBox, destName);

        T response = (T) message.getClass().newInstance();
        response.setMailbox(this.mailBox);
        response.receive();
        response.unwrapMessage();
        return response.getContent(message);
    }

    public byte[] find(String fileName) throws Exception {
        FindMessage findMsg = new FindMessage();
        OtpErlangBinary file = (OtpErlangBinary)this.sendRecvMsg(
            this.pastryMailBox, 
            this.pastryName, 
            findMsg, 
            List.of(new OtpErlangString(fileName)));
        return file.binaryValue();
    }

    public List<String> find_all() throws Exception {
        FindAllMessage findAllMsg = new FindAllMessage();
        OtpErlangList fileList = (OtpErlangList)this.sendRecvMsg(
            this.pastryMailBox, 
            this.pastryName, 
            findAllMsg, 
            List.of());
        return fileList.elements().stream()
                       .map(elem -> ((OtpErlangString)elem).stringValue())
                       .collect(Collectors.toList());
    }

    public boolean store(String fileName, byte[] fileData) throws Exception {
        StoreMessage findStorageMsg = new StoreMessage();
        OtpErlangTuple storeNode = (OtpErlangTuple)this.sendRecvMsg(
            this.pastryMailBox, 
            this.pastryName, 
            findStorageMsg, 
            List.of(new OtpErlangString(fileName), new OtpErlangBinary(fileData)));
        
        String destMailbox = ((OtpErlangAtom)storeNode.elementAt(0)).atomValue();
        String destName = ((OtpErlangAtom)storeNode.elementAt(1)).atomValue();
        StoreMessage storeMsg = new StoreMessage(storeNode);
        return this.sendRecvMsg(
            destMailBox, 
            destName, 
            storeMsg, 
            List.of(storeNode));
    }

    public boolean delete(String fileName) throws Exception {
        DeleteMessage deleteMsg = new DeleteMessage();
        return this.sendRecvMsg(
            this.pastryMailBox, 
            this.pastryName, 
            deleteMsg,
            List.of(new OtpErlangString(fileName)))
            .equals("true");
    }
}
