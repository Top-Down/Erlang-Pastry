package it.unipi.dsmt.javaerlang;

import com.ericsson.otp.erlang.*;

import java.io.IOException;
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

    public byte[] find(String fileName) throws Exception {
        FindMessage findMsg = new FindMessage();
        findMsg.setMailbox(mailBox);
        findMsg.setContent(fileName);
        findMsg.wrapMessage(this.node, selfAddr, selfName);
        findMsg.sendMessage(this.pastryMailBox, this.pastryName);

        FindMessage findResponse = new FindMessage();
        findResponse.setMailbox(mailBox);
        findResponse.receiveMessage();
        findResponse.unwrapMessage();
        return findResponse.getContent(findMsg).binaryValue();
    }

    public void find_all(){
        FindAllMessage findAllMsg = new FindAllMessage();
        findAllMsg.setMailbox(mailBox);
        findAllMsg.setContent();
        findAllMsg.wrapMessage(this.node, selfAddr, selfName);
        findAllMsg.sendMessage(this.pastryMailBox, this.pastryName);

        FindAllMessage findAllResponse = new FindAllMessage();
        findAllResponse.setMailbox(mailBox);
        findAllResponse.receiveMessage();
        findAllResponse.unwrapMessage();
        findAllResponse.getContent(findAllMsg);
    }

    public void store(String fileName, byte[] fileData) throws Exception {
        StoreMessage storeMsg = new StoreMessage();
        storeMsg.setMailbox(mailBox);
        storeMsg.setContent(fileName, fileData);
        storeMsg.wrapMessage(this.node, selfAddr, selfName);
        storeMsg.sendMessage(this.pastryMailBox, this.pastryName);

        StoreMessage storeResponse = new StoreMessage();
        storeResponse.setMailbox(mailBox);
        storeResponse.receiveMessage();
        storeResponse.unwrapMessage();
        storeResponse.getContent(storeMsg);
    }


    public boolean delete(String fileName) {
        DeleteMessage deleteMsg = new DeleteMessage();
        deleteMsg.setMailbox(mailBox);
        deleteMsg.setContent(fileName);
        deleteMsg.wrapMessage(this.node, selfAddr, selfName);
        deleteMsg.sendMessage(this.pastryMailBox, this.pastryName);

        DeleteMessage deleteResponse = new DeleteMessage();
        deleteResponse.setMailbox(mailBox);
        deleteResponse.receiveMessage();
        deleteResponse.unwrapMessage();
        return deleteResponse.getContent(deleteMsg);
    }
}
