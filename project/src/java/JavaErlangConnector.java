package it.unipi.dsmt.javaerlang;

import com.ericsson.otp.erlang.*;

import java.io.IOException;
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
            new OtpErlangAtom(this.mbox), new OtpErlangAtom(this.node)
        });
    }

    private find(String fileName) throws Exception {
        FindMessage findMsg = new FindMessage();
        findMsg.setMailbox(mailBox);
        findMsg.setContent(fileName);
        findMsg.wrapMessage(selfAddr, selfName);
        findMsg.send(this.pastryMailBox, this.pastryName);

        FindMessage findResponse = new FindMessage();
        findResponse.setMailbox(mailBox);
        findResponse.receive();
        findResponse.unwrapMessage();
        OtpErlangBinary file = findResponse.getContent(findMsg);
    }

    private find_all(){
        FindAllMessage findAllMsg = new FindAllMessage();
        findAllMsg.setMailbox(mailBox);
        findAllMsg.setContent();
        findAllMsg.wrapMessage(selfAddr, selfName);
        findAllMsg.send(this.pastryMailBox, this.pastryName);

        FindAllMessage findAllResponse = new FindAllMessage();
        findAllResponse.setMailbox(mailBox);
        findAllResponse.receive();
        findAllResponse.unwrapMessage();
        findAllResponse.getContent(findAllMsg);
    }

    private store(String fileName, byte[] fileData) throws Exception {
        StoreMessage storeMsg = new StoreMessage();
        storeMsg.setMailbox(mailBox);
        storeMsg.setContent(fileName, fileData);
        storeMsg.wrapMessage(selfAddr, selfName);
        storeMsg.send(this.pastryMailBox, this.pastryName);

        StoreMessage storeResponse = new StoreMessage();
        storeResponse.setMailbox(mailBox);
        storeResponse.receive();
        storeResponse.unwrapMessage();
        storeResponse.getContent(storeMsg);
    }


    private delete(String fileName){
        DeleteMessage deleteMsg = new DeleteMessage();
        deleteMsg.setMailbox(mailBox);
        deleteMsg.setContent(fileName);
        deleteMsg.wrapMessage(selfAddr, selfName);
        deleteMsg.send(this.pastryMailBox, this.pastryName);

        DeleteMessage storeResponse = new DeleteMessage();
        deleteResponse.setMailbox(mailBox);
        deleteResponse.receive();
        deleteResponse.unwrapMessage();
        deleteResponse.getContent(deleteMsg);
    }
}
