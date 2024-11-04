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

    webServerMailBox.send(pastryMailBox, pastryName, message);

    try {
        OtpErlangObject o = webServerMailBox.receive();
        if (o instanceof OtpErlangTuple) {
            OtpErlangTuple msg = (OtpErlangTuple)o;
            OtpErlangAtom atom = (OtpErlangAtom)(msg.elementAt(0));
            OtpErlangString response = (OtpErlangString)(msg.elementAt(1));
            System.out.println("Atom: " + atom.toString());
            System.out.println("Response: " + response.toString());
        }
    } catch (OtpErlangDecodeException e) {
        throw new RuntimeException(e);
    } catch (OtpErlangExit e) {
        throw new RuntimeException(e);
    }


    private find(String fileName) throws Exception {

        OtpErlangTuple findMsgContent = new OtpErlangTuple(new OtpErlangObject[]{
            new OtpErlangAtom("find"), new OtpErlangString(fileName)
        });
        OtpErlangTuple findMsg = this.wrapMessage(addMsgContent)
        mbox.send(this.pastryMailBox, this.pastryName, findMsg);

        try {
            OtpErlangObject response = this.mailBox.receive();
            if (response instanceof OtpErlangTuple) {
                OtpErlangList payload = this.unwrapMessage(response);
            }
        } catch (OtpErlangDecodeException e) {
            throw new RuntimeException(e);
        } catch (OtpErlangExit e) {
            throw new RuntimeException(e);
        }
    }

    private find_all(){
        OtpErlangTuple findMsgContent = new OtpErlangTuple(new OtpErlangObject[]{
            new OtpErlangAtom("get_all_files"), new OtpErlangString(fileName)
        });
        OtpErlangTuple findMsg = this.wrapMessage(addMsgContent)
        mbox.send(this.pastryMailBox, this.pastryName, findMsg);

        try {
            OtpErlangObject response = this.mailBox.receive();
            if (response instanceof OtpErlangTuple) {
                OtpErlangList payload = this.unwrapMessage(response);
            }
        } catch (OtpErlangDecodeException e) {
            throw new RuntimeException(e);
        } catch (OtpErlangExit e) {
            throw new RuntimeException(e);
        }
    }

    private add(String fileName, byte[] fileData) throws Exception {

        OtpErlangTuple findMsgContent = new OtpErlangTuple(new OtpErlangObject[]{
            new OtpErlangAtom("find"), new OtpErlangString(fileName)
        });
        OtpErlangTuple findMsg = this.wrapMessage(addMsgContent)
        mbox.send(this.pastryMailBox, this.pastryName, findMsg);

        try {
            OtpErlangObject response = this.mailBox.receive();
            if (response instanceof OtpErlangTuple) {
                OtpErlangList payload = this.unwrapMessage(response);
            }
        } catch (OtpErlangDecodeException e) {
            throw new RuntimeException(e);
        } catch (OtpErlangExit e) {
            throw new RuntimeException(e);
        }


        OtpErlangTuple storeContent = new OtpErlangTuple(new OtpErlangObject[]{
            new OtpErlangAtom("file_send"), new OtpErlangString(fileName), new OtpErlangBinary(fileData)
        });
        OtpErlangTuple storeMsg = this.wrapMessage(addMsgContent)
        mbox.send(this.pastryMailBox, this.pastryName, storeMsg);

        try {
            OtpErlangObject response = this.mailBox.receive();
            if (response instanceof OtpErlangTuple) {
                OtpErlangList payload = this.unwrapMessage(response);
            }
        } catch (OtpErlangDecodeException e) {
            throw new RuntimeException(e);
        } catch (OtpErlangExit e) {
            throw new RuntimeException(e);
        }
    }


    private delete(String fileName){
        OtpErlangTuple findMsgContent = new OtpErlangTuple(new OtpErlangObject[]{
            new OtpErlangAtom("delete"), new OtpErlangString(fileName)
        });
        OtpErlangTuple findMsg = this.wrapMessage(addMsgContent)
        mbox.send(this.pastryMailBox, this.pastryName, findMsg);

        try {
            OtpErlangObject response = this.mailBox.receive();
            if (response instanceof OtpErlangTuple) {
                OtpErlangList payload = this.unwrapMessage(response);
            }
        } catch (OtpErlangDecodeException e) {
            throw new RuntimeException(e);
        } catch (OtpErlangExit e) {
            throw new RuntimeException(e);
        }
    }


    OtpErlangTuple private wrapMessage(OtpErlangObject content){
        OtpErlangString fileNameErl = new OtpErlangString(fileName);
        OtpErlangBinary fileDataErl = new OtpErlangBinary(fileData);

        OtpErlangRef msgId = new OtpErlangRef();
        OtpErlangLong timestamp = new OtpErlangLong(System.currentTimeMillis());

        OtpErlangTuple selfInfo = new OtpErlangTuple(new OtpErlangObject[]{
            this.selfAddr, this.selfName
        });
        OtpErlangTuple msg = new OtpErlangTuple(new OtpErlangObject[]{
            selfInfo, msgId, timestamp, content
        });

        return msg
    }

    private OtpErlangTuple unwrapMessage(OtpErlangObject response) {
        OtpErlangTuple msg = (OtpErlangTuple) response;
        OtpErlangTuple nodeAddrName = (OtpErlangTuple) msg.elementAt(0);
        OtpErlangAtom recvNodeAddr = (OtpErlangAtom) nodeAddrName.elementAt(0);
        OtpErlangAtom recvNodeName = (OtpErlangAtom) nodeAddrName.elementAt(1);

        OtpErlangRef msgId = (OtpErlangRef) msg.elementAt(1);
        OtpErlangLong getTime = (OtpErlangLong) msg.elementAt(2);

        OtpErlangTuple content = (OtpErlangTuple) msg.elementAt(3);
        OtpErlangAtom operation = (OtpErlangAtom) content.elementAt(0);

        int contentSize = content.arity();
        OtpErlangObject[] payloadElements = new OtpErlangObject[contentSize - 1];
        for (int i = 1; i < contentSize; i++) {
            payloadElements[i - 1] = content.elementAt(i);
        }

        OtpErlangList payload = new OtpErlangList(payloadElements);
    }
}
