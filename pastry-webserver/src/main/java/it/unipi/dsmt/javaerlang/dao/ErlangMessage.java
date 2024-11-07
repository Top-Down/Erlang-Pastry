package it.unipi.dsmt.javaerlang.dao;

import java.util.ArrayList;
import com.ericsson.otp.erlang.*;
import it.unipi.dsmt.javaerlang.dto.ErlangMessageDTO;

public abstract class ErlangMessage {
    ErlangMessageDTO msgDTO;
    OtpErlangTuple senderInfo;
    OtpMbox mailBox;
    OtpErlangTuple msg;
    OtpErlangTuple content;
    
    public abstract void setContent(ArrayList<OtpErlangObject> content);
    public abstract OtpErlangObject getContent(ErlangMessage request);

    public void setMailbox(OtpMbox mboxIn){
        this.mailBox = mboxIn;
    }

    public void wrapMessage(OtpNode node, OtpErlangTuple senderAddrIn, String senderNameIn){
        OtpErlangRef msgId = new OtpErlangRef(node);
        OtpErlangLong timestamp = new OtpErlangLong(System.currentTimeMillis());
        this.msgDTO = new ErlangMessageDTO(
            senderAddrIn,
            senderNameIn,
            msgId,
            timestamp);

        OtpErlangString senderNameObj = new OtpErlangString(this.msgDTO.getSenderName());
        senderInfo = new OtpErlangTuple(new OtpErlangObject[]{
            this.msgDTO.getSenderAddr(), senderNameObj
        });

        this.msg = new OtpErlangTuple(new OtpErlangObject[]{
            senderInfo,
            this.msgDTO.getMsgId(),
            this.msgDTO.getTimestamp(),
            this.msgDTO.getContent()
        });
    }

    public void unwrapMessage() {
        senderInfo = (OtpErlangTuple) msg.elementAt(0);
        String senderName = (String) this.senderInfo.elementAt(0).toString();
        OtpErlangTuple senderAddr = (OtpErlangTuple) this.senderInfo.elementAt(1);

        OtpErlangRef msgId = (OtpErlangRef) msg.elementAt(1);
        OtpErlangLong timestamp = (OtpErlangLong) msg.elementAt(2);
        content = (OtpErlangTuple) msg.elementAt(3);

        this.msgDTO = new ErlangMessageDTO(
            senderAddr,
            senderName,
            msgId,
            timestamp);
        this.msgDTO.setContent(content);
    }

    public boolean checkMsgId(ErlangMessage oldMsg) {
        return oldMsg.msgDTO.getMsgId() == this.msgDTO.getMsgId();
    }

    public boolean checkOperation(String operationIn) {
    	OtpErlangAtom operation = new OtpErlangAtom(operationIn);
        return operation == this.msgDTO.getOperation();
    }

    public void sendMessage(String destMailBox, String destName){
        this.mailBox.send(destMailBox, destName, this.msg);
    }

    public void receiveMessage(){
        try {
            OtpErlangObject response = this.mailBox.receive();
            if (response instanceof OtpErlangTuple) {
                this.msg = (OtpErlangTuple) response;
            }
        } catch (OtpErlangDecodeException e) {
            throw new RuntimeException(e);
        } catch (OtpErlangExit e) {
            throw new RuntimeException(e);
        }
    }
}
