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

public class ErlangMessage {
    ErlangMessageDTO msgDTO;
    OtpErlangTuple senderInfo;
    OtpMbox mailBox;
    OtpErlangTuple msg;
    OtpErlangTuple content;

    public abstract void setContent(List<OtpErlangObject> content);
    public abstract OtpErlangObject getContent(ErlangMessage request);

    private void setMailbox(OtpMbox mboxIn){
        this.mbox = mboxIn;
    }

    private void wrapMessage(OtpErlangTuple senderAddrIn, String senderNameIn){
        msgId = new OtpErlangRef();
        timestamp = new OtpErlangLong(System.currentTimeMillis());
        this.msgDTO = new ErlangMessageDTO(
            senderAddrIn, 
            senderNameIn,
            timestamp,
            msgId);

        senderInfo = new OtpErlangTuple(new OtpErlangObject[]{
            this.msgDTO.getSenderAddr(), this.msgDTO.getSenderName()
        });

        this.msg = new OtpErlangTuple(new OtpErlangObject[]{
            senderInfo,
            this.msgDTO.getMsgId(),
            this.msgDTO.getTimestamp(),
            this.msgDTO.getContent()
        });
    }

    private void unwrapMessage() {
        senderInfo = (OtpErlangTuple) msg.elementAt(0);
        senderName = (String) this.senderInfo.elementAt(0)stringValue();
        senderAddr = (OtpErlangTuple) this.senderInfo.elementAt(1)

        msgId = (OtpErlangRef) msg.elementAt(1);
        timestamp = (OtpErlangLong) msg.elementAt(2);
        content = (OtpErlangTuple) msg.elementAt(3);

        this.msgDTO = new ErlangMessageDTO(
            senderAddr, 
            senderName,
            timestamp,
            msgId,
            content);
    }

    private bool checkOperation(ErlangMessage oldMsg){
        return oldMsg.msgDTO.getOperation() == this.msgDTO.getOperation();
    }

    private bool checkMsgId(String operationIn){
        OtpErlangString operation = new OtpErlangString(operationIn)
        return operation == this.msgDTO.getMsgId();
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
