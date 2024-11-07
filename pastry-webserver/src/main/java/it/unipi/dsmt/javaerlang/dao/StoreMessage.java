package it.unipi.dsmt.javaerlang.dao;

import java.util.ArrayList;

import com.ericsson.otp.erlang.*;

public class StoreMessage extends ErlangMessage {

    @Override
    public void setContent(ArrayList<OtpErlangObject> content) {
    	if(content.size() == 1 && content.get(0) instanceof OtpErlangString) {
            OtpErlangAtom operation = new OtpErlangAtom("find_store");
            OtpErlangString fileName = (OtpErlangString) content.get(0);
            OtpErlangTuple findMsgContent = new OtpErlangTuple(new OtpErlangObject[]{
                operation, fileName
            });

            this.msgDTO.setContent(findMsgContent);
        }
    	else if(content.size() == 2 && content.get(0) instanceof OtpErlangString && content.get(1) instanceof OtpErlangBinary) {
            OtpErlangAtom operation = new OtpErlangAtom("store");
            OtpErlangString fileName = (OtpErlangString) content.get(0);
            OtpErlangBinary file = (OtpErlangBinary) content.get(1);
            OtpErlangLong size = new OtpErlangLong(file.binaryValue().length);
            OtpErlangTuple findMsgContent = new OtpErlangTuple(new OtpErlangObject[]{
                operation, fileName, size, file
            });

            this.msgDTO.setContent(findMsgContent);
        }
    }


    @Override
    public OtpErlangTuple getContent(ErlangMessage storeReq) {
    	OtpErlangTuple emptyTuple = new OtpErlangTuple(new OtpErlangObject[]{});
    	
    	if(!this.checkMsgId(storeReq)) return emptyTuple;
        if(!(this.checkOperation("store_end") || this.checkOperation("store_found"))) return emptyTuple;
        
        if(this.checkOperation("store_found")) {
            return (OtpErlangTuple) this.msgDTO.getContentElement(1);
        }
        else return new OtpErlangTuple(new OtpErlangAtom("store_OK"));
    }
}
