package it.unipi.dsmt.javaerlang.dao;

import java.util.ArrayList;

import com.ericsson.otp.erlang.*;

public class DeleteMessage extends ErlangMessage {

    @Override
    public void setContent(ArrayList<OtpErlangObject> content) {
    	if(content.size() != 1 || !(content.get(0) instanceof OtpErlangString)) {
            throw new IllegalArgumentException("DeleteMessage requires a single OtpErlangString as content.");
        }
        OtpErlangAtom operation = new OtpErlangAtom("delete");
        OtpErlangString fileName = (OtpErlangString) content.get(0);
        OtpErlangTuple deleteMsgContent = new OtpErlangTuple(new OtpErlangObject[]{
            operation, fileName
        });

        this.content = deleteMsgContent;
    }


    @Override
    public OtpErlangAtom getContent(ErlangMessage deleteReq) {
    	OtpErlangAtom falseAtom = new OtpErlangAtom("false");
        if(!this.checkOperation("delete_end")) return falseAtom;
        if(!this.checkMsgId(deleteReq)) return falseAtom;

        return new OtpErlangAtom("true");
    }
}
