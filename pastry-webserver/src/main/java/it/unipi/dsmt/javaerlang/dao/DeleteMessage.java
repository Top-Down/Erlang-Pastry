package it.unipi.dsmt.javaerlang.dao;

import com.ericsson.otp.erlang.*;

public class DeleteMessage extends ErlangMessage {

    public void setContent(String fileNameIn) {
        OtpErlangAtom operation = new OtpErlangAtom("delete");
        OtpErlangString fileName = new OtpErlangString(fileNameIn);
        OtpErlangTuple deleteMsgContent = new OtpErlangTuple(new OtpErlangObject[]{
            operation, fileName
        });

        this.msgDTO.setContent(deleteMsgContent);
    }


    public boolean getContent(DeleteMessage deleteReq) {
        if(!this.checkOperation("delete_end")) return false;
        if(!this.checkMsgId(deleteReq)) return false;
        
        return true;
    }
}
