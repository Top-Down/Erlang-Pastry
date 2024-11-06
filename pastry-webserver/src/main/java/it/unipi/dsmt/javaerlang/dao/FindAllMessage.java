package it.unipi.dsmt.javaerlang.dao;

import com.ericsson.otp.erlang.*;

public class FindAllMessage extends ErlangMessage {

    public void setContent() {
        OtpErlangAtom operation = new OtpErlangAtom("get_all_files");
        OtpErlangTuple findAllMsgContent = new OtpErlangTuple(new OtpErlangObject[]{
            operation
        });

        this.msgDTO.setContent(findAllMsgContent);
    }


    public OtpErlangList getContent(FindAllMessage findAllReq) {
        if(!this.checkOperation("all_files_res")) return new OtpErlangList();
        if(!this.checkMsgId(findAllReq)) return new OtpErlangList();
        
        return (OtpErlangList) this.msgDTO.getContentElement(1);
    }
}
