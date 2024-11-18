package it.unipi.dsmt.javaerlang.dao;

import java.util.ArrayList;
import com.ericsson.otp.erlang.*;

public class FindAllMessage extends ErlangMessage {

    @Override
    public void setContent(ArrayList<OtpErlangObject> content) {
    	if (content.size() != 0) {
            throw new IllegalArgumentException("FindAllMessage does not require any content.");
        }
        OtpErlangAtom operation = new OtpErlangAtom("get_all_files");
        OtpErlangTuple findAllMsgContent = new OtpErlangTuple(new OtpErlangObject[]{
            operation
        });

        this.content = findAllMsgContent;
    }


    @Override
    public OtpErlangList getContent(ErlangMessage findAllReq) {
        if(!this.checkOperation("all_files_res")) return new OtpErlangList();
        if(!this.checkMsgId(findAllReq)) return new OtpErlangList();
        
        return (OtpErlangList) this.msgDTO.getContentElement(1);
    }
}
