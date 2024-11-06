package it.unipi.dsmt.javaerlang.dao;

import com.ericsson.otp.erlang.*;

public class FindMessage extends ErlangMessage {

    public void setContent(String fileNameIn) {
        OtpErlangAtom operation = new OtpErlangAtom("find");
        OtpErlangString fileName = new OtpErlangString(fileNameIn);
        OtpErlangTuple findMsgContent = new OtpErlangTuple(new OtpErlangObject[]{
            operation, fileName
        });

        this.msgDTO.setContent(findMsgContent);
    }


    public OtpErlangBinary getContent(FindMessage findReq) {
        if(!this.checkOperation("find_end")) return null;
        if(!this.checkMsgId(findReq)) return null;

        OtpErlangLong size = (OtpErlangLong) this.msgDTO.getContentElement(1);
        OtpErlangBinary file = (OtpErlangBinary) this.msgDTO.getContentElement(2);

        if(file.binaryValue().length == size.longValue()) return file;
        else return null;
    }
}
