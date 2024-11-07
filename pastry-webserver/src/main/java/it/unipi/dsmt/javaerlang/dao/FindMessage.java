package it.unipi.dsmt.javaerlang.dao;

import java.util.ArrayList;

import com.ericsson.otp.erlang.*;

public class FindMessage extends ErlangMessage {

    @Override
    public void setContent(ArrayList<OtpErlangObject> content) {
    	if(content.size() != 1 || !(content.get(0) instanceof OtpErlangString)) {
            throw new IllegalArgumentException("FindMessage requires a single OtpErlangString as content.");
        }
        OtpErlangAtom operation = new OtpErlangAtom("find");
        OtpErlangString fileName = (OtpErlangString) content.get(0);
        OtpErlangTuple findMsgContent = new OtpErlangTuple(new OtpErlangObject[]{
            operation, fileName
        });

        this.msgDTO.setContent(findMsgContent);
    }


    @Override
    public OtpErlangBinary getContent(ErlangMessage findReq) {
        if(!this.checkOperation("find_end")) return new OtpErlangBinary(new byte[0]);
        if(!this.checkMsgId(findReq)) return new OtpErlangBinary(new byte[0]);

        OtpErlangLong size = (OtpErlangLong) this.msgDTO.getContentElement(1);
        OtpErlangBinary file = (OtpErlangBinary) this.msgDTO.getContentElement(2);

        if(file.binaryValue().length == size.longValue()) return file;
        else return new OtpErlangBinary(new byte[0]);
    }
}
