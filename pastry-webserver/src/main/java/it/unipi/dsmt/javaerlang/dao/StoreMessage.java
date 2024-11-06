package it.unipi.dsmt.javaerlang.dao;

import com.ericsson.otp.erlang.*;

public class StoreMessage extends ErlangMessage {

    public void setContent(String fileNameIn, byte[] fileData) {
        OtpErlangAtom operation = new OtpErlangAtom("store");
        OtpErlangString fileName = new OtpErlangString(fileNameIn);
        OtpErlangBinary file = new OtpErlangBinary(fileData);
        OtpErlangLong size = new OtpErlangLong(fileData.length);
        OtpErlangTuple findMsgContent = new OtpErlangTuple(new OtpErlangObject[]{
            operation, fileName, size, file
        });

        this.msgDTO.setContent(findMsgContent);
    }


    public boolean getContent(StoreMessage storeReq) {
        if (!this.checkOperation("store_end")) {
            return false;
        }
        if (!this.checkMsgId(storeReq)) {
            return false;
        }
        return true;
    }
}
