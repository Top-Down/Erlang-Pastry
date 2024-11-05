package it.unipi.dsmt;


public class BinaryLengthMismatchException extends RuntimeException {
    public BinaryLengthMismatchException(String message) {
        super(message);
    }
}

public class FindAllMessage extends ErlangMessage {

    public void setContent(String fileNameIn, byte[] fileData) {
        OtpErlangAtom operation = new OtpErlangAtom("store");
        OtpErlangString fileName = new OtpErlangString(fileNameIn);
        OtpErlangBinary file = new OtpErlangBinary(fileData);
        OtpErlangLong size = fileData.length;
        OtpErlangTuple findMsgContent = new OtpErlangTuple(new OtpErlangObject[]{
            operation, fileName, size, file
        });

        this.msgDTO.setContent(findMsgContent);
    }


    public OtpErlangBinary getContent(StoreMessage storeReq) {
        if (!this.checkOperation("store_end")) {
            throw new RuntimeException("Operation check failed.");
        }
        if (!this.checkMsgId(storeReq)) {
            throw new RuntimeException("Message ID check failed.");
        }
    }
}
