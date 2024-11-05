package it.unipi.dsmt;


public class BinaryLengthMismatchException extends RuntimeException {
    public BinaryLengthMismatchException(String message) {
        super(message);
    }
}

public class DeleteMessage extends ErlangMessage {

    public void setContent(String fileNameIn) {
        OtpErlangAtom operation = new OtpErlangAtom("delete");
        OtpErlangString fileName = new OtpErlangString(fileNameIn);
        OtpErlangTuple deleteMsgContent = new OtpErlangTuple(new OtpErlangObject[]{
            operation, fileName
        });

        this.msgDTO.setContent(deleteMsgContent);
    }


    public void getContent(FindMessage deleteReq) {
        if (!this.checkOperation("delete_end")) {
            throw new RuntimeException("Operation check failed.");
        }
        if (!this.checkMsgId(deleteReq)) {
            throw new RuntimeException("Message ID check failed.");
        }
    }
}
