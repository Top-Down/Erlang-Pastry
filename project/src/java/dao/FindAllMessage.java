package it.unipi.dsmt;


public class BinaryLengthMismatchException extends RuntimeException {
    public BinaryLengthMismatchException(String message) {
        super(message);
    }
}

public class FindAllMessage extends ErlangMessage {

    public void setContent() {
        OtpErlangAtom operation = new OtpErlangAtom("get_all_files");
        OtpErlangTuple findAllMsgContent = new OtpErlangTuple(new OtpErlangObject[]{
            operation
        });

        this.msgDTO.setContent(findAllMsgContent);
    }


    public OtpErlangList getContent(FindAllMessage findAllReq) {
        if (!this.checkOperation("all_files_res")) {
            throw new RuntimeException("Operation check failed.");
        }
        if (!this.checkMsgId(findAllReq)) {
            throw new RuntimeException("Message ID check failed.");
        }
    }
}
