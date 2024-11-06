package it.unipi.dsmt;


public class BinaryLengthMismatchException extends RuntimeException {
    public BinaryLengthMismatchException(String message) {
        super(message);
    }
}

public class FindMessage extends ErlangMessage {

    @Override
    public void setContent(String fileNameIn) {
        OtpErlangAtom operation = new OtpErlangAtom("find");
        OtpErlangString fileName = new OtpErlangString(fileNameIn);
        OtpErlangTuple findMsgContent = new OtpErlangTuple(new OtpErlangObject[]{
            operation, fileName
        });

        this.msgDTO.setContent(findMsgContent);
    }

    @Override
    public OtpErlangBinary getContent(FindMessage findReq) {
        if (!this.checkOperation("find_end")) {
            throw new RuntimeException("Operation check failed.");
        }
        if (!this.checkMsgId(findReq)) {
            throw new RuntimeException("Message ID check failed.");
        }

        OtpErlangLong size = (OtpErlangLong) this.msgDTO.getContentElement(1);
        OtpErlangBinary file = (OtpErlangBinary) this.msgDTO.getContentElement(2);

        if (file.binaryValue().length == size.longValue()) {
            return file;
        } else {
            throw new BinaryLengthMismatchException("Binary length does not match the specified size.");
        }
    }
}
