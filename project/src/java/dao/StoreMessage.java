package it.unipi.dsmt;


public class BinaryLengthMismatchException extends RuntimeException {
    public BinaryLengthMismatchException(String message) {
        super(message);
    }
}

public class FindAllMessage extends ErlangMessage {


    @Override
    public void setContent(List<OtpErlangObject> content) {
        if (content.size() == 1 && content.get(0) instanceof OtpErlangString) {
            OtpErlangAtom operation = new OtpErlangAtom("find_store");
            OtpErlangString fileName = (OtpErlangString) content.get(0);
            OtpErlangTuple findMsgContent = new OtpErlangTuple(new OtpErlangObject[]{
                operation, fileName
            });

            this.msgDTO.setContent(findMsgContent);

        } else if (content.size() == 2 && content.get(0) instanceof OtpErlangString && content.get(1) instanceof OtpErlangBinary) {
            OtpErlangAtom operation = new OtpErlangAtom("store");
            OtpErlangString fileName = (OtpErlangString) content.get(0);
            OtpErlangBinary file = (OtpErlangBinary) content.get(1);
            OtpErlangLong size = new OtpErlangLong(file.binaryValue().length);
            OtpErlangTuple findMsgContent = new OtpErlangTuple(new OtpErlangObject[]{
                operation, fileName, size, file
            });

            this.msgDTO.setContent(findMsgContent);
            
        } else {
            throw new IllegalArgumentException("Invalid content for FindAllMessage.");
        }
    }

    @Override
    public OtpErlangObject getContent(ErlangMessage request) {
        if (!this.checkMsgId(request)) {
            throw new RuntimeException("Message ID check failed.");
        }

        if (this.checkOperation("store_end")) {
            return new OtpErlangString("ok");
        } else if (this.checkOperation("store_found")) {
            return this.getContentAt(1);
        } else {
            throw new RuntimeException("Operation check failed.");
        }
    }
}



    
