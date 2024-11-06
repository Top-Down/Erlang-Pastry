package it.unipi.dsmt;

import com.ericsson.otp.erlang.*;
import java.util.List;

public class DeleteMessage extends ErlangMessage {

    @Override
    public void setContent(List<OtpErlangObject> content) {
        if (content.size() != 1 || !(content.get(0) instanceof OtpErlangString)) {
            throw new IllegalArgumentException("DeleteMessage requires a single OtpErlangString as content.");
        }
        OtpErlangAtom operation = new OtpErlangAtom("delete");
        OtpErlangString fileName = (OtpErlangString) content.get(0);
        OtpErlangTuple deleteMsgContent = new OtpErlangTuple(new OtpErlangObject[]{
            operation, fileName
        });

        this.msgDTO.setContent(deleteMsgContent);
    }

    @Override
    public OtpErlangObject getContent(ErlangMessage request) {
        if (!this.checkOperation("delete_end")) {
            throw new RuntimeException("Operation check failed.");
        }
        if (!this.checkMsgId(request)) {
            throw new RuntimeException("Message ID check failed.");
        }
        return this.msgDTO.getContent();
    }
}
