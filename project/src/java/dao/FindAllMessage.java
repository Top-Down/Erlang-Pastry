package it.unipi.dsmt;

import com.ericsson.otp.erlang.*;
import java.util.List;

public class FindAllMessage extends ErlangMessage {

    @Override
    public void setContent(List<OtpErlangObject> content) {
        if (content.size() != 0) {
            throw new IllegalArgumentException("FindAllMessage does not require any content.");
        }
        OtpErlangAtom operation = new OtpErlangAtom("get_all_files");
        OtpErlangTuple findAllMsgContent = new OtpErlangTuple(new OtpErlangObject[]{
            operation
        });

        this.msgDTO.setContent(findAllMsgContent);
    }

    @Override
    public OtpErlangObject getContent(ErlangMessage request) {
        if (!this.checkOperation("all_files_res")) {
            throw new RuntimeException("Operation check failed.");
        }
        if (!this.checkMsgId(request)) {
            throw new RuntimeException("Message ID check failed.");
        }
        return this.msgDTO.getContent();
    }
}
