package example;

import org.graalvm.polyglot.*;

public class Foreign {
    public static void main(String[] args) throws Exception {
        Context c = Context.newBuilder()
            .allowAllAccess(true)
            .build();
        Source src =
            Source.newBuilder("llvm", Foreign.class.getResource("/foreign"))
                  .build();
        Value foreign = c.eval(src);
        Value init_foreign = foreign.getMember("init_foreign");
        Value caller = foreign.getMember("caller");
        init_foreign.executeVoid();
        caller.executeVoid();
    }
}