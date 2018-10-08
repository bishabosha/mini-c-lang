package mycc;

import org.graalvm.nativeimage.c.CContext;

import java.util.Collections;
import java.util.List;

public class CContextDirectives implements CContext.Directives {
    @Override
    public List<String> getLibraries() {
        return Collections.singletonList("mycclib");
    }
}
