package mycc;

import org.graalvm.nativeimage.c.CContext;
import org.graalvm.nativeimage.c.constant.CEnum;
import org.graalvm.nativeimage.c.constant.CEnumLookup;
import org.graalvm.nativeimage.c.constant.CEnumValue;

@CContext(CContextDirectives.class)
@CEnum("Tag")
public enum Tag {
    NODE, TOKEN, STRING_CONSTANT, INT_CONSTANT;

    @CEnumValue
    public native int getCValue();

    @CEnumLookup
    public static native Tag fromCValue(int value);
}
