package mycc;

import java.lang.reflect.Method;

import org.graalvm.nativeimage.Feature;
import com.oracle.svm.core.annotate.AutomaticFeature;
import org.graalvm.nativeimage.RuntimeReflection;

@AutomaticFeature
class RuntimeReflectionRegistrationFeature implements Feature {
  public void beforeAnalysis(Feature.BeforeAnalysisAccess access) {
    try {
      RuntimeReflection.register(Method.class);
      RuntimeReflection.register(Method.class.getDeclaredMethod("invoke", Object.class, Object.class, Object[].class));
    } catch (NoSuchMethodException e) { }
  }
}