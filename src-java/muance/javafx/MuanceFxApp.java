package muance.javafx;

import clojure.java.api.Clojure;
import clojure.lang.IFn;
import javafx.application.Application;
import javafx.stage.Stage;

public class MuanceFxApp extends Application {
  public static IFn stagePromise = (IFn) Clojure.var("clojure.core", "promise").invoke();
  public static IFn hostServicesPromise = (IFn) Clojure.var("clojure.core", "promise").invoke();
  public static IFn parametersPromise = (IFn) Clojure.var("clojure.core", "promise").invoke();

  @Override
  public void start(Stage stage) throws Exception {
    stagePromise.invoke(stage);
    hostServicesPromise.invoke(this.getHostServices());
    parametersPromise.invoke(this.getParameters());
  }
}
