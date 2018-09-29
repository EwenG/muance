package muance.javafx;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.util.Callback;

public class TableColumn extends javafx.scene.control.TableColumn {
  public TableColumn() {
    super();
    this.setCellValueFactory(new Callback<CellDataFeatures, ObservableValue>() {
      @Override
      public ObservableValue call(CellDataFeatures cellDataFeatures) {
        return new SimpleObjectProperty<>(cellDataFeatures.getValue());
      }
    });
  }
}
