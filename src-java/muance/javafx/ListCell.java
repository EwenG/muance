package muance.javafx;

import clojure.lang.IFn;

public class ListCell<T> extends javafx.scene.control.ListCell<T> {
  private IFn updateItem;
  private Object vtreeComponent;

  public ListCell(IFn updateItem, Object vtreeComponent) {
    this.updateItem = updateItem;
    this.vtreeComponent = vtreeComponent;
  }

  public Object getVtreeComponent() {
    return vtreeComponent;
  }

  @Override
  public void updateItem(T item, boolean empty) {
    super.updateItem(item, empty);
    updateItem.invoke(this, item, empty);
  }
}
