package muance.javafx;

import clojure.lang.IFn;

public class ListCell<T> extends javafx.scene.control.ListCell<T> {
  private IFn updateItem;
  private Object state;

  public ListCell(IFn updateItem, Object state) {
    this.updateItem = updateItem;
    this.state = state;
  }

  public ListCell(IFn updateItem) {
    this.updateItem = updateItem;
  }

  @Override
  public void updateItem(T item, boolean empty) {
    super.updateItem(item, empty);
    updateItem.invoke(this, item, empty);
  }

  public Object getState() {
    return state;
  }
}
