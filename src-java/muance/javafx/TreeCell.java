package muance.javafx;

import clojure.lang.IFn;

public class TreeCell<T> extends javafx.scene.control.TreeCell<T> {
  private IFn updateItem;
  private IFn destructor;
  private Object state;

  public TreeCell(IFn updateItem) {
    super();
    this.updateItem = updateItem;
  }

  public TreeCell(IFn constructor, IFn updateItem, IFn destructor) {
    super();
    this.updateItem = updateItem;
    this.destructor = destructor;
    constructor.invoke(this);
  }

  @Override
  public void updateItem(T item, boolean empty) {
    super.updateItem(item, empty);
    updateItem.invoke(this, item, empty);
  }

  public void destroy() {
    destructor.invoke(this);
  }

  public Object getState() {
    return this.state;
  }

  public void setState(Object newState) {
    this.state = newState;
  }
}
