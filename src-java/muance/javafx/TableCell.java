package muance.javafx;

import clojure.lang.IFn;
import javafx.scene.Node;
import javafx.scene.control.TableColumn;

public class TableCell<S, T> extends javafx.scene.control.TableCell<S, T> implements Cell {
  private IFn updateItem;
  private IFn destructor;
  private Object cellComponent;
  private Node node;
  private Object vtree;

  public TableCell(TableColumn o, Object cellComponent, IFn updateItem, IFn constructor, IFn destructor) {
    super();
    this.cellComponent = cellComponent;
    this.updateItem = updateItem;
    this.destructor = destructor;
    if(constructor != null) {
      constructor.invoke(this, o);
    }
  }

  @Override
  public Object getCellComponent() {
    return cellComponent;
  }

  @Override
  public Node getNode() {
    return node;
  }

  @Override
  public void setNode(Node node) {
    this.node = node;
  }

  @Override
  public Object getVtree() {
    return vtree;
  }

  @Override
  public void setVtree(Object vtree) {
    this.vtree = vtree;
  }

  @Override
  public void updateItem(T item, boolean empty) {
    super.updateItem(item, empty);
    if(updateItem != null) {
      updateItem.invoke(this, item, empty);
    }
  }

  public void destroy() {
    if(destructor != null) {
      destructor.invoke(this);
    }
  }
}
