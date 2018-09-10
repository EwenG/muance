package muance.javafx;

import javafx.scene.Node;

public abstract class TreeItem<T> extends javafx.scene.control.TreeItem {
  public TreeItem() {
  }

  @SuppressWarnings("unchecked")
  public TreeItem(T o) {
    super(o);
  }

  @SuppressWarnings("unchecked")
  public TreeItem(T o, Node node) {
    super(o, node);
  }
}
