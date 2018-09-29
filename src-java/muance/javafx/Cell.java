package muance.javafx;

import javafx.scene.Node;

public interface Cell {
  Object getCellComponent();
  Node getNode();
  void setNode(Node node);
  Object getVtree();
  void setVtree(Object vtree);
}
