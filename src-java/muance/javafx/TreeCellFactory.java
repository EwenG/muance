package muance.javafx;

import clojure.lang.IFn;
import javafx.util.Callback;

import java.util.ArrayList;

public class TreeCellFactory implements Callback {

  private IFn cellConstructor;
  private IFn updateItem;
  private IFn cellDestructor;

  private ArrayList<TreeCell> cells = new ArrayList<>();

  public TreeCellFactory(IFn updateItem) {
    this.updateItem = updateItem;
  }

  public TreeCellFactory(IFn cellConstructor, IFn updateItem, IFn cellDestructor) {
    this.cellConstructor = cellConstructor;
    this.updateItem = updateItem;
    this.cellDestructor = cellDestructor;
  }

  @Override
  public Object call(Object o) {
    if(cellConstructor != null && cellDestructor != null) {
      TreeCell cell = new TreeCell(cellConstructor, updateItem, cellDestructor);
      cells.add(cell);
      return cell;
    } else {
      TreeCell cell = new TreeCell(updateItem);
      cells.add(cell);
      return cell;
    }
  }

  public ArrayList<TreeCell> getCells() {
    return cells;
  }
}
