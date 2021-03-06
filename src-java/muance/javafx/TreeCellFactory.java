package muance.javafx;

import clojure.lang.IFn;
import javafx.scene.control.TreeView;
import javafx.util.Callback;

import java.util.ArrayList;

public class TreeCellFactory implements CellFactory, Callback<TreeView, TreeCell> {

  private Object cellComponent;
  private IFn updateItem;
  private IFn cellConstructor;
  private IFn cellDestructor;

  private ArrayList<Cell> cells = new ArrayList<>();

  public TreeCellFactory(Object cellComponent, IFn updateItem, IFn cellConstructor, IFn cellDestructor) {
    this.cellComponent = cellComponent;
    this.updateItem = updateItem;
    this.cellConstructor = cellConstructor;
    this.cellDestructor = cellDestructor;
  }

  @Override
  public TreeCell call(TreeView o) {
    TreeCell cell = new TreeCell(o, cellComponent, updateItem, cellConstructor, cellDestructor);
    cells.add(cell);
    return cell;
  }

  @Override
  public ArrayList<Cell> getCells() {
    return cells;
  }
}
