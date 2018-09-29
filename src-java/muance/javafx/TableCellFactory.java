package muance.javafx;

import clojure.lang.IFn;
import javafx.scene.control.TableColumn;
import javafx.util.Callback;

import java.util.ArrayList;

public class TableCellFactory implements CellFactory, Callback<TableColumn, TableCell> {

  private Object cellComponent;
  private IFn updateItem;
  private IFn cellConstructor;
  private IFn cellDestructor;

  private ArrayList<Cell> cells = new ArrayList<>();

  public TableCellFactory(Object cellComponent, IFn updateItem, IFn cellConstructor, IFn cellDestructor) {
    this.cellComponent = cellComponent;
    this.updateItem = updateItem;
    this.cellConstructor = cellConstructor;
    this.cellDestructor = cellDestructor;
  }

  @Override
  public TableCell call(TableColumn o) {
    TableCell cell = new TableCell(o, cellComponent, updateItem, cellConstructor, cellDestructor);
    cells.add(cell);
    return cell;
  }

  @Override
  public ArrayList<Cell> getCells() {
    return cells;
  }
}
