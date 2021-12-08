import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, ChoiceDialog, Label, Menu, MenuBar, MenuItem, TableColumn, TableView, TextInputDialog}
import scalafx.scene.layout.VBox
import scalafx.scene.paint.Color.White
import java.io.File

object Application extends JFXApp3 {
  var vbox: VBox = null
  var nodesTable: TableView[NodesTableElement] = null
  var typeBuilder: TypeBuilder = null
  var linkedListInt = new LinkedList[Int]
  var linkedListString = new LinkedList[String]

  class NodesTableElement(val _value: String) {
    val value = new StringProperty(_value)
  }

  override def start(): Unit = {
    vbox = new VBox {
      autosize()
    }

    stage = new PrimaryStage {
      scene = new Scene(600, 800) {
        title = "LinkedListScalaGUI"
        fill = White
        root = vbox
      }
    }

    val choices = TypeFactory.getTypeNameList

    val dialog = new ChoiceDialog(defaultChoice = choices.head, choices = choices) {
      initOwner(stage)
      title = "Выбор"
      headerText = "Выберите тип данных"
    }

    val result = dialog.showAndWait()

    result match {
      case Some(choice) =>
        if (choice.equals("Integer")) {
          typeBuilder = TypeFactory.getBuilder(MyTypes.Integer)
          linkedListInt = new LinkedList[Int]()
        }
        else if (choice.equals("String")) {
          typeBuilder = TypeFactory.getBuilder(MyTypes.String)
          linkedListString = new LinkedList[String]()
        }
        else {
          println("Ошибка типа")
          this.stage.close()
        }
      case None =>
        println("Ничего не было выбрано")
        this.stage.close()
    }

    configLayers(vbox)
  }

  def updateData(): Unit = {
    val nodes: ObservableBuffer[NodesTableElement] = ObservableBuffer[NodesTableElement]()

    if(typeBuilder.typeName.equals("Integer")) {
      for(i <- 0 until linkedListInt.getSize) {
        nodes.addOne(new NodesTableElement(linkedListInt.get(i).toString))
      }
    }
    else {
      for(i <- 0 until linkedListString.getSize) {
        nodes.addOne(new NodesTableElement(linkedListString.get(i)))
      }
    }

    updateNodesTableView(nodes)
    val label = new Label(" Размер списка: " + linkedListInt.getSize)
    vbox.children.set(1, label)
    vbox.children.set(2, nodesTable)
  }

  def updateNodesTableView(nodes: ObservableBuffer[NodesTableElement]): Unit = {
    nodesTable = new TableView[NodesTableElement] {
      columns ++= Seq (
        new TableColumn[NodesTableElement, String] {
          text = "Значения узлов"
          prefHeight = 760
          cellValueFactory = _.value.value
        }
      )
      items = nodes
    }
    nodesTable.columnResizePolicy = TableView.ConstrainedResizePolicy
  }

  def configLayers(root: VBox): Unit = {
    val menuBar = new MenuBar()

    //Файл
    val mainMenu = new Menu("Файл")

    val saveMainItem = new MenuItem("Сохранить")
    saveMainItem.onAction = _ => {
      val file = new File(typeBuilder.typeName + ".dat")
      if (typeBuilder.typeName.equals("Integer")) {
        linkedListInt.save(file)
      } else {
        linkedListString.save(file)
      }
    }

    val loadMainItem = new MenuItem("Загрузить")
    loadMainItem.onAction = _ => {
      val file = new File(typeBuilder.typeName + ".dat")
      if (typeBuilder.typeName.equals("Integer")) {
        linkedListInt.load(file)
      } else {
        linkedListString.load(file)
      }
      updateData()
    }

    val exitMainItem = new MenuItem("Выход")
    exitMainItem.onAction = _ => {
      this.stage.close()
    }

    mainMenu.items = List(saveMainItem, loadMainItem, exitMainItem)

    //Работа со списком
    val actionMenu = new Menu("Работа со списком")

    val addActionItem = new MenuItem("Добавить n-ое количество элементов")
    addActionItem.onAction = _ => {
      val dialog = new TextInputDialog(defaultValue = "10") {
        initOwner(stage)
        title = "Добавление"
        headerText = "Введите количество добавляемых элементов в список"
        contentText = "Количество: "
      }

      val result = dialog.showAndWait()

      result match {
        case Some(count) =>
          if(typeBuilder.typeName.equals("Integer")) {
            for(i <- 0 until count.toInt) {
              linkedListInt.pushFront(typeBuilder.create.asInstanceOf[Int])
            }
          }
          else {
            for(i <- 0 until count.toInt) {
              linkedListString.pushFront(typeBuilder.create.asInstanceOf[String])
            }
          }
        case None =>
          println("Отмена добавления")
      }

      updateData()
    }

    val deleteActionItem = new MenuItem("Удаление по индексу")
    deleteActionItem.onAction = _ => {
      val dialog = new TextInputDialog(defaultValue = "0") {
        initOwner(stage)
        title = "Удаление"
        headerText = "Введите индекс элемента, который хотите удалить"
        contentText = "Индекс: "
      }

      val result = dialog.showAndWait()

      result match {
        case Some(index) =>
          if(typeBuilder.typeName.equals("Integer")) {
            linkedListInt.remove(index.toInt)
          }
          else {
              linkedListString.remove(index.toInt)
          }
        case None =>
          println("Отмена удаления")
      }

      updateData()
    }

    val sortActionItem = new MenuItem("Сортировка")
    sortActionItem.onAction = _ => {
      val start = System.nanoTime
      if(typeBuilder.typeName.equals("Integer")) {
        linkedListInt.sort(typeBuilder.typeComparator)
      } else {
        linkedListString.sort(typeBuilder.typeComparator)
      }
      val time = (System.nanoTime - start).toDouble / 1_000_000_000.0

      new Alert(AlertType.Information) {
        initOwner(stage)
        title = "Информация"
        headerText = "Список был успешно отсортирован"
        contentText = s"На это потребовалось ${BigDecimal(time).setScale(3, BigDecimal.RoundingMode.HALF_UP)} cек"
      }.showAndWait()

      updateData()
    }

    actionMenu.items = Seq(addActionItem, deleteActionItem, sortActionItem)

    menuBar.menus = Seq(mainMenu, actionMenu)

    val label = new Label(" Размер списка: 0")
    updateNodesTableView(ObservableBuffer[NodesTableElement]())

    root.children = Seq (
      menuBar,
      label,
      nodesTable
    )
  }
}