package robear

import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import java.io._

trait Operation
trait Condition

case class Select(columns: Seq[String], var table: Option[String], var whereCond: Option[Seq[Condition]]) extends Operation {
  def from(tableName: String) = {
    table = Some(tableName)
    this
  }
  def where(condition: (Condition)*) = {
    whereCond = Some(condition)
    this
  }
}

case class Insert(tableName: String, var data: Option[Map[String, String]]) extends Operation {
  def values(datas: (String, String)*) = {
    data = Some(Map(datas map { (d) => d._1 -> d._2 }: _*))
    this
  }
}

case class Create(tableName: String, var columns: Option[Seq[String]]) extends Operation {
  def cols(datas: String*) = {
    columns = Some(datas)
    this
  }
}

case class Eq(column: String, condition: String) extends Condition
case class Contains(column: String, condition: String) extends Condition
case class FileDBError(msg: String) extends Exception

class FileDB(dbDirPath: String) {
  val dbDir = new File(dbDirPath)
  if (!dbDir.exists()) dbDir.mkdirs()

  def createDatabase(dbName: String): Unit = {
    val dbPath = new File(dbDir, dbName)
    if (!dbPath.exists()) dbPath.mkdirs()
  }

  def getTablePath(dbName: String, tableName: String): File = {
    new File(new File(dbDir, dbName), tableName + ".csv")
  }

  def executeQuery(dbName: String, op: Operation): Option[Map[String, ArrayBuffer[String]]] = {
    op match {
      case Select(columns, tableName, where) =>
        tableName match {
          case Some(tblName) =>
            val tablePath = getTablePath(dbName, tblName)
            if (tablePath.exists()) {
              val result = readTable(tablePath, columns)
              where match {
                case Some(conditions) =>
                  val filteredResult = conditions.foldLeft(result) { (res, cond) =>
                    cond match {
                      case Eq(column, value) => filterData(res, column, _.equals(value))
                      case Contains(column, value) => filterData(res, column, _.contains(value))
                    }
                  }
                  Some(filteredResult)
                case None => Some(result)
              }
            } else {
              throw new FileDBError(s"Table $tblName does not exist in database $dbName")
            }
          case None => throw new FileDBError("Table name is required for SELECT query")
        }

      case Insert(tableName, data) =>
        data match {
          case Some(rowData) =>
            val tablePath = getTablePath(dbName, tableName)
            if (tablePath.exists()) {
              appendToTable(tablePath, rowData)
            } else {
              throw new FileDBError(s"Table $tableName does not exist in database $dbName")
            }
            None
          case None => throw new FileDBError("Data is required for INSERT query")
        }

      case Create(tableName, columns) =>
        columns match {
          case Some(cols) =>
            val tablePath = getTablePath(dbName, tableName)
            if (!tablePath.exists()) {
              createTable(tablePath, cols)
            } else {
              throw new FileDBError(s"Table $tableName already exists in database $dbName")
            }
            None
          case None => throw new FileDBError("Columns are required for CREATE TABLE query")
        }
    }
  }

  private def readTable(file: File, columns: Seq[String]): Map[String, ArrayBuffer[String]] = {
    val result = Map[String, ArrayBuffer[String]]()
    val reader = Source.fromFile(file)
    val lines = reader.getLines()
    val header = lines.next().split(",").toSeq

    val sortedColumns = columns.sorted
    sortedColumns.foreach { col =>
      if (!header.contains(col)) throw new FileDBError(s"Column $col does not exist in the table")
      result += (col -> ArrayBuffer[String]())
    }

    lines.foreach { line =>
      val fields = line.split(",")
      sortedColumns.zipWithIndex.foreach { case (col, index) =>
        val headerIndex = header.indexOf(col)
        result(col) += fields(headerIndex)
      }
    }

    reader.close()
    result
  }

  private def appendToTable(file: File, data: Map[String, String]): Unit = {
    val writer = new FileWriter(file, true)
    val sortedData = data.toSeq.sortBy(_._1).map(_._2)
    writer.write(sortedData.mkString(",") + "\n")
    writer.close()
  }

  private def createTable(file: File, columns: Seq[String]): Unit = {
    val writer = new FileWriter(file)
    val sortedColumns = columns.sorted
    writer.write(sortedColumns.mkString(",") + "\n")
    writer.close()
  }

  private def filterData(
      data: Map[String, ArrayBuffer[String]],
      column: String,
      predicate: String => Boolean
  ): Map[String, ArrayBuffer[String]] = {
    val filteredData = data(column).zipWithIndex.filter { case (value, _) => predicate(value) }.map(_._2)
    data.map { case (col, values) =>
      col -> filteredData.map(values)
    }
  }
}

package object filedb {
  def select(columns: String*) = Select(columns, None, None)
  def insert(tableName: String) = Insert(tableName, None)
  def create(tableName: String) = Create(tableName, None)
  def equal(column: String, condition: String) = Eq(column, condition)
  def contains(column: String, condition: String) = Contains(column, condition)
  def query(db: FileDB, dbName: String, op: Operation): Option[Map[String, ArrayBuffer[String]]] = db.executeQuery(dbName, op)
}
