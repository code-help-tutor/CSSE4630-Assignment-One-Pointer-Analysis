WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
package tip.analysis

import tip.ast.{ADeclaration, DepthFirstAstVisitor, _}
import tip.solvers._
import tip.util.Log
import tip.ast.AstNodeData.DeclarationData
import scala.language.implicitConversions

class AndersenAnalysis(program: AProgram)(implicit declData: DeclarationData) extends DepthFirstAstVisitor[Unit] with PointsToAnalysis {

  val log = Log.logger[this.type]()

  sealed trait Cell
  case class Alloc(alloc: AAlloc) extends Cell {
    override def toString = s"alloc-${alloc.loc}"
  }
  case class Var(id: ADeclaration) extends Cell {
    override def toString = id.toString
  }
  case class FieldId(id: ADeclaration, field: String) extends Cell {
    override def toString = s"$id.$field"
  }

  case class FieldAlloc(alloc: AAlloc, field: String) extends Cell {
    override def toString = s"$alloc.$field"
  }

  val solver = new SimpleCubicSolver[Cell, Cell]

  import AstOps._
  val cells: Set[Cell] = (program.appearingIds.map(Var): Set[Cell]) union program.appearingAllocs.map(Alloc)
  val Fields: Set[String] = program.appearingFields

  NormalizedForPointsToAnalysis.assertContainsProgram(program)
//  NoRecords.assertContainsProgram(program)

  /**
    * @inheritdoc
    */
  def analyze(): Unit =
    visit(program, ())

  /**
    * Generates the constraints for the given sub-AST.
    * @param node the node for which it generates the constraints
    * @param arg unused for this visitor
    */
  def visit(node: AstNode, arg: Unit): Unit = {

    implicit def identifierToTarget(id: AIdentifier): Var = Var(id)
    implicit def allocToTarget(alloc: AAlloc): Alloc = Alloc(alloc)
    implicit def fieldIdToTarget(id: AIdentifier, field: String): FieldId = FieldId(id, field)
    implicit def fieldAllocToTarget(alloc: AAlloc, field: String): FieldAlloc = FieldAlloc(alloc, field)

    node match {
      case AAssignStmt(id: AIdentifier, alloc: AAlloc, _) =>
        // X = alloc P: alloc-i in [[X]]
        alloc match {
          case AAlloc(ARecord(fields, _), _) =>
            val at = allocToTarget(alloc)
            solver.addConstantConstraint(at, identifierToTarget(id))
            fields.foreach {
              case ARecordField(field, expr, _) =>
                expr match {
                  case id2: AIdentifier =>
                    solver.addSubsetConstraint(identifierToTarget(id2), fieldAllocToTarget(alloc, field))
                  case AVarRef(id2: AIdentifier, _) =>
                    solver.addSubsetConstraint(identifierToTarget(id2), fieldAllocToTarget(alloc, field))
                  case _ =>
                }
            }

          case AAlloc(id2: AIdentifier, _) =>
            solver.addConstantConstraint(allocToTarget(alloc), identifierToTarget(id))

          case _ =>
        }

      case AAssignStmt(id1: AIdentifier, AVarRef(id2: AIdentifier, _), _) =>
        // X1 = &X2: X2 in [[X1]]
        solver.addConstantConstraint(identifierToTarget(id2), identifierToTarget(id1))

      case AAssignStmt(id1: AIdentifier, ARecord(fields, _), _) =>
        fields.foreach {
          case ARecordField(field, expr, _) =>
            expr match {
              case id2: AIdentifier =>
                solver.addSubsetConstraint(fieldIdToTarget(id2, field), identifierToTarget(id1))
              case _ =>
            }
        }

      case AAssignStmt(id1: AIdentifier, AFieldAccess(id2: AIdentifier, field, _), _) =>
        solver.addSubsetConstraint(fieldIdToTarget(id1, field), identifierToTarget(id1))

      case AAssignStmt(id1: AIdentifier, id2: AIdentifier, _) =>
        // X1 = X2: [[X2]] is a subset of [[X1]]
        solver.addSubsetConstraint(identifierToTarget(id2), identifierToTarget(id1))

      case AAssignStmt(id1: AIdentifier, AUnaryOp(DerefOp, id2: AIdentifier, _), _) => // TODO(nathan): handle pointers to records
        // X1 = *X2: c in [[X2]] => [[c]] is a subset of [[X1]] for each c in Cells
        cells.foreach { cell =>
          solver.addConditionalConstraint(cell, identifierToTarget(id2), cell, identifierToTarget(id1));
        }

      case AAssignStmt(ADerefWrite(id1: AIdentifier, _), id2: AIdentifier, _) =>
        // *X1 = X2: c in [[X1]] => [[c]] is a subset of [[X2]] for each c in Cells
        cells.foreach { cell =>
          solver.addConditionalConstraint(cell, identifierToTarget(id1), cell, identifierToTarget(id2))
        }

      case _ =>
    }
    visitChildren(node, ())
  }

  /**
    * @inheritdoc
    */
  def pointsTo(): Map[ADeclaration, Set[AstNode]] = {
    val pointsTo = solver.getSolution.collect {
      case (v: Var, ts: Set[Cell]) =>
        v.id -> ts.map {
          case Var(x) => x
          case Alloc(m) => m
          case FieldId(i, _) => i
          case FieldAlloc(a, _) => a
        }
    }
    log.info(s"Points-to:\n${pointsTo.mapValues(v => s"{${v.mkString(",")}}").mkString("\n")}")
    pointsTo
  }

  /**
    * @inheritdoc
    */
  def mayAlias(): (ADeclaration, ADeclaration) => Boolean = { (x: ADeclaration, y: ADeclaration) =>
    solver.getSolution(Var(x)).intersect(solver.getSolution(Var(y))).nonEmpty
  }
}
