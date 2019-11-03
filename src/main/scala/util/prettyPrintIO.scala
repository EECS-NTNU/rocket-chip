package util

import Chisel._
import freechips.rocketchip.diplomacy.AutoBundle
import freechips.rocketchip.util.HeterogeneousBag

object prettyPrintIO {
  def apply(d: Data, depth: Int = 0, indent: String = "  "): String = {
    val desc = d match {
      case aggregate: Aggregate => aggregate match {
        case vec: Vec[_] => {
          val children = vec.map((d) => s"${indent * (depth + 1)}${prettyPrintIO(d, depth + 1)},\n"
          ).reduce(_ + _)
          s"Vec(\n$children${indent * depth})"
        }
        case record: Record => {
          val className = record.getClass.getName
          val name = if (className.contains("$")) record.className else className
          val children = if (record.elements.nonEmpty) record.elements.toList.reverse.map {
            case (st, d) => s"${indent * (depth + 1)}$st = ${prettyPrintIO(d, depth + 1)}\n"
          }.reduce(_ + _) else ""
          s"$name{\n$children${indent * depth}}"
        }
//          record match {
//          case bundle: Bundle => {
//            val className = bundle.getClass.getName
//            val name = if (className.contains("$")) bundle.className else className
//            val children = if (bundle.elements.nonEmpty) bundle.elements.toList.reverse.map {
//              case (st, d) => s"${indent * (depth + 1)}$st = ${prettyPrintIO(d, depth + 1)}\n"
//            }.reduce(_ + _) else ""
//            s"$name{\n$children${indent * depth}}"
//          }
//          case bundle: AutoBundle => {
//            val className = bundle.getClass.getName
//            val name = if (className.contains("$")) bundle.className else className
//            val children = if (bundle.elements.nonEmpty) bundle.elements.toList.reverse.map {
//              case (st, d) => s"${indent * (depth + 1)}$st = ${prettyPrintIO(d, depth + 1)}\n"
//            }.reduce(_ + _) else ""
//            s"$name{\n$children${indent * depth}}"
//          }
//          case HeterogeneousBag(elts) => {
//            val name = "HeterogeneousBag"
//            val children = if (record.elements.nonEmpty) record.elements.toList.reverse.map {
//              case (st, d) => s"${indent * (depth + 1)}$st = ${prettyPrintIO(d, depth + 1)}\n"
//            }.reduce(_ + _) else ""
//            s"$name{\n$children${indent * depth}}"
//          }
//          case _ => record.className
//        }
      }
      case element: Element => {
        val widthInfo = s"(${if (element.widthKnown) element.getWidth + ".W" else ""})"
        element match {
          case bool: Bool => "Bool()"
          case uint: UInt => s"UInt($widthInfo)"
          case sint: SInt => s"SInt($widthInfo)"
          case _ => s"${element.getClass.getName}(${if (element.widthKnown) element.getWidth else "?"})"
        }
      }
      case _ => "Data??"
    }
    d.dir match {
      case Chisel.INPUT => s"Input($desc)"
      case Chisel.NODIR => s"$desc"
      case Chisel.OUTPUT => s"Output($desc)"
    }
  }
}
