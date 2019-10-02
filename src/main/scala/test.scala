
package teststuff

import chisel3.{Driver}
import chisel3.experimental.RawModule
import chisel3.internal.firrtl.Circuit
import freechips.rocketchip.config.{Config, Parameters}
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.util.HasGeneratorUtilities

object test extends App with HasGeneratorUtilities{
  // ~/git/rocket-chip/vsim$ make verilog
  // =
  // sbt "runMain freechips.rocketchip.system.Generator /home/david/git/rocket-chip/vsim/generated-src freechips.rocketchip.system TestHarness freechips.rocketchip.system DefaultConfig"
  // =
  //    val targetDir = "/home/david/git/rocket-chip/vsim/generated-src"
  val topModuleProject = "freechips.rocketchip.system"
  val topModuleClass = "TestHarness"
  //    val configProject = "freechips.rocketchip.system"
  //    val configs = "DefaultConfig"
  val fullTopModuleClass: String = topModuleProject + "." + topModuleClass

  import freechips.rocketchip.system.TestHarness
  import freechips.rocketchip.system.DefaultConfig
  val config = new Config(new DefaultConfig)
  // implicit is some kind of magic that selects it as default parameter
  val params: Parameters = config.toInstance
  val top: () => RawModule = () =>
    Class.forName(fullTopModuleClass)
      .getConstructor(classOf[Parameters])
      .newInstance(params) match {
      case m: RawModule => m
      case l: LazyModule => LazyModule(l).module
    }
  println(chisel3.Driver.emitVerilog(top()))
  ////    val circuit = Driver.elaborate(()=>module)
  //    Driver.emitVerilog[TestHarness](module)
}
