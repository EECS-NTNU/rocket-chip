package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._
import chisel3.core.WireInit
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.diplomacy.RegionType
import freechips.rocketchip.tile.{CoreBundle, CoreModule, XLen}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._
import chisel3.internal.sourceinfo.SourceInfo


class NaiveITLB(lgMaxSize: Int, cfg: TLBConfig)(implicit edge: TLEdgeOut, p: Parameters) extends BaseTLB(
  lgMaxSize = lgMaxSize,
  cfg = cfg
)(edge, p) {
  val tlbEntryCnt = 2
  class TLBEntry() extends Bundle {
    val tag = UInt(width = vpnBits)
    val ppn = UInt(width = ppnBits)
    val valid = Bool()

    def hit(vpn: UInt) = {
      valid && vpn === tag
    }
    def invalidate() { valid := false }
    def insert(tag: UInt, ppn : UInt) {
      this.tag := tag
      this.ppn := ppn
      this.valid := true
    }
  }

  val priv = io.ptw.status.prv
  val priv_uses_vm = priv <= PRV.S
  // indicates whether address should be translated
  val vm_enabled = WireInit(Bool(usingVM) && io.ptw.ptbr.mode(io.ptw.ptbr.mode.getWidth-1) && priv_uses_vm && !io.req.bits.passthrough)

  val tlb = Reg(Vec(tlbEntryCnt, new TLBEntry()))
  val vpn = WireInit(io.req.bits.vaddr(vaddrBits-1, pgIdxBits))
  val hitsVec = tlb.map(vm_enabled && _.hit(vpn))
  val ppn = Mux1H(hitsVec :+ !vm_enabled, tlb.map(_.ppn(vpn)) :+ vpn(ppnBits-1, 0))

  val do_refill = Bool(usingVM) && io.ptw.resp.valid
  val s_ready :: s_request :: s_wait :: s_wait_invalidate :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_ready)
  val r_refill_tag = Reg(UInt(width = vpnBits))
  val r_repl_addr = Reg(UInt())
  val tlb_miss = (vm_enabled && !hitsVec.orR)

  val bad_va = vm_enabled &&
    (if (vpnBits == vpnBitsExtended) Bool(false)
    else (io.req.bits.vaddr.asSInt < 0.S) =/= (vpn.asSInt < 0.S))

  // ignore those for now
//  io.resp.pf.ld := (bad_va && isRead(io.req.bits.cmd)) || (pf_ld_array & hits).orR
//  io.resp.pf.st := (bad_va && isWrite(io.req.bits.cmd)) || (pf_st_array & hits).orR
  io.resp.pf.inst := bad_va || false//(pf_inst_array & hits).orR
//  io.resp.ae.ld := (ae_ld_array & hits).orR
//  io.resp.ae.st := (ae_st_array & hits).orR
  io.resp.ae.inst := false//(~px_array & hits).orR
//  io.resp.ma.ld := (ma_ld_array & hits).orR
//  io.resp.ma.st := (ma_st_array & hits).orR

  io.resp.ma.inst := false // this is up to the pipeline to figure out
  io.resp.cacheable := false.B //(c_array & hits).orR
  io.resp.prefetchable := false.B //(prefetchable_array & hits).orR && edge.manager.managers.forall(m => !m.supportsAcquireB || m.supportsHint)


  io.req.ready := state === s_ready
  io.resp.miss := do_refill || tlb_miss
  io.resp.paddr := Cat(ppn, io.req.bits.vaddr(pgIdxBits-1, 0))

  io.ptw.req.valid := state === s_request
  io.ptw.req.bits.valid := !io.kill
  io.ptw.req.bits.bits.addr := r_refill_tag


  val superpage_insert = WireInit(false.B)
  val special_page_insert = WireInit(false.B)
  val invalidate_refill = state.isOneOf(s_request /* don't care */, s_wait_invalidate)
  val insert_ppn = Wire(UInt())
  val plru = new PseudoLRU(tlbEntryCnt)
  // update plru
  when (io.req.valid && vm_enabled) {
    when (hitsVec.orR) { plru.access(OHToUInt(hitsVec)) }
  }

  when (do_refill && !invalidate_refill) {
    when (!io.ptw.resp.bits.homogeneous) {
      special_page_insert := true.B
    }
    val pte = io.ptw.resp.bits.pte
    when(io.ptw.resp.bits.level < pgLevels - 1) {
      // handle inserting superpage fractions here
      val ppn = Wire(Vec(pgLevels - 1, UInt(pgLevelBits.W)))
      for (j <- 1 until pgLevels) {
        val base = (pgLevels - j - 1) * pgLevelBits
        when(io.ptw.resp.bits.level < j) {
          ppn(j - 1) := r_refill_tag(base + pgLevelBits - 1, base)
        }.otherwise {
          ppn(j - 1) := pte.ppn(base + pgLevelBits - 1, base)
        }
      }
      val ppnTop = pte.ppn(ppnBits - 1, (pgLevels - 1) * pgLevelBits) :: Nil
      insert_ppn := Cat(ppnTop ++ ppn)
      //        var res = pte.ppn >> pgLevelBits*(pgLevels - 1)
      //        for (j <- 1 until pgLevels) {
      //          val ignore = io.ptw.resp.bits.level < j
      //          res = Cat(res, (Mux(ignore, vpn, 0.U) | pte.ppn)(vpnBits - j*pgLevelBits - 1, vpnBits - (j + 1)*pgLevelBits))
      //        }
      //        newEntry.ppn := res
      superpage_insert := true.B
    }
    val waddr = r_repl_addr
    for ((e, i) <- tlb.zipWithIndex) when(waddr === i) {
      e.insert(r_refill_tag, insert_ppn)
    }
  }

  if (usingVM) {
    val sfence = io.sfence.valid
    when(io.req.fire() && tlb_miss) {
      state := s_request
      r_refill_tag := vpn

      //      r_superpage_repl_addr := replacementEntry(superpage_entries, superpage_plru.replace)
      r_repl_addr := replacementEntry(tlb, plru.replace)

    }
    when(state === s_request) {
      when(sfence) {
        state := s_ready
      }
      when(io.ptw.req.ready) {
        state := Mux(sfence, s_wait_invalidate, s_wait)
      }
      when(io.kill) {
        state := s_ready
      }
    }
    when(state === s_wait && sfence) {
      state := s_wait_invalidate
    }
    when(io.ptw.resp.valid) {
      state := s_ready
    }

    when(sfence) {
      assert(!io.sfence.bits.rs1 || (io.sfence.bits.addr >> pgIdxBits) === vpn)
      for (e <- tlb) {
        // invalidate all for now
        e.invalidate()
      }
    }
  }
  def replacementEntry(set: Seq[TLBEntry], alt: UInt) = {
    val valids = set.map(_.valid).asUInt
    Mux(valids.andR, alt, PriorityEncoder(~valids))
  }
}
