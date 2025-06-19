module M (Config : Axi4_config_intf.Config) = struct
  module type S = sig
    module I : sig
      type 'a t =
        { s_axi_bvalid : 'a
        ; s_axi_wready : 'a
        ; s_axi_bid : 'a
        ; s_axi_bresp : 'a
        ; s_axi_rvalid : 'a
        ; s_axi_rid : 'a
        ; s_axi_rdata : 'a
        ; s_axi_rresp : 'a
        ; s_axi_rlast : 'a
        ; s_axi_rready : 'a
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { s_axi_awvalid : 'a
        ; s_axi_awid : 'a
        ; s_axi_awaddr : 'a
        ; s_axi_awlena : 'a
        ; s_axi_awsize : 'a
        ; s_axi_awburst : 'a
        ; s_axi_wdata : 'a
        ; s_axi_wstrb : 'a
        ; s_axi_wlast : 'a
        ; s_axi_arvalid : 'a
        ; s_axi_arid : 'a
        ; s_axi_araddr : 'a
        ; s_axi_arlen : 'a
        ; s_axi_arsize : 'a
        ; s_axi_arburst : 'a
        ; s_axi_rready : 'a
        }
      [@@deriving hardcaml]
    end
  end
end
