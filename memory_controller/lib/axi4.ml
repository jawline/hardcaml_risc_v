module Make (Config : Axi4_config_intf.Config) = struct
  open Config

  module I = struct
    type 'a t =
      { s_axi_bvalid : 'a
      ; s_axi_wready : 'a
      ; s_axi_bid : 'a [@bits id_width]
      ; s_axi_bresp : 'a [@bits 2]
      ; s_axi_rvalid : 'a
      ; s_axi_rid : 'a [@bits id_width]
      ; s_axi_rdata : 'a [@bits data_width]
      ; s_axi_rresp : 'a [@bits 2]
      ; s_axi_rlast : 'a [@bits 1]
      ; s_axi_rready : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { s_axi_awvalid : 'a
      ; s_axi_awid : 'a [@bits id_width]
      ; s_axi_awaddr : 'a [@bits addr_width]
      ; s_axi_awlena : 'a [@bits 8]
      ; s_axi_awsize : 'a [@bits 3]
      ; s_axi_awburst : 'a [@bits 2]
      ; s_axi_wdata : 'a [@bits data_width]
      ; s_axi_wstrb : 'a [@bits data_width / 8]
      ; s_axi_wlast : 'a
      ; s_axi_arvalid : 'a
      ; s_axi_arid : 'a [@bits id_width]
      ; s_axi_araddr : 'a [@bits addr_width]
      ; s_axi_arlen : 'a [@bits 8]
      ; s_axi_arsize : 'a [@bits 3]
      ; s_axi_arburst : 'a [@bits 2]
      ; s_axi_rready : 'a
      }
    [@@deriving hardcaml]
  end
end
