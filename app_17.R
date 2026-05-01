# =============================================================================
# DGE2Pathview Explorer — app v17
# -----------------------------------------------------------------------------
# Changes from v16:
#   - Fixed extra closing parenthesis in output$pkg_install_ui (line 1392)
#     paste0('...'))  was paste0('...'))  but had an extra ) → caused sourcing error
# (All other 'possible missing comma' linter warnings are false positives from
#  if/else expressions inside function arguments — valid R syntax)
# Changes inherited from v16:
#   - Added static_swatch() helper function (was missing → caused startup error)
#   - Added DEP (Proteomics) sidebar section and 4 DEP tabs
#   - Added DGE Heatmap sidebar item and tabItem
#   - Added global hidden inputs + full 11-ID swatch JS system (sessioninitialized)
#   - Added metadata column mapping UI for DESeq2 upload mode (dge_meta_col_ui)
#   - Added dist_contrast_ui and hm_contrast_ui contrast selectors
#   - Added dep reactiveValues (declared before sidebar_status to prevent crash)
#   - Full DEP server module (normalisation, imputation, limma, volcano, heatmap)
#   - Full DGE heatmap server (heatmap_data reactive, renderPlot, download)
#   - Updated sidebar_status to show DEP status rows
#   - Pathway zoom: scroll-to-zoom + drag-to-pan (from v15, retained)
#   - Help tab: formatted HTML tables for all input formats (from v15, retained)
# =============================================================================

options(shiny.maxRequestSize = 1000 * 1024^2)

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(ggplot2)
library(plotly)

# -- Static swatch helper — rendered once as plain HTML; JS handles interaction
# No renderUI means grids can never be collapsed by Shiny re-renders.
static_swatch <- function(input_id, label, default_name, default_hex) {
  tagList(
    tags$div(class="swatch-group",
      tags$div(class="swatch-label",
        tags$span(label),
        tags$span(id=paste0("preview_", input_id),
                  class="swatch-preview",
                  style=paste0("background-color:", default_hex, ";")),
        tags$span(id=paste0("swtext_", input_id),
                  style="font-size:11px;color:#718096;font-family:'DM Mono',monospace;",
                  default_name)
      ),
      tags$div(id=paste0("swatches_", input_id), class="swatch-grid")
    )
  )
}

# -- UI ----------------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://www.genome.jp/Fig/kegg128.gif",
               height = "28px", style = "margin-right:8px; vertical-align:middle;"),
      "DGE2Pathview Explorer"
    ),
    titleWidth = 260
  ),

  dashboardSidebar(
    width = 260,
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;600&family=DM+Mono:wght@400;500&display=swap');
        * { font-family: 'DM Sans', sans-serif; }
        code, .mono { font-family: 'DM Mono', monospace; }
        .skin-blue .main-header .logo { background: #1a73e8; border-bottom: 1px solid #1558b0; }
        .skin-blue .main-header .logo:hover { background: #1558b0; }
        .skin-blue .main-header .navbar { background: #1a73e8; border-bottom: 1px solid #1558b0; }
        .skin-blue .main-sidebar { background: #ffffff; border-right: 1px solid #e0e8f5; }
        .skin-blue .sidebar-menu > li > a { color: #4a5568; font-weight: 400; font-size: 13px; }
        .skin-blue .sidebar-menu > li.active > a,
        .skin-blue .sidebar-menu > li > a:hover { color: #1a73e8; background: #e8f0fe; border-left: 3px solid #1a73e8; }
        .skin-blue .sidebar-menu > li > a .fa { color: #1a73e8; }
        .skin-blue .sidebar-menu > li.active > a .fa { color: #1a73e8; }
        .content-wrapper { background: #f0f4f8; }
        .main-footer { background: #ffffff; border-top: 1px solid #e0e8f5; color: #718096; }
        .box { background: #ffffff; border: 1px solid #e0e8f5; border-top: 3px solid #1a73e8;
               border-radius: 8px; box-shadow: 0 2px 12px rgba(26,115,232,0.08); }
        .box-header { background: transparent; border-bottom: 1px solid #e8f0fe; padding: 12px 18px; }
        .box-title { color: #1a73e8; font-weight: 600; font-size: 12px; letter-spacing: 0.07em; text-transform: uppercase; }
        .btn-primary { background: #1a73e8; border-color: #1558b0; font-weight: 600; letter-spacing: 0.04em; border-radius: 6px; }
        .btn-primary:hover { background: #1558b0; border-color: #0f3f8a; box-shadow: 0 4px 12px rgba(26,115,232,0.3); }
        .btn-success { background: #0f9d58; border-color: #0b7a43; font-weight: 600; border-radius: 6px; }
        .btn-success:hover { background: #0b7a43; border-color: #08582f; }
        .btn-warning { background: #f4a433; border-color: #e08e10; font-weight: 600; border-radius: 6px; color: #fff; }
        .btn-default { border-radius: 6px; }
        .form-control, .selectize-input, textarea {
          background: #ffffff !important; border: 1px solid #c9d8f0 !important;
          color: #2d3748 !important; border-radius: 6px; font-size: 13px; }
        .form-control:focus, .selectize-input.focus {
          border-color: #1a73e8 !important; box-shadow: 0 0 0 3px rgba(26,115,232,0.15) !important; }
        .selectize-dropdown { background: #ffffff; border: 1px solid #c9d8f0; border-radius: 6px;
                              box-shadow: 0 4px 16px rgba(0,0,0,0.1); }
        .selectize-dropdown-content .option { color: #4a5568; }
        .selectize-dropdown-content .option:hover,
        .selectize-dropdown-content .option.active { background: #e8f0fe; color: #1a73e8; }
        label { color: #4a5568; font-size: 12px; font-weight: 600; letter-spacing: 0.05em; text-transform: uppercase; }
        .well { background: #f8faff; border: 1px solid #e0e8f5; box-shadow: none; border-radius: 6px; }
        hr { border-color: #e0e8f5; }
        .nav-tabs { border-bottom: 2px solid #e0e8f5; }
        .nav-tabs > li > a { color: #718096; background: transparent; border: none; font-size: 13px; font-weight: 500; padding: 8px 16px; }
        .nav-tabs > li.active > a, .nav-tabs > li > a:hover { color: #1a73e8; background: transparent;
          border: none; border-bottom: 2px solid #1a73e8; margin-bottom: -2px; }
        .dataTables_wrapper { color: #4a5568; }
        table.dataTable { background: #ffffff; border-radius: 6px; }
        table.dataTable thead th { background: #f0f4f8; color: #1a73e8; border-bottom: 2px solid #e0e8f5; font-size: 12px; font-weight: 600; }
        table.dataTable tbody tr { background: #ffffff; color: #2d3748; }
        table.dataTable tbody tr:hover { background: #f0f4f8; }
        table.dataTable tbody tr td { border-color: #e8f0fe; }
        .dataTables_filter input { background: #ffffff !important; border: 1px solid #c9d8f0 !important; color: #2d3748 !important; border-radius: 6px; }
        .dataTables_length select { background: #ffffff !important; color: #2d3748 !important; border: 1px solid #c9d8f0 !important; border-radius: 4px; }
        .dataTables_info, .dataTables_paginate { color: #718096 !important; }
        .paginate_button { color: #718096 !important; border-radius: 4px !important; }
        .paginate_button.current { background: #e8f0fe !important; color: #1a73e8 !important; border: 1px solid #c9d8f0 !important; }
        .paginate_button:hover { background: #e8f0fe !important; color: #1a73e8 !important; }
        .alert-info { background: #e8f0fe; border-color: #c9d8f0; color: #2d5fa8; font-size: 12px; border-radius: 6px; }
        .alert-success { background: #e6f4ea; border-color: #b7dfc4; color: #1e6e3e; font-size: 12px; border-radius: 6px; }
        .alert-danger { background: #fce8e6; border-color: #f5c6c2; color: #b31b1b; font-size: 12px; border-radius: 6px; }
        .alert-warning { background: #fef7e0; border-color: #f8d7a0; color: #7d5000; font-size: 12px; border-radius: 6px; }
        .status-dot { display:inline-block; width:8px; height:8px; border-radius:50%; margin-right:6px; }
        .status-ok { background: #0f9d58; box-shadow: 0 0 5px rgba(15,157,88,0.5); }
        .status-warn { background: #f4a433; box-shadow: 0 0 5px rgba(244,164,51,0.5); }
        .status-err { background: #db4437; box-shadow: 0 0 5px rgba(219,68,55,0.5); }
        .shiny-plot-output { background: transparent; }
        .sidebar-form .form-group { margin-bottom: 8px; }
        .shiny-notification { background: #ffffff; border: 1px solid #1a73e8; color: #2d3748;
                              border-radius: 8px; box-shadow: 0 4px 16px rgba(0,0,0,0.12); }
        ::-webkit-scrollbar { width: 6px; height: 6px; }
        ::-webkit-scrollbar-track { background: #f0f4f8; }
        ::-webkit-scrollbar-thumb { background: #c9d8f0; border-radius: 3px; }
        ::-webkit-scrollbar-thumb:hover { background: #1a73e8; }
        .info-tile { background: #f8faff; border: 1px solid #e0e8f5; border-radius: 8px;
                     padding: 14px 16px; text-align: center; box-shadow: 0 1px 4px rgba(26,115,232,0.06); }
        .info-tile .tile-val { font-size: 28px; font-weight: 600; color: #1a73e8;
                               font-family: 'DM Mono', monospace; }
        .info-tile .tile-lbl { font-size: 11px; color: #718096; text-transform: uppercase;
                               letter-spacing: 0.07em; margin-top: 3px; }
        .section-divider { color: #1a73e8; font-size: 11px; font-weight: 600;
                           letter-spacing: 0.1em; text-transform: uppercase;
                           padding: 6px 0 4px 0; margin: 8px 0 4px 0;
                           border-bottom: 2px solid #e8f0fe; }
        h3 { color: #1a73e8 !important; }
        .sidebar-status-text { color: #4a5568 !important; font-size: 12px; }
        .skin-blue .sidebar-menu { padding-top: 4px; }
        .main-sidebar hr { border-color: #e0e8f5; margin: 8px 12px; }
        .swatch-group { margin-bottom: 10px; }
        .swatch-label { font-size: 11px; font-weight: 600; color: #4a5568;
                        text-transform: uppercase; letter-spacing: 0.05em;
                        margin-bottom: 5px; display: flex; align-items: center; gap: 8px; }
        .swatch-preview { display: inline-block; width: 18px; height: 18px;
                          border-radius: 4px; border: 2px solid #c9d8f0; vertical-align: middle; }
        .swatch-grid { display: flex; flex-wrap: wrap; gap: 4px; }
        .swatch-btn { width: 22px; height: 22px; border-radius: 4px;
                      border: 2px solid transparent; cursor: pointer;
                      transition: transform 0.1s, border-color 0.15s; padding: 0; outline: none; }
        .swatch-btn:hover { transform: scale(1.2); border-color: #1a73e8; }
        .swatch-btn.selected { border-color: #1a73e8; transform: scale(1.15);
                               box-shadow: 0 0 0 2px rgba(26,115,232,0.3); }
        .pv-zoom-wrap { position: relative; overflow: hidden; border-radius: 6px;
          background: #f8faff; border: 1px solid #e0e8f5;
          box-shadow: 0 2px 12px rgba(0,0,0,0.08); cursor: zoom-in; user-select: none; }
        .pv-zoom-wrap.zoomed  { cursor: grab; }
        .pv-zoom-wrap.dragging { cursor: grabbing; }
        .pv-zoom-wrap img { display: block; width: 100%; max-width: none !important;
          transform-origin: 0 0; will-change: transform; border: none !important; box-shadow: none !important; }
        .pv-zoom-controls { display: flex; align-items: center; gap: 6px; margin-bottom: 8px; flex-wrap: wrap; }
        .pv-zoom-btn { background: #ffffff; border: 1px solid #c9d8f0; color: #1a73e8;
          border-radius: 6px; padding: 4px 12px; cursor: pointer;
          font-size: 12px; font-weight: 600; transition: background 0.15s; }
        .pv-zoom-btn:hover { background: #e8f0fe; }
        .pv-zoom-level { font-size: 12px; font-weight: 600; color: #1a73e8;
          font-family: 'DM Mono', monospace; min-width: 44px; text-align: center;
          background: #f0f4f8; border-radius: 4px; padding: 3px 6px; }
        .pv-zoom-hint { margin-left: auto; font-size: 11px; color: #a0aec0; font-style: italic; }
      "))
    ),

    sidebarMenu(
      id = "tabs",
      tags$li(style="padding:4px 15px 2px;pointer-events:none;",
        tags$span(style="color:#1a73e8;font-size:10px;font-weight:700;letter-spacing:0.1em;text-transform:uppercase;",
          "DGE Analysis (DESeq2)")
      ),
      menuItem("Run DESeq2",        tabName = "deseq2_tab",  icon = icon("flask")),
      menuItem("PCA",               tabName = "pca_tab",     icon = icon("circle-dot")),
      menuItem("Volcano Plot",      tabName = "volc_tab",    icon = icon("chart-line")),
      menuItem("Distribution",      tabName = "dist_dge_tab",icon = icon("chart-area")),
      menuItem("Heatmap",           tabName = "heatmap_tab", icon = icon("th")),
      menuItem("Results Table",     tabName = "dge_res_tab", icon = icon("table")),
      tags$hr(style="border-color:#e0e8f5;margin:4px 10px;"),
      tags$li(style="padding:4px 15px 2px;pointer-events:none;",
        tags$span(style="color:#1a73e8;font-size:10px;font-weight:700;letter-spacing:0.1em;text-transform:uppercase;",
          "Proteomics (DEP)")
      ),
      menuItem("Run DEP",           tabName = "dep_tab",      icon = icon("atom")),
      menuItem("Protein Volcano",   tabName = "dep_volc_tab", icon = icon("chart-line")),
      menuItem("Protein Heatmap",   tabName = "dep_hm_tab",   icon = icon("th")),
      menuItem("DEP Results",       tabName = "dep_res_tab",  icon = icon("table")),
      tags$hr(style="border-color:#e0e8f5;margin:4px 10px;"),
      tags$li(style="padding:4px 15px 2px;pointer-events:none;",
        tags$span(style="color:#1a73e8;font-size:10px;font-weight:700;letter-spacing:0.1em;text-transform:uppercase;",
          "PathView Explorer")
      ),
      menuItem("Data Input",        tabName = "input_tab",   icon = icon("upload")),
      menuItem("Gene ID Converter", tabName = "convert_tab", icon = icon("exchange-alt")),
      menuItem("PathView Plots",    tabName = "pathview_tab",icon = icon("project-diagram")),
      menuItem("Results & Export",  tabName = "results_tab", icon = icon("download")),
      menuItem("Help",              tabName = "help_tab",    icon = icon("question-circle"))
    ),

    tags$hr(),
    tags$div(style="padding:10px 15px;",
      tags$div(class="section-divider", "Quick Status"),
      tags$div(id="dge_sidebar_running", style="display:none;margin-bottom:4px;",
        tags$span(class="status-dot status-warn"),
        tags$span(style="color:#4a5568;font-size:12px;", "DESeq2 running...")
      ),
      uiOutput("sidebar_status")
    )
  ),

  dashboardBody(
    useShinyjs(),

    # -- Global hidden inputs for all plot colors (swatches live in each tab)
    tags$div(style="display:none;",
      tags$input(type="hidden", id="dge_up_col",   value="tomato"),
      tags$input(type="hidden", id="dge_dn_col",   value="turquoise"),
      tags$input(type="hidden", id="dge_ns_col",   value="gray70"),
      tags$input(type="hidden", id="dge_grp1_col", value="royalblue"),
      tags$input(type="hidden", id="dge_grp2_col", value="tomato"),
      tags$input(type="hidden", id="dep_up_col",   value="tomato"),
      tags$input(type="hidden", id="dep_dn_col",   value="steelblue"),
      tags$input(type="hidden", id="dep_ns_col",   value="gray70")
    ),

    # -- Swatch JS + pathway zoom JS -----------------------------------------
    tags$script(HTML("
    // ── Swatch system ────────────────────────────────────────────────────────
    var rcolors = [
      {n:'navy',h:'#001f5b'},{n:'royalblue',h:'#4169E1'},{n:'blue',h:'#0000FF'},
      {n:'dodgerblue',h:'#1E90FF'},{n:'deepskyblue',h:'#00BFFF'},{n:'steelblue',h:'#4682B4'},
      {n:'skyblue',h:'#87CEEB'},{n:'lightblue',h:'#ADD8E6'},{n:'cyan',h:'#00FFFF'},
      {n:'turquoise',h:'#40E0D0'},{n:'darkgreen',h:'#006400'},{n:'forestgreen',h:'#228B22'},
      {n:'green',h:'#00FF00'},{n:'limegreen',h:'#32CD32'},{n:'chartreuse',h:'#7FFF00'},
      {n:'yellowgreen',h:'#9ACD32'},{n:'olivedrab',h:'#6B8E23'},{n:'springgreen',h:'#00FF7F'},
      {n:'aquamarine',h:'#7FFFD4'},{n:'palegreen',h:'#98FB98'},{n:'darkred',h:'#8B0000'},
      {n:'firebrick',h:'#B22222'},{n:'red',h:'#FF0000'},{n:'tomato',h:'#FF6347'},
      {n:'orangered',h:'#FF4500'},{n:'coral',h:'#FF7F50'},{n:'salmon',h:'#FA8072'},
      {n:'hotpink',h:'#FF69B4'},{n:'deeppink',h:'#FF1493'},{n:'pink',h:'#FFC0CB'},
      {n:'purple',h:'#800080'},{n:'darkviolet',h:'#9400D3'},{n:'magenta',h:'#FF00FF'},
      {n:'orchid',h:'#DA70D6'},{n:'plum',h:'#DDA0DD'},{n:'mediumpurple',h:'#9370DB'},
      {n:'blueviolet',h:'#8A2BE2'},{n:'indigo',h:'#4B0082'},{n:'violet',h:'#EE82EE'},
      {n:'lavender',h:'#E6E6FA'},{n:'gold',h:'#FFD700'},{n:'yellow',h:'#FFFF00'},
      {n:'orange',h:'#FFA500'},{n:'darkorange',h:'#FF8C00'},{n:'lightyellow',h:'#FFFFE0'},
      {n:'khaki',h:'#F0E68C'},{n:'wheat',h:'#F5DEB3'},{n:'peachpuff',h:'#FFDAB9'},
      {n:'black',h:'#000000'},{n:'gray10',h:'#1A1A1A'},{n:'gray30',h:'#4D4D4D'},
      {n:'gray',h:'#808080'},{n:'gray70',h:'#B3B3B3'},{n:'gray90',h:'#E5E5E5'},
      {n:'white',h:'#FFFFFF'},{n:'whitesmoke',h:'#F5F5F5'},{n:'snow',h:'#FFFAFA'},
      {n:'ivory',h:'#FFFFF0'}
    ];

    var swSelected = {
      low_col:'blue', mid_col:'gray', high_col:'red',
      dge_up_col:'tomato', dge_dn_col:'turquoise', dge_ns_col:'gray70',
      dge_grp1_col:'royalblue', dge_grp2_col:'tomato',
      dep_up_col:'tomato', dep_dn_col:'steelblue', dep_ns_col:'gray70'
    };

    var allSwatchIds = [
      'low_col','mid_col','high_col',
      'dge_up_col','dge_dn_col','dge_ns_col',
      'dge_grp1_col','dge_grp2_col',
      'dep_up_col','dep_dn_col','dep_ns_col'
    ];

    function buildSwatches(targetId) {
      var container = document.getElementById('swatches_' + targetId);
      if (!container) return;
      container.innerHTML = '';
      rcolors.forEach(function(c) {
        var btn = document.createElement('button');
        btn.className = 'swatch-btn' + (swSelected[targetId] === c.n ? ' selected' : '');
        btn.style.backgroundColor = c.h;
        btn.title = c.n;
        btn.addEventListener('click', function(e) {
          e.preventDefault();
          swSelected[targetId] = c.n;
          var el = document.getElementById(targetId);
          if (el) { el.value = c.n; el.dispatchEvent(new Event('change')); }
          Shiny.setInputValue(targetId, c.n, {priority: 'event'});
          var prev = document.getElementById('preview_' + targetId);
          if (prev) prev.style.backgroundColor = c.h;
          var txt = document.getElementById('swtext_' + targetId);
          if (txt) txt.textContent = c.n;
          buildSwatches(targetId);
        });
        container.appendChild(btn);
      });
    }

    $(document).on('shiny:sessioninitialized', function() {
      var defaults = {
        low_col:'blue', mid_col:'gray', high_col:'red',
        dge_up_col:'tomato', dge_dn_col:'turquoise', dge_ns_col:'gray70',
        dge_grp1_col:'royalblue', dge_grp2_col:'tomato',
        dep_up_col:'tomato', dep_dn_col:'steelblue', dep_ns_col:'gray70'
      };
      Object.keys(defaults).forEach(function(id) {
        Shiny.setInputValue(id, defaults[id]);
      });
      setTimeout(function() { allSwatchIds.forEach(buildSwatches); }, 300);
    });
    setTimeout(function() { allSwatchIds.forEach(buildSwatches); }, 800);

    // ── Pathway zoom (scroll-to-zoom + drag-to-pan) ──────────────────────────
    (function() {
      var st = { scale:1, tx:0, ty:0, drag:false, sx:0, sy:0, stx:0, sty:0 };
      var iw0 = 1, ih0 = 1;

      function clamp(wrap, tx, ty) {
        var ww = wrap.clientWidth, wh = wrap.clientHeight;
        var iw = iw0 * st.scale,  ih = ih0 * st.scale;
        return {
          tx: Math.max(Math.min(0, ww - iw), Math.min(0, tx)),
          ty: Math.max(Math.min(0, wh - ih), Math.min(0, ty))
        };
      }
      function applyT(img) {
        img.style.transform = 'translate3d('+st.tx+'px,'+st.ty+'px,0) scale('+st.scale+')';
      }
      function doZoom(factor, wrap, img, cx, cy) {
        var rect = wrap.getBoundingClientRect();
        var ox = (cx !== undefined ? cx - rect.left : wrap.clientWidth  / 2);
        var oy = (cy !== undefined ? cy - rect.top  : wrap.clientHeight / 2);
        var ns = Math.max(1, Math.min(12, st.scale * factor));
        var r  = ns / st.scale;
        st.tx  = ox - r * (ox - st.tx);
        st.ty  = oy - r * (oy - st.ty);
        st.scale = ns;
        var c  = clamp(wrap, st.tx, st.ty);
        st.tx = c.tx; st.ty = c.ty;
        applyT(img);
        wrap.classList.toggle('zoomed', st.scale > 1);
        return ns;
      }
      function setupZoom(container) {
        var img = container.querySelector('img');
        if (!img || container._pvZoom) return;
        container._pvZoom = true;
        st.scale = 1; st.tx = 0; st.ty = 0;
        function captureSize() {
          iw0 = img.offsetWidth  || img.clientWidth  || 600;
          ih0 = img.offsetHeight || img.clientHeight || 450;
        }
        if (img.complete && img.naturalWidth) captureSize();
        else img.addEventListener('load', captureSize);
        var ctrl = document.createElement('div');
        ctrl.className = 'pv-zoom-controls';
        ctrl.innerHTML =
          '<button class=\"pv-zoom-btn\" data-z=\"in\">\uFF0B Zoom In</button>' +
          '<span class=\"pv-zoom-level\">100%</span>' +
          '<button class=\"pv-zoom-btn\" data-z=\"out\">\uFF0D Zoom Out</button>' +
          '<button class=\"pv-zoom-btn\" data-z=\"rst\">&#8635; Reset</button>' +
          '<span class=\"pv-zoom-hint\">Scroll to zoom &nbsp;\u00B7&nbsp; Drag to pan</span>';
        container.insertBefore(ctrl, container.firstChild);
        var lbl = ctrl.querySelector('.pv-zoom-level');
        var wrap = document.createElement('div');
        wrap.className = 'pv-zoom-wrap';
        img.parentNode.insertBefore(wrap, img);
        wrap.appendChild(img);
        function updateLbl() { lbl.textContent = Math.round(st.scale * 100) + '%'; }
        ctrl.addEventListener('click', function(e) {
          var btn = e.target.closest('[data-z]');
          if (!btn) return;
          var a = btn.dataset.z;
          if      (a === 'in')  { doZoom(1.35, wrap, img); }
          else if (a === 'out') { doZoom(1/1.35, wrap, img); }
          else if (a === 'rst') { st.scale=1; st.tx=0; st.ty=0; applyT(img); wrap.classList.remove('zoomed'); }
          updateLbl();
        });
        wrap.addEventListener('wheel', function(e) {
          e.preventDefault();
          doZoom(e.deltaY < 0 ? 1.12 : 1/1.12, wrap, img, e.clientX, e.clientY);
          updateLbl();
        }, { passive: false });
        wrap.addEventListener('mousedown', function(e) {
          if (st.scale <= 1) return;
          st.drag = true; st.sx = e.clientX; st.sy = e.clientY;
          st.stx = st.tx; st.sty = st.ty;
          wrap.classList.add('dragging'); e.preventDefault();
        });
        window.addEventListener('mousemove', function(e) {
          if (!st.drag) return;
          var c = clamp(wrap, st.stx + e.clientX - st.sx, st.sty + e.clientY - st.sy);
          st.tx = c.tx; st.ty = c.ty; applyT(img);
        });
        window.addEventListener('mouseup', function() {
          if (!st.drag) return;
          st.drag = false; wrap.classList.remove('dragging');
        });
      }
      $(document).on('shiny:value', function(e) {
        if (e.name === 'pathway_plot_display') {
          setTimeout(function() {
            var c = document.getElementById('pathway_plot_display');
            if (c) { c._pvZoom = false; setupZoom(c); }
          }, 120);
        }
      });
      $(document).on('shiny:connected', function() {
        new MutationObserver(function() {
          var c = document.getElementById('pathway_plot_display');
          if (c && c.querySelector('img') && !c._pvZoom) setupZoom(c);
        }).observe(document.body, { childList: true, subtree: true });
      });
    })();
    ")),

    tabItems(

      # ── Data Input ──────────────────────────────────────────────────────────
      tabItem(tabName = "input_tab",
        fluidRow(column(12,
          tags$h3(style="color:#1a73e8;font-weight:300;margin-bottom:20px;letter-spacing:0.05em;",
            icon("dna", style="margin-right:10px;"), "Gene Expression Data Input")
        )),
        fluidRow(
          column(4,
            box(width=NULL, title="Upload Expression Data",
              tags$p(style="color:#8899bb;font-size:12px;margin-bottom:12px;",
                "Upload a CSV/TSV with gene IDs in the first column and log2 fold-change values in the second."),
              fileInput("expr_file", "Expression File (.csv / .tsv)",
                        accept=c(".csv",".tsv",".txt"), buttonLabel="Browse", placeholder="No file selected"),
              selectInput("file_sep", "Delimiter",
                          choices=c("Comma (,)"=",","Tab (\\t)"="\t","Semicolon (;)"=";"), selected=","),
              checkboxInput("has_header", "File has header row", value=TRUE),
              actionButton(
                "load_uploaded_expr",
                "Load Uploaded Data",
                class = "btn-primary btn-sm",
                icon = icon("upload"),
                width = "100%"
              ),
              tags$hr(),
              tags$div(class="section-divider", "Or use example data"),
              selectInput("example_data", "Load Example Dataset",
                          choices=c("None"="none","Human TCGA BRCA (SYMBOL)"="brca",
                                    "Mouse LPS Macrophage (ENSEMBL)"="lps","Human COVID-19 Lung (ENTREZ)"="covid")),
              actionButton("load_example", "Load Example", class="btn-warning btn-sm",
                           icon=icon("flask"), width="100%")
            ),
            box(width=NULL, title="Column Mapping",
              tags$p(style="color:#8899bb;font-size:12px;","Map your data columns after upload."),
              uiOutput("col_mapping_ui")
            )
          ),
          column(8,
            box(width=NULL, title="Data Preview & Statistics",
              fluidRow(
                column(3, uiOutput("tile_genes")), column(3, uiOutput("tile_up")),
                column(3, uiOutput("tile_down")),  column(3, uiOutput("tile_sig"))
              ),
              tags$br(),
              tabsetPanel(
                tabPanel("Table",        DTOutput("expr_table")),
                tabPanel("Volcano Plot", plotlyOutput("volcano_plot", height="400px")),
                tabPanel("Distribution", plotlyOutput("dist_plot",    height="400px"))
              )
            )
          )
        )
      ),

      # ── Gene ID Converter ───────────────────────────────────────────────────
      tabItem(tabName = "convert_tab",
        fluidRow(column(12,
          tags$h3(style="color:#4fc3f7;font-weight:300;margin-bottom:20px;letter-spacing:0.05em;",
            icon("exchange-alt",style="margin-right:10px;"), "Gene ID Conversion Tool")
        )),
        fluidRow(
          column(4,
            box(width=NULL, title="Conversion Settings",
              tags$p(style="color:#8899bb;font-size:12px;",
                "Convert gene IDs to ENTREZ (required by PathView). Uses Bioconductor annotation packages."),
              selectInput("organism", "Organism",
                          choices=c("Human (Homo sapiens)"="hsa","Mouse (Mus musculus)"="mmu",
                                    "Rat (Rattus norvegicus)"="rno","Zebrafish (Danio rerio)"="dre",
                                    "Fly (Drosophila melanog.)"="dme","Worm (C. elegans)"="cel",
                                    "Yeast (S. cerevisiae)"="sce"), selected="hsa"),
              selectInput("id_from", "Input ID Type",
                          choices=c("Gene Symbol"="SYMBOL","Ensembl Gene ID"="ENSEMBL",
                                    "Ensembl Transcript"="ENSEMBLTRANS","RefSeq mRNA"="REFSEQ",
                                    "UniProt"="UNIPROT","Entrez Gene ID"="ENTREZID",
                                    "Probe ID (Affy)"="PROBEID"), selected="SYMBOL"),
              selectInput("id_to", "Output ID Type",
                          choices=c("Entrez Gene ID (for PathView)"="ENTREZID","Gene Symbol"="SYMBOL",
                                    "Ensembl Gene ID"="ENSEMBL","UniProt"="UNIPROT"), selected="ENTREZID"),
              tags$hr(),
              tags$div(class="section-divider", "Duplicate handling"),
              radioButtons("dup_method", NULL,
                           choices=c("Keep first match"="first","Keep all (1-to-many)"="all",
                                     "Keep highest |FC|"="maxfc"), selected="first"),
              tags$hr(),
              actionButton("run_conversion", "Run Conversion",
                           class="btn-primary", icon=icon("play"), width="100%"),
              tags$br(), tags$br(),
              uiOutput("pkg_install_ui")
            )
          ),
          column(8,
            box(width=NULL, title="Conversion Results",
              uiOutput("conversion_summary"),
              tags$br(),
              tabsetPanel(
                tabPanel("Converted IDs",  DTOutput("converted_table")),
                tabPanel("Unmapped Genes", DTOutput("unmapped_table")),
                tabPanel("ID Map",         DTOutput("id_map_table"))
              ),
              tags$br(),
              fluidRow(
                column(6, actionButton("use_converted", "Use Converted Data for PathView",
                                       class="btn-success", icon=icon("arrow-right"), width="100%")),
                column(6, downloadButton("dl_converted", "Download Converted Table",
                                         class="btn-sm", style="width:100%"))
              )
            )
          )
        )
      ),

      # ── PathView Plots ──────────────────────────────────────────────────────
      tabItem(tabName = "pathview_tab",
        fluidRow(column(12,
          tags$h3(style="color:#4fc3f7;font-weight:300;margin-bottom:20px;letter-spacing:0.05em;",
            icon("project-diagram",style="margin-right:10px;"), "PathView Pathway Visualization")
        )),
        fluidRow(
          column(3,
            box(width=NULL, title="Pathway Selection",
              selectInput("organism_pv", "Organism (KEGG code)",
                          choices=c("hsa  -  Human"="hsa","mmu  -  Mouse"="mmu","rno  -  Rat"="rno",
                                    "dre  -  Zebrafish"="dre","dme  -  Fly"="dme",
                                    "cel  -  Worm"="cel","sce  -  Yeast"="sce"), selected="hsa"),
              tags$div(class="section-divider", "Pathway IDs"),
              tags$p(style="color:#8899bb;font-size:12px;",
                "Enter KEGG pathway IDs (numeric part only). One per line, or comma-separated."),
              textAreaInput("pathway_ids", NULL,
                            value="04110\n04115\n04210\n04310\n04630",
                            rows=6, placeholder="04110\n04210\n04310"),
              tags$hr(),
              actionButton("search_pathways", "Search KEGG Pathways",
                           class="btn-sm btn-default", icon=icon("search"), width="100%"),
              uiOutput("pathway_search_ui"),
              tags$hr(),
              tags$div(class="section-divider", "Plot Options"),
              selectInput("kegg_native", "Plot Style",
                          choices=c("KEGG Native (PNG overlay)"="TRUE",
                                    "ggraph / Graphviz"="FALSE"), selected="TRUE"),
              fluidRow(
                column(8,
                       sliderInput("fc_limit", "Fold-change cutoff",
                                   min = 0, max = 5, value = 1, step = 0.01)
                ),
                column(4,
                       numericInput("fc_limit_num", "Exact value",
                                    value = 1, min = 0, max = 5, step = 0.01)
                )
              ),
              checkboxInput("both_dirs", "Show both directions (up & down)", value=TRUE),
              tags$div(class="section-divider", "Color Scale"),
              # PathView low/mid/high hidden inputs
              tags$input(type="hidden", id="low_col",  value="blue"),
              tags$input(type="hidden", id="mid_col",  value="gray"),
              tags$input(type="hidden", id="high_col", value="red"),
              static_swatch("low_col",  "Low Color",  "blue", "#0000FF"),
              static_swatch("mid_col",  "Mid Color",  "gray", "#808080"),
              static_swatch("high_col", "High Color", "red",  "#FF0000"),
              tags$hr(),
              actionButton("run_pathview", "Generate PathView Plots",
                           class="btn-primary", icon=icon("play"), width="100%",
                           style="font-size:14px;padding:10px;")
            )
          ),
          column(6,
            box(width=NULL, title="Pathway Plot Viewer",
              uiOutput("pathview_status"),
              tags$br(),
              uiOutput("pathway_selector_ui"),
              tags$br(),
              uiOutput("pathway_plot_display"),
              tags$br(),
              fluidRow(
                column(6, downloadButton("dl_current_plot", "Download Current Plot",
                                         class="btn-sm", style="width:100%")),
                column(6, downloadButton("dl_all_plots", "Download All Plots (ZIP)",
                                         class="btn-sm", style="width:100%"))
              )
            )
          ),
          column(3,
            box(width=NULL, title="Genes in Pathway",
              tags$p(style="color:#718096;font-size:11px;margin-bottom:8px;",
                "Genes from your expression data found in the selected pathway, coloured by direction."),
              uiOutput("pv_gene_summary_ui"),
              tags$hr(style="border-color:#e0e8f5;"),
              tags$div(class="section-divider", "Up-regulated"),
              DTOutput("pv_up_table"),
              tags$br(),
              tags$div(class="section-divider", "Down-regulated"),
              DTOutput("pv_dn_table"),
              tags$br(),
              downloadButton("dl_pv_genes", "Download Gene List", class="btn-sm", style="width:100%;")
            )
          )
        )
      ),

      # ── Results & Export ────────────────────────────────────────────────────
      tabItem(tabName = "results_tab",
        fluidRow(column(12,
          tags$h3(style="color:#4fc3f7;font-weight:300;margin-bottom:20px;letter-spacing:0.05em;",
            icon("download",style="margin-right:10px;"), "Results & Export")
        )),
        fluidRow(
          column(6,
            box(width=NULL, title="Export Options",
              tags$p(style="color:#8899bb;font-size:12px;","Download processed data and plots."),
              tags$hr(),
              tags$div(class="section-divider", "Data Tables"),
              downloadButton("dl_expr_processed","Processed Expression Data (.csv)", style="width:100%;margin-bottom:8px;"),
              downloadButton("dl_conversion_map","Full ID Conversion Map (.csv)",    style="width:100%;margin-bottom:8px;"),
              tags$hr(),
              tags$div(class="section-divider", "Plots"),
              downloadButton("dl_all_zip","All PathView Plots (.zip)", class="btn-success", style="width:100%;margin-bottom:8px;"),
              tags$hr(),
              tags$div(class="section-divider", "R Session Info"),
              actionButton("show_session","Show Session Info", class="btn-sm btn-default", icon=icon("info"), width="100%")
            )
          ),
          column(6,
            box(width=NULL, title="Analysis Summary", uiOutput("results_summary"))
          )
        ),
        fluidRow(column(12,
          box(width=NULL, title="Session / Package Info", verbatimTextOutput("session_info_out"))
        ))
      ),

      # ── Help ────────────────────────────────────────────────────────────────
      tabItem(tabName = "help_tab",
        fluidRow(column(12,
          tags$h3(style="color:#4fc3f7;font-weight:300;margin-bottom:20px;letter-spacing:0.05em;",
            icon("question-circle",style="margin-right:10px;"), "Help & Documentation")
        )),
        fluidRow(
          column(6,
            box(width=NULL, title="Quick Start (PathView / DESeq2)",
              tags$ol(style="color:#4a5568;font-size:13px;line-height:2;padding-left:20px;",
                tags$li("Upload your gene expression CSV in the ", tags$b("Data Input"), " tab."),
                tags$li("Go to ", tags$b("Gene ID Converter"), " and run conversion to Entrez IDs."),
                tags$li("Click ", tags$b("Use Converted Data for PathView"), "."),
                tags$li("In ", tags$b("PathView Plots"), ", enter KEGG pathway IDs and click Generate."),
                tags$li("Download results in ", tags$b("Results & Export"), ".")
              ),
              tags$hr(style="border-color:#e0e8f5;"),
              tags$div(class="section-divider", "DEP Workflow"),
              tags$ol(style="color:#4a5568;font-size:13px;line-height:2;padding-left:20px;",
                tags$li("Go to ", tags$b("Run DEP"), " and select Demo or upload LFQ matrix + design."),
                tags$li("Choose normalisation/imputation, click ", tags$b("Run DEP Analysis"), "."),
                tags$li("Explore ", tags$b("Protein Volcano"), ", ", tags$b("Protein Heatmap"), ", ", tags$b("DEP Results"), "."),
                tags$li("Click ", tags$b("Use DEP Results in PathView"), " then convert IDs and run PathView.")
              )
            )
          ),
          column(6,
            box(width=NULL, title="Required R Packages",
              tags$pre(style="background:#f8faff;color:#2d3748;font-size:11px;border:1px solid #e0e8f5;padding:10px;border-radius:6px;",
'# CRAN
install.packages(c("shiny","shinydashboard","shinyWidgets",
                   "DT","ggplot2","plotly","zip"))
# Bioconductor
BiocManager::install(c(
  "DESeq2","limma","SummarizedExperiment",
  "pathview","AnnotationDbi",
  "org.Hs.eg.db","org.Mm.eg.db","org.Rn.eg.db","KEGGREST"
))
# Optional
BiocManager::install(c("vsn","impute","matrixStats","ggrepel","tidyr","tibble"))'
              )
            )
          )
        ),
        fluidRow(
          column(6,
            box(width=NULL, title="DESeq2 Count Matrix Format",
              tags$p(style="color:#718096;font-size:12px;",
                "First column = gene IDs; remaining columns = one integer count column per sample."),
              tags$div(style="overflow-x:auto;",
                tags$table(style="width:100%;border-collapse:collapse;font-size:12px;font-family:'DM Mono',monospace;",
                  tags$thead(tags$tr(style="background:#e8f0fe;",
                    tags$th(style="padding:6px 10px;color:#1a73e8;border:1px solid #c9d8f0;","gene"),
                    tags$th(style="padding:6px 10px;color:#1a73e8;border:1px solid #c9d8f0;","Ctrl_1"),
                    tags$th(style="padding:6px 10px;color:#1a73e8;border:1px solid #c9d8f0;","Ctrl_2"),
                    tags$th(style="padding:6px 10px;color:#1a73e8;border:1px solid #c9d8f0;","Trt_1")
                  )),
                  tags$tbody(
                    tags$tr(tags$td(style="padding:5px 10px;border:1px solid #e0e8f5;","TP53"),
                            tags$td(style="padding:5px 10px;border:1px solid #e0e8f5;text-align:right;","1240"),
                            tags$td(style="padding:5px 10px;border:1px solid #e0e8f5;text-align:right;","1183"),
                            tags$td(style="padding:5px 10px;border:1px solid #e0e8f5;text-align:right;","4502")),
                    tags$tr(style="background:#f8faff;",
                            tags$td(style="padding:5px 10px;border:1px solid #e0e8f5;","BRCA1"),
                            tags$td(style="padding:5px 10px;border:1px solid #e0e8f5;text-align:right;","892"),
                            tags$td(style="padding:5px 10px;border:1px solid #e0e8f5;text-align:right;","914"),
                            tags$td(style="padding:5px 10px;border:1px solid #e0e8f5;text-align:right;","312"))
                  )
                )
              )
            )
          ),
          column(6,
            box(width=NULL, title="DEP LFQ Matrix Format",
              tags$p(style="color:#718096;font-size:12px;",
                "Rows = proteins; cols = log2 LFQ intensities. Include ", tags$code("name"), " and ", tags$code("ID"), " columns; NAs allowed."),
              tags$div(style="overflow-x:auto;",
                tags$table(style="width:100%;border-collapse:collapse;font-size:12px;font-family:'DM Mono',monospace;",
                  tags$thead(tags$tr(style="background:#e8f0fe;",
                    tags$th(style="padding:6px 8px;color:#1a73e8;border:1px solid #c9d8f0;","name"),
                    tags$th(style="padding:6px 8px;color:#1a73e8;border:1px solid #c9d8f0;","ID"),
                    tags$th(style="padding:6px 8px;color:#1a73e8;border:1px solid #c9d8f0;","Ctrl_1"),
                    tags$th(style="padding:6px 8px;color:#1a73e8;border:1px solid #c9d8f0;","Trt_1")
                  )),
                  tags$tbody(
                    tags$tr(tags$td(style="padding:5px 8px;border:1px solid #e0e8f5;","Prot_001"),
                            tags$td(style="padding:5px 8px;border:1px solid #e0e8f5;","ACTB"),
                            tags$td(style="padding:5px 8px;border:1px solid #e0e8f5;text-align:right;","28.1"),
                            tags$td(style="padding:5px 8px;border:1px solid #e0e8f5;text-align:right;","30.4")),
                    tags$tr(style="background:#f8faff;",
                            tags$td(style="padding:5px 8px;border:1px solid #e0e8f5;","Prot_002"),
                            tags$td(style="padding:5px 8px;border:1px solid #e0e8f5;","TP53"),
                            tags$td(style="padding:5px 8px;border:1px solid #e0e8f5;text-align:right;","24.3"),
                            tags$td(style="padding:5px 8px;border:1px solid #e0e8f5;color:#a0aec0;font-style:italic;text-align:right;","NA"))
                  )
                )
              )
            )
          )
        )
      ),

      # =======================================================================
      # DGE ANALYSIS TABS
      # =======================================================================

      # ── Run DESeq2 ──────────────────────────────────────────────────────────
      tabItem(tabName = "deseq2_tab",
        fluidRow(column(12,
          tags$h3(style="color:#1a73e8;font-weight:300;margin-bottom:20px;",
            icon("flask",style="margin-right:10px;"), "Differential Gene Expression  -  DESeq2")
        )),
        fluidRow(
          column(4,
            box(width=NULL, title="Data Source",
              tags$p(style="color:#718096;font-size:12px;",
                "Use the built-in demo data or upload your own count matrix and metadata."),
              radioButtons("dge_source", NULL,
                choices=c("Demo data (2,000 genes x 6 samples)"="demo",
                          "Upload count matrix + metadata"="upload"), selected="demo"),
              conditionalPanel("input.dge_source == 'upload'",
                fileInput("dge_count_file", "Count Matrix",
                  accept=c(".csv",".tsv",".txt",".tab"), placeholder="CSV, TSV, or TXT file"),
                selectInput("dge_count_sep", "Count Matrix Delimiter",
                  choices=c("Comma (CSV)"=",","Tab (TSV/TXT)"="\t","Semicolon"=";","Space"=" "), selected=","),
                checkboxInput("dge_count_header", "File has header row", value=TRUE),
                tags$hr(style="border-color:#e0e8f5;margin:6px 0;"),
                fileInput("dge_meta_file", "Sample Metadata",
                  accept=c(".csv",".tsv",".txt",".tab"), placeholder="CSV, TSV, or TXT file"),
                selectInput("dge_meta_sep", "Metadata Delimiter",
                  choices=c("Comma (CSV)"=",","Tab (TSV/TXT)"="\t","Semicolon"=";","Space"=" "), selected=","),
                uiOutput("dge_meta_col_ui"),
                tags$div(class="alert alert-info", style="margin-top:6px;padding:8px;font-size:11px;",
                  tags$b("Metadata format:"), " any column order is fine — map sample and condition columns below after upload.",
                  tags$br(),
                  tags$b("Example:"), tags$code("sample_name,group\nS1,Control\nS2,Treatment"))
              )
            ),
            box(width=NULL, title="DESeq2 Parameters",
              sliderInput("dge_padj",  "Adj. P-value Cutoff", min=0.001, max=0.1, value=0.05, step=0.001),
              sliderInput("dge_lfc",   "|Log2FC| Cutoff",     min=0.5,   max=3,   value=1,    step=0.1),
              sliderInput("dge_minct", "Min Count Filter",    min=1,     max=20,  value=10,   step=1),
              tags$hr(),
              actionButton("dge_run", "Run DESeq2 Analysis",
                class="btn-primary", width="100%", style="font-size:14px;padding:10px;font-weight:600;"),
              tags$br(), tags$br(),
              tags$div(id="dge_status_div",
                tags$div(class="alert alert-info", style="font-size:12px;padding:8px;",
                  icon("info-circle"), " Ready  -  click Run DESeq2 Analysis.")
              ),
              uiOutput("dge_status_ui")
            ),
            box(width=NULL, title="Send to PathView",
              tags$p(style="color:#718096;font-size:12px;",
                "After DESeq2 completes, push the log2FC results into the PathView pipeline."),
              actionButton("dge_to_pathview", "Use DESeq2 Results in PathView",
                class="btn-success", width="100%", style="font-weight:600;"),
              tags$br(), tags$br(),
              uiOutput("dge_pathview_msg")
            )
          ),
          column(8,
            fluidRow(
              column(3, uiOutput("dge_tile_total")), column(3, uiOutput("dge_tile_de")),
              column(3, uiOutput("dge_tile_up")),    column(3, uiOutput("dge_tile_dn"))
            ),
            tags$br(),
            box(width=NULL, title="Results Preview (top genes by adjusted p-value)",
              DTOutput("dge_preview_table"))
          )
        )
      ),

      # ── PCA ─────────────────────────────────────────────────────────────────
      tabItem(tabName = "pca_tab",
        fluidRow(column(12,
          tags$h3(style="color:#1a73e8;font-weight:300;margin-bottom:20px;",
            icon("circle-dot",style="margin-right:10px;"), "Principal Component Analysis")
        )),
        fluidRow(
          column(3,
            box(width=NULL, title="PCA Controls",
              selectInput("pca_pc_x", "X Axis", choices=paste0("PC",1:5), selected="PC1"),
              selectInput("pca_pc_y", "Y Axis", choices=paste0("PC",1:5), selected="PC2"),
              checkboxInput("pca_show_labels", "Show sample labels", value=TRUE),
              sliderInput("pca_top_n", "Top variable genes", min=100, max=2000, value=500, step=100),
              tags$hr(style="border-color:#e0e8f5;"),
              tags$div(class="section-divider", style="margin:6px 0 4px;", "Group Colors"),
              static_swatch("dge_grp1_col", "Group 1", "royalblue", "#4169E1"),
              static_swatch("dge_grp2_col", "Group 2", "tomato",    "#FF6347")
            )
          ),
          column(9,
            box(width=NULL, title="PCA Plot", plotlyOutput("dge_pca_plot", height="500px"))
          )
        )
      ),

      # ── Volcano ─────────────────────────────────────────────────────────────
      tabItem(tabName = "volc_tab",
        fluidRow(column(12,
          tags$h3(style="color:#1a73e8;font-weight:300;margin-bottom:20px;",
            icon("chart-line",style="margin-right:10px;"), "Volcano Plot")
        )),
        fluidRow(
          column(3,
            box(width=NULL, title="Volcano Controls",
              uiOutput("volc_contrast_ui"),
              tags$hr(style="border-color:#e0e8f5;"),
              sliderInput("volc_padj",  "Adj. P Cutoff",      min=0.001, max=0.1, value=0.05, step=0.001),
              sliderInput("volc_lfc",   "|Log2FC| Cutoff",    min=0.5,   max=3,   value=1,    step=0.1),
              sliderInput("volc_nlabs", "Top genes labelled", min=0,     max=30,  value=10,   step=1),
              checkboxInput("volc_interactive", "Interactive (Plotly)", value=TRUE),
              tags$hr(style="border-color:#e0e8f5;"),
              tags$div(class="section-divider", style="margin:6px 0 4px;", "Point Colors"),
              static_swatch("dge_up_col", "Up-regulated",    "tomato",    "#FF6347"),
              static_swatch("dge_dn_col", "Down-regulated",  "turquoise", "#40E0D0"),
              static_swatch("dge_ns_col", "Not significant", "gray70",    "#B3B3B3")
            )
          ),
          column(9,
            box(width=NULL, title="Volcano Plot", uiOutput("dge_volcano_ui"))
          )
        )
      ),

      # ── Distribution ────────────────────────────────────────────────────────
      tabItem(tabName = "dist_dge_tab",
        fluidRow(column(12,
          tags$h3(style="color:#1a73e8;font-weight:300;margin-bottom:20px;",
            icon("chart-area",style="margin-right:10px;"), "Distribution Analysis")
        )),
        fluidRow(
          column(3,
            box(width=NULL, title="Plot Controls",
              uiOutput("dist_contrast_ui"),
              tags$hr(style="border-color:#e0e8f5;"),
              selectInput("dge_dist_type", "Plot type",
                choices=c("Normalised counts (violin)"="violin","Log2FC distribution"="lfc_hist",
                          "P-value histogram"="pval_hist","MA plot"="ma","Dispersion plot"="disp"),
                selected="violin"),
              checkboxInput("dge_dist_log", "Log10 scale (violin)", value=TRUE),
              tags$hr(style="border-color:#e0e8f5;"),
              tags$div(class="section-divider", style="margin:6px 0 4px;", "Group Colors (violin)"),
              static_swatch("dge_grp1_col", "Group 1", "royalblue", "#4169E1"),
              static_swatch("dge_grp2_col", "Group 2", "tomato",    "#FF6347"),
              tags$div(class="section-divider", style="margin:8px 0 4px;", "DE Colors (LFC hist / MA)"),
              static_swatch("dge_up_col", "Up-regulated",    "tomato",    "#FF6347"),
              static_swatch("dge_dn_col", "Down-regulated",  "turquoise", "#40E0D0"),
              static_swatch("dge_ns_col", "Not significant", "gray70",    "#B3B3B3")
            )
          ),
          column(9,
            box(width=NULL, title="Distribution Plot", plotlyOutput("dge_dist_plot", height="500px"))
          )
        )
      ),

      # ── Heatmap ─────────────────────────────────────────────────────────────
      tabItem(tabName = "heatmap_tab",
        fluidRow(column(12,
          tags$h3(style="color:#1a73e8;font-weight:300;margin-bottom:20px;",
            icon("th",style="margin-right:10px;"), "Heatmap  -  Top Differentially Expressed Genes")
        )),
        fluidRow(
          column(3,
            box(width=NULL, title="Heatmap Controls",
              uiOutput("hm_contrast_ui"),
              tags$hr(style="border-color:#e0e8f5;"),
              sliderInput("hm_top_n", "Top DE genes to show", min=10, max=200, value=50, step=5),
              selectInput("hm_order_by", "Order genes by",
                choices=c("Adjusted p-value"="padj","|Log2FC| descending"="lfc",
                          "Hierarchical clustering"="hclust"), selected="padj"),
              checkboxInput("hm_scale_rows", "Scale rows (z-score)", value=TRUE),
              checkboxInput("hm_show_labels", "Show gene labels", value=TRUE),
              tags$div(class="section-divider", style="margin:8px 0 6px;", "Color Palette"),
              selectInput("hm_palette", "Palette",
                choices=c("RdBu (diverging)"="RdBu","RdYlBu"="RdYlBu","PRGn"="PRGn",
                          "PiYG"="PiYG","Viridis"="viridis","Plasma"="plasma"), selected="RdBu"),
              checkboxInput("hm_rev_pal", "Reverse palette", value=TRUE),
              tags$hr(style="border-color:#e0e8f5;"),
              downloadButton("dl_heatmap", "Download Heatmap (PNG)", style="width:100%;")
            )
          ),
          column(9,
            box(width=NULL, title="Heatmap", plotOutput("dge_heatmap", height="600px"))
          )
        )
      ),

      # ── DGE Results Table ───────────────────────────────────────────────────
      tabItem(tabName = "dge_res_tab",
        fluidRow(column(12,
          tags$h3(style="color:#1a73e8;font-weight:300;margin-bottom:20px;",
            icon("table",style="margin-right:10px;"), "DESeq2 Results Table")
        )),
        fluidRow(column(12,
          box(width=NULL, title="Filter & Download",
            fluidRow(
              column(4, uiOutput("tbl_contrast_ui")),
              column(4, selectInput("dge_tbl_filter", "Show",
                choices=c("All genes"="all","Significant only"="sig",
                          "Upregulated"="up","Downregulated"="dn"), selected="all")),
              column(4, downloadButton("dge_dl_csv", "Download CSV", style="margin-top:25px;width:100%;"))
            ),
            DTOutput("dge_full_table")
          )
        ))
      ),

      # =======================================================================
      # DEP (DIFFERENTIAL PROTEIN EXPRESSION) TABS
      # =======================================================================

      # ── Run DEP ─────────────────────────────────────────────────────────────
      tabItem(tabName = "dep_tab",
        fluidRow(column(12,
          tags$h3(style="color:#1a73e8;font-weight:300;margin-bottom:20px;",
            icon("atom",style="margin-right:10px;"),
            "Differential Protein Expression  -  DEP (limma-based)")
        )),
        fluidRow(
          column(4,
            box(width=NULL, title="Data Source",
              tags$p(style="color:#718096;font-size:12px;",
                "Use the built-in demo data or upload your own LFQ protein intensity matrix and experimental design."),
              radioButtons("dep_source", NULL,
                choices=c("Demo data (500 proteins x 6 samples)"="demo",
                          "Upload LFQ matrix + metadata"="upload"), selected="demo"),
              conditionalPanel("input.dep_source == 'upload'",
                fileInput("dep_prot_file", "LFQ Protein Intensity Matrix",
                  accept=c(".csv",".tsv",".txt",".tab"),
                  placeholder="CSV/TSV — proteins in rows, samples in columns"),
                selectInput("dep_id_col", "Protein ID column", choices = NULL),
                selectInput("dep_name_col", "Protein name column (optional)", choices = NULL),
                DT::dataTableOutput("data_preview"),   # 👈 ADD THIS LINE HERE
                selectInput("dep_prot_sep", "Matrix Delimiter",
                  choices=c("Comma (CSV)"=",","Tab (TSV/TXT)"="\t","Semicolon"=";","Space"=" "), selected=","),
                tags$hr(style="border-color:#e0e8f5;margin:6px 0;"),
                fileInput("dep_meta_file", "Experimental Design (metadata)",
                  accept=c(".csv",".tsv",".txt",".tab"),
                  placeholder="CSV/TSV — label, condition, replicate"),
                selectInput("dep_meta_sep", "Metadata Delimiter",
                  choices=c("Comma (CSV)"=",","Tab (TSV/TXT)"="\t","Semicolon"=";","Space"=" "), selected=","),
                uiOutput("dep_meta_col_ui"),
                tags$div(class="alert alert-info", style="margin-top:6px;padding:8px;font-size:11px;",
                  tags$b("Matrix:"), " rows = proteins, cols = LFQ intensity samples.",
                  tags$br(),
                  tags$b("Design:"), " columns ",
                  tags$code("label"), ", ", tags$code("condition"), ", ", tags$code("replicate"), ".")
              )
            ),
            box(width=NULL, title="DEP Parameters",
              selectInput("dep_norm", "Normalisation",
                choices=c("VSN (variance-stabilising)"="vsn","Quantile"="quantile","None"="none"), selected="vsn"),
              selectInput("dep_impute", "Imputation Method",
                choices=c("MinProb (left-censored)"="MinProb","QRILC"="QRILC","kNN"="knn","None (skip)"="none"),
                selected="MinProb"),
              sliderInput("dep_padj", "Adj. P-value Cutoff", min=0.001, max=0.1, value=0.05, step=0.001),
              sliderInput("dep_lfc",  "|Log2FC| Cutoff",     min=0.1,   max=3,   value=1,    step=0.1),
              tags$hr(),
              actionButton("dep_run", "Run DEP Analysis",
                class="btn-primary", width="100%", style="font-size:14px;padding:10px;font-weight:600;"),
              tags$br(), tags$br(),
              tags$div(id="dep_status_div",
                tags$div(class="alert alert-info", style="font-size:12px;padding:8px;",
                  icon("info-circle"), " Ready  -  click Run DEP Analysis.")
              ),
              uiOutput("dep_status_ui")
            ),
            box(width=NULL, title="Send to PathView",
              tags$p(style="color:#718096;font-size:12px;",
                "Export DEP results as log2FC into the PathView pipeline."),
              uiOutput("dep_contrast_send_ui"),
              tags$br(),
              actionButton("dep_to_pathview", "Use DEP Results in PathView",
                class="btn-success", width="100%", style="font-weight:600;"),
              tags$br(), tags$br(),
              uiOutput("dep_pathview_msg")
            )
          ),
          column(8,
            fluidRow(
              column(3, uiOutput("dep_tile_total")), column(3, uiOutput("dep_tile_sig")),
              column(3, uiOutput("dep_tile_up")),    column(3, uiOutput("dep_tile_dn"))
            ),
            tags$br(),
            box(width=NULL, title="Results Preview (top proteins by adjusted p-value)",
              uiOutput("dep_contrast_preview_ui"), tags$br(), DTOutput("dep_preview_table")),
            box(width=NULL, title="Quality Control: Missing Values per Sample",
              plotOutput("dep_qc_plot", height="280px"))
          )
        )
      ),

      # ── Protein Volcano ─────────────────────────────────────────────────────
      tabItem(tabName = "dep_volc_tab",
        fluidRow(column(12,
          tags$h3(style="color:#1a73e8;font-weight:300;margin-bottom:20px;",
            icon("chart-line",style="margin-right:10px;"), "Protein Volcano Plot")
        )),
        fluidRow(
          column(3,
            box(width=NULL, title="Volcano Controls",
              uiOutput("dep_volc_contrast_ui"),
              tags$hr(style="border-color:#e0e8f5;"),
              sliderInput("dep_volc_padj", "Adj. P Cutoff",       min=0.001, max=0.1, value=0.05, step=0.001),
              sliderInput("dep_volc_lfc",  "|Log2FC| Cutoff",     min=0.1,   max=3,   value=1,    step=0.1),
              sliderInput("dep_volc_nlabs","Top proteins labelled",min=0,     max=30,  value=10,   step=1),
              checkboxInput("dep_volc_interactive", "Interactive (Plotly)", value=TRUE),
              tags$hr(style="border-color:#e0e8f5;"),
              tags$div(class="section-divider", style="margin:6px 0 4px;", "Point Colors"),
              static_swatch("dep_up_col", "Up-regulated",    "tomato",    "#FF6347"),
              static_swatch("dep_dn_col", "Down-regulated",  "steelblue", "#4682B4"),
              static_swatch("dep_ns_col", "Not significant", "gray70",    "#B3B3B3")
            )
          ),
          column(9,
            box(width=NULL, title="Protein Volcano Plot", uiOutput("dep_volcano_ui"))
          )
        )
      ),

      # ── Protein Heatmap ─────────────────────────────────────────────────────
      tabItem(tabName = "dep_hm_tab",
        fluidRow(column(12,
          tags$h3(style="color:#1a73e8;font-weight:300;margin-bottom:20px;",
            icon("th",style="margin-right:10px;"),
            "Protein Heatmap  -  Top Differentially Expressed Proteins")
        )),
        fluidRow(
          column(3,
            box(width=NULL, title="Heatmap Controls",
              uiOutput("dep_hm_contrast_ui"),
              tags$hr(style="border-color:#e0e8f5;"),
              sliderInput("dep_hm_top_n", "Top DE proteins to show", min=10, max=100, value=30, step=5),
              selectInput("dep_hm_order_by", "Order proteins by",
                choices=c("Adjusted p-value"="padj","|Log2FC| descending"="lfc",
                          "Hierarchical clustering"="hclust"), selected="padj"),
              checkboxInput("dep_hm_scale_rows", "Scale rows (z-score)", value=TRUE),
              checkboxInput("dep_hm_show_labels", "Show protein labels", value=TRUE),
              tags$div(class="section-divider", style="margin:8px 0 6px;", "Color Palette"),
              selectInput("dep_hm_palette", "Palette",
                choices=c("RdBu (diverging)"="RdBu","RdYlBu"="RdYlBu","PRGn"="PRGn",
                          "PiYG"="PiYG","Viridis"="viridis","Plasma"="plasma"), selected="RdBu"),
              checkboxInput("dep_hm_rev_pal", "Reverse palette", value=TRUE),
              tags$hr(style="border-color:#e0e8f5;"),
              downloadButton("dep_dl_heatmap", "Download Heatmap (PNG)", style="width:100%;")
            )
          ),
          column(9,
            box(width=NULL, title="Protein Heatmap", plotOutput("dep_heatmap", height="600px"))
          )
        )
      ),

      # ── DEP Results Table ───────────────────────────────────────────────────
      tabItem(tabName = "dep_res_tab",
        fluidRow(column(12,
          tags$h3(style="color:#1a73e8;font-weight:300;margin-bottom:20px;",
            icon("table",style="margin-right:10px;"), "DEP Results Table")
        )),
        fluidRow(column(12,
          box(width=NULL, title="Filter & Download",
            fluidRow(
              column(4, uiOutput("dep_tbl_contrast_ui")),
              column(4, selectInput("dep_tbl_filter", "Show",
                choices=c("All proteins"="all","Significant only"="sig",
                          "Upregulated"="up","Downregulated"="dn"), selected="all")),
              column(4, downloadButton("dep_dl_csv", "Download CSV", style="margin-top:25px;width:100%;"))
            ),
            DTOutput("dep_full_table")
          )
        ))
      )

    ) # end tabItems
  )   # end dashboardBody
)     # end dashboardPage


# -- SERVER ------------------------------------------------------------------
# ---------------- CLEANING ----------------
clean_proteomics <- function(df) {
  df[] <- lapply(df, function(x) as.numeric(as.character(x)))
  
  df <- df[rowSums(is.na(df)) < ncol(df), ]
  df <- df[rowSums(!is.na(df)) >= 3, ]
  df <- df[apply(df, 1, var, na.rm = TRUE) > 0, ]
  
  df <- log2(df)
  return(df)
}

# ---------------- DEP ----------------
make_dep <- function(mat, meta) {
  library(DEP)
  library(SummarizedExperiment)
  
  se <- SummarizedExperiment(
    assays = list(counts = as.matrix(mat)),
    colData = meta
  )
  
  se <- normalize_vsn(se)
  se <- impute(se, fun = "MinProb", q = 0.01)
  
  return(se)
}

run_dep <- function(se, control) {
  se <- test_diff(se, type = "control", control = control)
  se <- add_rejections(se, alpha = 0.05, lfc = log2(1.5))
  res <- get_results(se)
  return(list(se = se, res = res))
}

# ---------------- ID CONVERSION ----------------
convert_to_entrez <- function(ids, from_type = "SYMBOL") {
  library(org.Hs.eg.db)
  library(AnnotationDbi)
  
  mapIds(
    org.Hs.eg.db,
    keys = ids,
    column = "ENTREZID",
    keytype = from_type,
    multiVals = "first"
  )
}

# ---------------- PATHVIEW PREP ----------------
prepare_gene_data <- function(res) {
  gene_data <- res$log2FoldChange
  names(gene_data) <- res$ENTREZID
  gene_data <- gene_data[!is.na(names(gene_data))]
  gene_data <- tapply(gene_data, names(gene_data), mean)
  return(gene_data)
}

# ---------------- GENE SETS ----------------
get_gene_sets <- function(res) {
  list(
    up = res$ENTREZID[res$log2FoldChange > 1 & res$padj < 0.05],
    down = res$ENTREZID[res$log2FoldChange < -1 & res$padj < 0.05]
  )
}
server <- function(input, output, session) {

  # -- Pre-load pathview internal objects ------------------------------------
  local({
    if (!requireNamespace("pathview", quietly=TRUE)) return()
    safe_assign <- function(name) {
      if (exists(name, envir=globalenv(), inherits=FALSE)) return()
      val <- tryCatch(utils::getFromNamespace(name, "pathview"), error=function(e) NULL)
      if (is.null(val) && name == "bods") {
        val <- data.frame(
          species   = c("Human","Mouse","Rat","Zebrafish","Fly","Worm","Yeast"),
          kegg.code = c("hsa","mmu","rno","dre","dme","cel","sce"),
          pkg.name  = c("org.Hs.eg.db","org.Mm.eg.db","org.Rn.eg.db",
                        "org.Dr.eg.db","org.Dm.eg.db","org.Ce.eg.db","org.Sc.sgd.db"),
          stringsAsFactors = FALSE)
      }
      if (!is.null(val)) {
        assign(name, val, envir=globalenv())
        tryCatch({
          ns <- asNamespace("pathview"); unlockBinding(name, ns); assign(name, val, envir=ns)
        }, error=function(e) NULL)
      }
    }
    safe_assign("bods"); safe_assign("KEGGEdgeSubtype"); safe_assign("rn.list")
  })

  # -- Reactive values -------------------------------------------------------
  rv <- reactiveValues(
    expr_data=NULL, gene_col=NULL, fc_col=NULL,
    converted_data=NULL, id_map=NULL, unmapped=NULL,
    pathview_imgs=list(), pathway_genes=list(),
    converted_id_type=NULL, session_info=NULL, conversion_done=FALSE
  )
  # ---- FC slider ↔ numeric sync ----
  
  observeEvent(input$fc_limit, {
    updateNumericInput(session, "fc_limit_num", value = input$fc_limit)
  })
  
  observeEvent(input$fc_limit_num, {
    val <- input$fc_limit_num
    if (!is.null(val) && !is.na(val)) {
      updateSliderInput(session, "fc_limit", value = val)
    }
  })
  
  # -- DEP reactive store (declared BEFORE sidebar_status to prevent crash) --
  dep <- reactiveValues(
    int_mat=NULL, coldata=NULL, res_df=NULL,
    all_results=NULL, contrasts=NULL, status="idle"
  )

  # -- DESeq2 reactive store -------------------------------------------------
  dge <- reactiveValues(
    dds=NULL, res_df=NULL, all_results=NULL, contrasts=NULL,
    vst_mat=NULL, coldata=NULL, status="idle"
  )

  # -- Helpers ---------------------------------------------------------------
  col_to_hex <- function(col_name) {
    tryCatch({ v <- col2rgb(col_name); sprintf("#%02X%02X%02X", v[1,1], v[2,1], v[3,1]) },
             error=function(e) "#000000")
  }
  dge_col <- function(input_id, fallback) {
    v <- input[[input_id]]
    if (is.null(v) || !nzchar(v)) fallback else v
  }
  org_pkg <- function(org_code) {
    switch(org_code, "hsa"="org.Hs.eg.db","mmu"="org.Mm.eg.db","rno"="org.Rn.eg.db",
           "dre"="org.Dr.eg.db","dme"="org.Dm.eg.db","cel"="org.Ce.eg.db",
           "sce"="org.Sc.sgd.db","org.Hs.eg.db")
  }
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && a != "") a else b
  make_tile <- function(val, lbl, col="#1a73e8") {
    tags$div(class="info-tile",
      tags$div(class="tile-val", style=paste0("color:",col,";"), val),
      tags$div(class="tile-lbl", lbl))
  }

  # -- Sidebar status --------------------------------------------------------
  output$sidebar_status <- renderUI({
    dge_dot <- switch(dge$status, idle="status-warn", running="status-warn",
                      done="status-ok", error="status-err", "status-warn")
    tags$div(
      tags$div(class="section-divider", style="margin-bottom:6px;", "DGE"),
      tags$div(
        tags$span(class=paste0("status-dot ", dge_dot)),
        tags$span(style="color:#4a5568;font-size:12px;",
          switch(dge$status, idle="DESeq2: not run", running="DESeq2: running...",
            done=paste0("DESeq2: ", if (!is.null(dge$res_df)) sum(dge$res_df$sig!="NS") else 0, " DE genes"),
            error="DESeq2: error", "DESeq2: not run"))
      ),
      tags$div(class="section-divider", style="margin-top:8px;margin-bottom:6px;", "Proteomics"),
      tags$div(
        tags$span(class=paste0("status-dot ", switch(dep$status, idle="status-warn",
          running="status-warn", done="status-ok", error="status-err", "status-warn"))),
        tags$span(style="color:#4a5568;font-size:12px;",
          switch(dep$status, idle="DEP: not run", running="DEP: running...",
            done=paste0("DEP: ", if (!is.null(dep$res_df)) sum(dep$res_df$sig!="NS", na.rm=TRUE) else 0, " DE proteins"),
            error="DEP: error", "DEP: not run"))
      ),
      tags$div(class="section-divider", style="margin-top:8px;margin-bottom:6px;", "PathView"),
      tags$div(
        tags$span(class=paste0("status-dot ", if (!is.null(rv$expr_data)) "status-ok" else "status-warn")),
        tags$span(style="color:#4a5568;font-size:12px;",
          if (!is.null(rv$expr_data)) paste0(nrow(rv$expr_data)," genes loaded") else "No data loaded")
      ),
      tags$div(style="margin-top:4px;",
        tags$span(class=paste0("status-dot ", if (rv$conversion_done) "status-ok" else "status-warn")),
        tags$span(style="color:#4a5568;font-size:12px;",
          if (rv$conversion_done) "IDs converted" else "IDs not converted")
      ),
      tags$div(style="margin-top:4px;",
        tags$span(class=paste0("status-dot ", if (length(rv$pathview_imgs)>0) "status-ok" else "status-warn")),
        tags$span(style="color:#4a5568;font-size:12px;",
          if (length(rv$pathview_imgs)>0) paste0(length(rv$pathview_imgs)," plots ready") else "No plots yet")
      )
    )
  })

  # -- Example data ----------------------------------------------------------
  make_example <- function(type) {
    set.seed(42)
    if (type == "brca") {
      genes <- c("TP53","BRCA1","BRCA2","MYC","EGFR","CDH1","PIK3CA","PTEN",
                 "RB1","CCND1","ESR1","ERBB2","AKT1","KRAS","VEGFA",
                 "CDK4","CDK6","MDM2","BCL2","BAX","CASP3","CASP9",
                 "CDKN1A","CDKN2A","E2F1","PCNA","MCM2","AURKA","PLK1","CDC20")
      data.frame(gene_symbol=genes,
                 log2FoldChange=round(rnorm(length(genes),0,2),3),
                 padj=round(runif(length(genes),0,0.1),5),
                 baseMean=round(runif(length(genes),50,5000),1))
    } else if (type == "lps") {
      genes <- c("ENSMUSG00000024190","ENSMUSG00000051747","ENSMUSG00000026395",
                 "ENSMUSG00000032012","ENSMUSG00000031764","ENSMUSG00000028037",
                 "ENSMUSG00000020108","ENSMUSG00000041498","ENSMUSG00000037944",
                 "ENSMUSG00000027852","ENSMUSG00000020122","ENSMUSG00000024401")
      data.frame(ensembl_id=genes,
                 log2FoldChange=round(rnorm(length(genes),0,2.5),3),
                 padj=round(runif(length(genes),0,0.1),6))
    } else if (type == "covid") {
      entrez <- c("7157","672","675","4609","1956","999","1029","5290","6597",
                  "10000","207","3845","7422","596","581","842","836","4616")
      data.frame(entrez_id=entrez,
                 log2FoldChange=round(rnorm(length(entrez),0,2),3),
                 padj=round(runif(length(entrez),0,0.05),6))
    }
  }
  observeEvent(input$load_uploaded_expr, {
    req(input$expr_file)
    
    df <- tryCatch({
      read.table(
        input$expr_file$datapath,
        sep = input$file_sep,
        header = input$has_header,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }, error = function(e) {
      showNotification(
        paste("Error reading expression file:", e$message),
        type = "error",
        duration = 10
      )
      return(NULL)
    })
    
    req(df)
    
    rv$expr_data <- df
    rv$conversion_done <- FALSE
    rv$converted_data <- NULL
    
    showNotification("Expression data loaded successfully.", type = "message")
  })
  observeEvent(input$load_example, {
    req(input$example_data != "none")
    rv$expr_data <- make_example(input$example_data)
    rv$conversion_done <- FALSE; rv$converted_data <- NULL; rv$converted_id_type <- NULL
    showNotification("Example dataset loaded.", type="message")
  })

  observeEvent(input$file, {
    req(input$file)
    
    df <- tryCatch({
      read.csv(input$file$datapath, row.names = 1, check.names = FALSE)
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      return(NULL)
    })
    
    req(df)
    
    df <- clean_proteomics(df)
    
    rv$expr_data <- df
  })

  # -- DESeq2 metadata column mapping ----------------------------------------
  rv_meta_cols <- reactiveVal(NULL)

  observeEvent(list(input$dge_meta_file, input$dge_meta_sep), {
    req(input$dge_meta_file)
    tryCatch({
      md <- read.table(input$dge_meta_file$datapath,
                       sep=if (is.null(input$dge_meta_sep)) "," else input$dge_meta_sep,
                       header=TRUE, nrows=5, stringsAsFactors=FALSE, check.names=FALSE)
      rv_meta_cols(colnames(md))
    }, error=function(e) rv_meta_cols(NULL))
  })

  output$dge_meta_col_ui <- renderUI({
    cols <- rv_meta_cols()
    req(!is.null(cols) && length(cols) >= 1)
    samp_default <- if ("sample" %in% cols) "sample"
                    else if (any(grepl("sample|name|id", cols, ignore.case=TRUE)))
                      cols[grepl("sample|name|id", cols, ignore.case=TRUE)][1]
                    else cols[1]
    cond_default <- if ("condition" %in% cols) "condition"
                    else if (any(grepl("cond|group|treat", cols, ignore.case=TRUE)))
                      cols[grepl("cond|group|treat", cols, ignore.case=TRUE)][1]
                    else cols[min(2, length(cols))]
    tagList(
      tags$div(class="section-divider", style="margin:8px 0 4px;", "Metadata Column Mapping"),
      fluidRow(
        column(6, selectInput("dge_meta_samp_col", "Sample ID Column",
          choices=cols, selected=samp_default, width="100%")),
        column(6, selectInput("dge_meta_cond_col", "Condition Column",
          choices=cols, selected=cond_default, width="100%"))
      )
    )
  })

  # -- Column mapping UI (Data Input tab) ------------------------------------
  output$col_mapping_ui <- renderUI({
    req(rv$expr_data)
    cols <- colnames(rv$expr_data)
    tagList(
      selectInput("gene_col_sel", "Gene ID Column", choices=cols, selected=cols[1]),
      selectInput("fc_col_sel", "Fold-Change Column", choices=cols,
        selected=local({
          pval_pat <- "pval|padj|p\\.adj|p_adj|pvalue|p\\.value|fdr|qval"
          non_pval <- cols[!grepl(pval_pat, cols, ignore.case=TRUE)]
          fc_cols  <- non_pval[grepl("log2|fold|lfc", non_pval, ignore.case=TRUE)]
          if (length(fc_cols) > 0) fc_cols[1]
          else if (length(non_pval) > 1) non_pval[2]
          else cols[min(2, length(cols))]
        })),
      selectInput("padj_col_sel", "Adj. P-value Column",
                  choices=c("None"="none", cols), selected="none")
    )
  })

  observe({
    req(input$gene_col_sel); rv$gene_col <- input$gene_col_sel
    req(input$fc_col_sel);   rv$fc_col   <- input$fc_col_sel
  })

  # -- Data tiles (Data Input tab) -------------------------------------------
  output$tile_genes <- renderUI({
    if (is.null(rv$expr_data)) return(make_tile(" - ","Genes"))
    make_tile(nrow(rv$expr_data), "Total Genes")
  })
  output$tile_up <- renderUI({
    if (is.null(rv$expr_data) || is.null(rv$fc_col)) return(make_tile(" - ","Up-regulated"))
    make_tile(sum(rv$expr_data[[rv$fc_col]] > 1, na.rm=TRUE), "Up-reg. (|FC|>1)", "#ef5350")
  })
  output$tile_down <- renderUI({
    if (is.null(rv$expr_data) || is.null(rv$fc_col)) return(make_tile(" - ","Down-regulated"))
    make_tile(sum(rv$expr_data[[rv$fc_col]] < -1, na.rm=TRUE), "Down-reg. (|FC|>1)", "#26a69a")
  })
  output$tile_sig <- renderUI({
    if (is.null(rv$expr_data)) return(make_tile(" - ","Significant"))
    if (!is.null(input$padj_col_sel) && input$padj_col_sel != "none" &&
        input$padj_col_sel %in% colnames(rv$expr_data)) {
      make_tile(sum(rv$expr_data[[input$padj_col_sel]] < 0.05, na.rm=TRUE),
                "Sig. (padj<0.05)", "#ffa726")
    } else { make_tile("N/A", "Sig. (no p-val col)") }
  })

  output$expr_table <- renderDT({
    req(rv$expr_data)
    datatable(rv$expr_data, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE, class="compact")
  })

  output$volcano_plot <- renderPlotly({
    req(rv$expr_data, rv$fc_col)
    df <- rv$expr_data; fc <- df[[rv$fc_col]]
    pval_col <- if (!is.null(input$padj_col_sel) && input$padj_col_sel != "none" &&
                    input$padj_col_sel %in% colnames(df)) input$padj_col_sel else NULL
    pv <- if (!is.null(pval_col)) -log10(df[[pval_col]] + 1e-300) else rep(0, nrow(df))
    color <- ifelse(fc > 1, "#ef5350", ifelse(fc < -1, "#26a69a", "#8899bb"))
    plot_ly(x=fc, y=pv, type="scatter", mode="markers",
            marker=list(color=color, size=5, opacity=0.7),
            text=if (!is.null(rv$gene_col)) df[[rv$gene_col]] else NULL,
            hoverinfo="text+x+y") %>%
      layout(paper_bgcolor="#fdfdf6", plot_bgcolor="#fdfaf6",
             xaxis=list(title="log2 Fold Change", gridcolor="#1a2a4a"),
             yaxis=list(title=if (!is.null(pval_col)) "-log10(padj)" else "Index",
                        gridcolor="#adaaa8"))
  })

  output$dist_plot <- renderPlotly({
    req(rv$expr_data, rv$fc_col)
    fc <- rv$expr_data[[rv$fc_col]]
    plot_ly(x=fc, type="histogram",
            marker=list(color="#4fc3f7", line=list(color="#1e3a5f", width=0.5))) %>%
      layout(paper_bgcolor="#fdfdf6", plot_bgcolor="#fdfaf6",
             xaxis=list(title="log2 Fold Change"), yaxis=list(title="Count"))
  })

  # -- Package install check -------------------------------------------------
  output$pkg_install_ui <- renderUI({
    pkg <- org_pkg(input$organism)
    if (!requireNamespace(pkg, quietly=TRUE) ||
        !requireNamespace("pathview", quietly=TRUE) ||
        !requireNamespace("AnnotationDbi", quietly=TRUE)) {
      tags$div(class="alert alert-warning",
        icon("exclamation-triangle"), " Missing packages. Run:",
        tags$pre(style="font-size:10px;margin:4px 0 0;",
          paste0('BiocManager::install(c("pathview","AnnotationDbi","', pkg, '")'))
      )
    } else {
      tags$div(class="alert alert-success", icon("check"), " Required packages installed.")
    }
  })

  # -- Run conversion --------------------------------------------------------
  observeEvent(input$run_conversion, {
    req(rv$expr_data, rv$gene_col, rv$fc_col)
    withProgress(message="Converting gene IDs...", value=0.3, {
      pkg <- org_pkg(input$organism)
      missing_pkgs <- c("AnnotationDbi", pkg)[!sapply(c("AnnotationDbi", pkg), requireNamespace, quietly=TRUE)]
      if (length(missing_pkgs) > 0) {
        showNotification(paste("Missing packages:", paste(missing_pkgs, collapse=", ")), type="error", duration=10)
        return()
      }
      tryCatch({
        db     <- get(pkg, envir=loadNamespace(pkg))
        genes  <- trimws(as.character(rv$expr_data[[rv$gene_col]]))
        fc_vec <- as.numeric(rv$expr_data[[rv$fc_col]])
        if (input$id_from %in% c("ENSEMBL","ENSEMBLTRANS","ENSEMBLPROT"))
          genes <- sub("\\.\\d+$", "", genes)
        setProgress(0.4, "Validating keys...")
        valid_keys <- tryCatch(AnnotationDbi::keys(db, keytype=input$id_from), error=function(e) character(0))
        n_valid <- sum(genes %in% valid_keys)
        if (n_valid == 0) {
          showNotification(paste0("None of your ", input$id_from, " IDs matched ", pkg, "."), type="error", duration=20)
          return()
        }
        setProgress(0.5, "Querying database...")
        mapped <- AnnotationDbi::mapIds(db, keys=genes, column=input$id_to, keytype=input$id_from,
                                        multiVals=if (input$dup_method=="all") "list" else "first")
        setProgress(0.8, "Processing...")
        if (input$dup_method == "all") {
          id_map <- data.frame(original_id=rep(genes, sapply(mapped, length)),
                               converted_id=unlist(mapped), stringsAsFactors=FALSE)
        } else {
          id_map <- data.frame(original_id=genes, converted_id=as.character(mapped), stringsAsFactors=FALSE)
        }
        merged <- merge(data.frame(original_id=genes, log2FoldChange=fc_vec, stringsAsFactors=FALSE),
                        id_map, by="original_id", all.x=TRUE)
        if (input$dup_method == "maxfc") {
          merged <- merged[order(abs(merged$log2FoldChange), decreasing=TRUE), ]
          merged <- merged[!duplicated(merged$converted_id), ]
        }
        rv$unmapped          <- merged[is.na(merged$converted_id) | merged$converted_id=="NA", ]
        rv$converted_data    <- merged[!is.na(merged$converted_id) & merged$converted_id!="NA", ]
        rv$id_map            <- id_map
        rv$converted_id_type <- input$id_to
        rv$conversion_done   <- TRUE
        showNotification(paste0("Conversion complete: ", nrow(rv$converted_data), " mapped, ",
                                nrow(rv$unmapped), " unmapped."), type="message")
      }, error=function(e) showNotification(paste("Conversion error:", e$message), type="error", duration=15))
    })
  })

  output$conversion_summary <- renderUI({
    if (!rv$conversion_done)
      return(tags$div(class="alert alert-info", icon("info-circle"), " Run conversion to see results."))
    n_conv <- nrow(rv$converted_data); n_unm <- nrow(rv$unmapped)
    pct <- round(100 * n_conv / (n_conv + n_unm), 1)
    fluidRow(
      column(4, tags$div(class="info-tile", tags$div(class="tile-val", n_conv), tags$div(class="tile-lbl","Mapped"))),
      column(4, tags$div(class="info-tile", tags$div(class="tile-val", style="color:#ffa726;", n_unm), tags$div(class="tile-lbl","Unmapped"))),
      column(4, tags$div(class="info-tile", tags$div(class="tile-val", style="color:#26a69a;", paste0(pct,"%")), tags$div(class="tile-lbl","Success Rate")))
    )
  })

  output$converted_table <- renderDT({ req(rv$converted_data)
    datatable(rv$converted_data, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE, class="compact") })
  output$unmapped_table  <- renderDT({ req(rv$unmapped)
    datatable(rv$unmapped, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE, class="compact") })
  output$id_map_table    <- renderDT({ req(rv$id_map)
    datatable(rv$id_map, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE, class="compact") })

  observeEvent(input$use_converted, {
    req(rv$converted_data)
    updateTabItems(session, "tabs", "pathview_tab")
    showNotification("Converted data ready for PathView.", type="message")
  })

  # -- KEGG pathway search ---------------------------------------------------
  observeEvent(input$search_pathways, {
    withProgress(message="Fetching KEGG pathway list...", value=0.5, {
      tryCatch({
        req(requireNamespace("KEGGREST", quietly=TRUE))
        pathlist <- KEGGREST::keggList("pathway", input$organism_pv)
        rv$kegg_pathways <- data.frame(
          id=sub("path:","",names(pathlist)), name=as.character(pathlist), stringsAsFactors=FALSE)
        showNotification(paste0("Fetched ", nrow(rv$kegg_pathways), " pathways."), type="message")
      }, error=function(e) showNotification(paste("KEGG error:", e$message), type="error"))
    })
  })

  output$pathway_search_ui <- renderUI({
    req(rv$kegg_pathways)
    df <- rv$kegg_pathways; df$short_id <- sub(input$organism_pv, "", df$id)
    selectizeInput("add_pathway", "Add pathway from list",
                   choices=setNames(df$short_id, paste0(df$short_id,"  -  ",df$name)),
                   multiple=TRUE, options=list(maxItems=20, placeholder="Search pathway..."))
  })

  observeEvent(input$add_pathway, {
    req(input$add_pathway)
    current <- strsplit(trimws(input$pathway_ids), "[,\n\r ]+")[[1]]
    current <- current[nchar(current)>0]
    new_ids <- setdiff(input$add_pathway, current)
    if (length(new_ids) > 0)
      updateTextAreaInput(session, "pathway_ids", value=paste(c(current, new_ids), collapse="\n"))
  })

  # -- PathView run ----------------------------------------------------------
  pv_warn_handler <- function(w) {
    if (grepl("cannot create dir", conditionMessage(w), fixed=TRUE)) {
      m <- regmatches(conditionMessage(w), regexpr("'[^']+'", conditionMessage(w)))
      if (length(m) > 0) dir.create(gsub("'","",m), recursive=TRUE, showWarnings=FALSE)
      invokeRestart("muffleWarning")
    }
  }

  run_pathview_safe <- function(fc_named, pid, species, kegg_native, fc_limit, low_col, mid_col, high_col) {
    pv_call <- function(native, same_layer) {
      withCallingHandlers(
        pathview::pathview(gene.data=fc_named, pathway.id=pid, species=species,
          kegg.native=native, limit=list(gene=fc_limit, cpd=1),
          low=list(gene=low_col, cpd="blue"), mid=list(gene=mid_col, cpd="gray"),
          high=list(gene=high_col, cpd="red"),
          out.suffix="pathview", new.signature=FALSE, same.layer=same_layer),
        warning=pv_warn_handler)
    }
    strategies <- list(
      list(native=kegg_native, same_layer=TRUE,  note=NULL),
      list(native=kegg_native, same_layer=FALSE, note=paste0(pid, ": used same.layer=FALSE.")),
      list(native=!kegg_native,same_layer=TRUE,  note=paste0(pid, ": switched render mode.")),
      list(native=!kegg_native,same_layer=FALSE, note=paste0(pid, ": last-resort render mode."))
    )
    last_err <- NULL
    for (s in strategies) {
      result <- tryCatch(pv_call(s$native, s$same_layer), error=function(e) { last_err <<- e; NULL })
      if (!is.null(result)) {
        if (!is.null(s$note)) showNotification(s$note, type="warning", duration=10)
        return(result)
      }
    }
    stop(last_err)
  }

  observeEvent(input$run_pathview, {
    use_converted <- rv$conversion_done && !is.null(rv$converted_data)
    if (!use_converted) {
      showNotification(tags$div(tags$b("[!] Gene ID conversion required"),
        tags$br(), "Go to Gene ID Converter, convert to Entrez IDs, then retry."),
        type="error", duration=20); return()
    }
    if (!is.null(rv$converted_id_type) && rv$converted_id_type != "ENTREZID") {
      showNotification(tags$div(tags$b("[!] Wrong ID type"),
        tags$br(), paste0("Last conversion produced '", rv$converted_id_type, "'. Need ENTREZID.")),
        type="error", duration=20); return()
    }
    fc_named <- setNames(as.numeric(rv$converted_data$log2FoldChange),
                         as.character(rv$converted_data$converted_id))
    fc_named <- fc_named[!is.na(names(fc_named)) & names(fc_named)!="NA" & !is.na(fc_named)]
    fc_named <- fc_named[!duplicated(names(fc_named))]
    raw_ids <- strsplit(trimws(input$pathway_ids), "[,\n\r ]+")[[1]]
    raw_ids <- raw_ids[nchar(trimws(raw_ids)) > 0]
    if (length(raw_ids) == 0) { showNotification("Please enter at least one pathway ID.", type="error"); return() }
    pathway_ids <- sapply(raw_ids, function(x) if (!grepl("^[a-z]{3}", x)) paste0(input$organism_pv, x) else x)
    if (!requireNamespace("pathview", quietly=TRUE)) {
      showNotification("Install pathview: BiocManager::install('pathview')", type="error"); return() }
    out_dir <- file.path(tempdir(), paste0("pathview_", as.integer(Sys.time())))
    dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
    img_list <- list(); pathway_gene_list <- list()
    withProgress(message="Generating PathView plots...", value=0, {
      for (i in seq_along(pathway_ids)) {
        pid <- pathway_ids[i]; setProgress(i/length(pathway_ids), detail=paste("Pathway:", pid))
        tryCatch({
          old_wd <- getwd(); setwd(out_dir); on.exit(setwd(old_wd), add=TRUE)
          pv_result <- run_pathview_safe(
            fc_named=fc_named, pid=pid, species=input$organism_pv,
            kegg_native=as.logical(input$kegg_native), fc_limit=input$fc_limit,
            low_col=if (is.null(input$low_col)  || !nzchar(input$low_col))  "blue" else input$low_col,
            mid_col=if (is.null(input$mid_col)  || !nzchar(input$mid_col))  "gray" else input$mid_col,
            high_col=if (is.null(input$high_col) || !nzchar(input$high_col)) "red"  else input$high_col)
          pathway_entrez <- tryCatch({
            links <- KEGGREST::keggLink(input$organism_pv, paste0("path:", pid))
            
            # keggLink returns genes as the VALUES, not the names
            unique(sub("^[a-z]+:", "", unname(links)))
          }, error=function(e) character(0))
          pathway_gene_list[[pid]] <- pathway_entrez
          setwd(old_wd)
          pngs <- list.files(out_dir, pattern=paste0(pid,".*\\.png$"), full.names=TRUE)
          if (length(pngs) > 0) img_list[[pid]] <- pngs[1]
          else {
            pdfs <- list.files(out_dir, pattern=paste0(pid,".*\\.pdf$"), full.names=TRUE)
            if (length(pdfs) > 0) img_list[[pid]] <- pdfs[1]
          }
        }, error=function(e) showNotification(paste0("Error on ", pid, ": ", e$message), type="warning", duration=8))
      }
    })
    rv$pathview_imgs <- img_list; rv$pathway_genes <- pathway_gene_list; rv$pathview_outdir <- out_dir
    if (length(img_list) > 0)
      showNotification(paste0(length(img_list), " PathView plot(s) generated!"), type="message")
    else
      showNotification("No plots generated. Check pathway IDs and organism.", type="error")
  })

  # -- Gene panel in PathView ------------------------------------------------
  pv_gene_df <- reactive({
    req(input$selected_pathway, rv$conversion_done, rv$converted_data)
    
    df <- data.frame(
      EntrezID = as.character(rv$converted_data$converted_id),
      Symbol = as.character(rv$converted_data$original_id),
      Log2FC = as.numeric(rv$converted_data$log2FoldChange),
      stringsAsFactors = FALSE
    )
    
    df <- df[!is.na(df$Log2FC) &
               !is.na(df$EntrezID) &
               df$EntrezID != "NA" &
               nzchar(df$EntrezID), ]
    
    if (nrow(df) == 0) return(NULL)
    
    selected <- input$selected_pathway
    selected_short <- sub("^[a-z]{3}", "", selected)
    selected_path <- paste0("path:", selected)
    
    possible_keys <- c(selected, selected_short, selected_path)
    
    pathway_entrez <- NULL
    for (k in possible_keys) {
      if (!is.null(rv$pathway_genes[[k]])) {
        pathway_entrez <- rv$pathway_genes[[k]]
        break
      }
    }
    
    if (!is.null(pathway_entrez) && length(pathway_entrez) > 0) {
      pathway_entrez <- gsub("^[a-z]{3}:", "", pathway_entrez)
      pathway_entrez <- gsub("^path:", "", pathway_entrez)
      pathway_entrez <- as.character(pathway_entrez)
      
      df <- df[df$EntrezID %in% pathway_entrez, ]
    }
    
    if (nrow(df) == 0) return(NULL)
    
    lim <- if (!is.null(input$fc_limit)) input$fc_limit else 1
    
    df$Direction <- ifelse(
      df$Log2FC >= lim, "Up",
      ifelse(df$Log2FC <= -lim, "Down", "Within limit")
    )
    
    df <- df[df$Direction != "Within limit", ]
    
    if (nrow(df) == 0) return(NULL)
    
    df <- df[order(-abs(df$Log2FC)), ]
    df$Log2FC <- round(df$Log2FC, 3)
    
    df[, c("EntrezID", "Symbol", "Log2FC", "Direction")]
  
  })


  output$pv_gene_summary_ui <- renderUI({
    df <- pv_gene_df()
    if (is.null(df) || nrow(df) == 0)
      return(tags$div(class="alert alert-info", style="font-size:11px;padding:6px;",
        icon("info-circle"), " Run PathView and load expression data to see genes."))
    col_up <- col_to_hex(if (!is.null(input$high_col) && nchar(input$high_col)>0) input$high_col else "red")
    col_dn <- col_to_hex(if (!is.null(input$low_col)  && nchar(input$low_col)>0)  input$low_col  else "blue")
    fluidRow(
      column(6, tags$div(class="info-tile",
        tags$div(class="tile-val", style=paste0("color:",col_up,";font-size:20px;"), sum(df$Direction=="Up")),
        tags$div(class="tile-lbl","Up"))),
      column(6, tags$div(class="info-tile",
        tags$div(class="tile-val", style=paste0("color:",col_dn,";font-size:20px;"), sum(df$Direction=="Down")),
        tags$div(class="tile-lbl","Down")))
    )
  })

  output$pv_up_table <- renderDT({
    df <- pv_gene_df(); req(!is.null(df))
    col_up <- col_to_hex(if (!is.null(input$high_col) && nchar(input$high_col)>0) input$high_col else "red")
    up <- df[df$Direction=="Up", c("Symbol","EntrezID","Log2FC"), drop=FALSE]
    if (nrow(up)==0) return(datatable(data.frame(Symbol=character(),EntrezID=character(),Log2FC=numeric()),
      options=list(dom="t"), rownames=FALSE))
    datatable(up, options=list(pageLength=8, dom="tp", scrollX=TRUE, order=list(list(2,"desc"))),
              rownames=FALSE, class="compact") |>
      formatStyle("Log2FC", color=col_up, fontWeight="bold")
  })

  output$pv_dn_table <- renderDT({
    df <- pv_gene_df(); req(!is.null(df))
    col_dn <- col_to_hex(if (!is.null(input$low_col) && nchar(input$low_col)>0) input$low_col else "blue")
    dn <- df[df$Direction=="Down", c("Symbol","EntrezID","Log2FC"), drop=FALSE]
    if (nrow(dn)==0) return(datatable(data.frame(Symbol=character(),EntrezID=character(),Log2FC=numeric()),
      options=list(dom="t"), rownames=FALSE))
    datatable(dn, options=list(pageLength=8, dom="tp", scrollX=TRUE, order=list(list(2,"asc"))),
              rownames=FALSE, class="compact") |>
      formatStyle("Log2FC", color=col_dn, fontWeight="bold")
  })

  output$dl_pv_genes <- downloadHandler(
    filename=function() paste0("pathway_genes_", input$selected_pathway, "_", Sys.Date(), ".csv"),
    content =function(file) { df <- pv_gene_df(); req(df); write.csv(df, file, row.names=FALSE) })

  # -- PathView plot display -------------------------------------------------
  output$pathview_status <- renderUI({
    if (length(rv$pathview_imgs)==0)
      tags$div(class="alert alert-info", icon("info-circle"), " ",
        if (!rv$conversion_done) "Tip: Convert gene IDs first, then generate plots."
        else "Configure settings and click 'Generate PathView Plots'.")
    else
      tags$div(class="alert alert-success", icon("check-circle"), " ",
        paste0(length(rv$pathview_imgs), " pathway plot(s) ready."))
  })

  output$pathway_selector_ui <- renderUI({
    req(length(rv$pathview_imgs) > 0)
    
    pids <- names(rv$pathview_imgs)
    
    tagList(
      selectInput(
        "selected_pathway",
        "Select Pathway to View",
        choices = pids,
        selected = pids[1],
        width = "100%"
      ),
      
      tags$div(
        style = "margin-top:8px;",
        lapply(pids, function(pid) {
          actionButton(
            inputId = paste0("pv_pick_", gsub("[^A-Za-z0-9]", "_", pid)),
            label = pid,
            class = "btn btn-default btn-sm",
            style = "margin:2px;",
            onclick = sprintf(
              "Shiny.setInputValue('clicked_pathway', '%s', {priority: 'event'});",
              pid
            )
          )
        })
      )
    )
  })
  observeEvent(input$clicked_pathway, {
    req(input$clicked_pathway)
    
    updateSelectInput(
      session,
      "selected_pathway",
      selected = input$clicked_pathway
    )
  })

  output$pathway_plot_display <- renderUI({
    req(input$selected_pathway, rv$pathview_imgs[[input$selected_pathway]])
    img_path <- rv$pathview_imgs[[input$selected_pathway]]
    if (!file.exists(img_path))
      return(tags$div(class="alert alert-danger", "Plot file not found: ", img_path))
    img_data <- base64enc::base64encode(img_path)
    ext <- tolower(tools::file_ext(img_path))
    if (ext %in% c("png","jpg","jpeg")) {
      tags$div(id="pathway_plot_display",
        tags$img(src=paste0("data:image/",ext,";base64,",img_data),
                 style="max-width:100%;border-radius:4px;"),
        tags$p(style="color:#8899bb;font-size:11px;margin-top:6px;",
          icon("map-marker"), " Pathway: ", input$selected_pathway, " | ", basename(img_path)))
    } else {
      tags$div(class="alert alert-warning", "Plot generated as PDF. Download below.")
    }
  })

  # -- Downloads -------------------------------------------------------------
  output$dl_converted <- downloadHandler(
    filename=function() paste0("converted_ids_", Sys.Date(), ".csv"),
    content =function(file) { req(rv$converted_data); write.csv(rv$converted_data, file, row.names=FALSE) })
  output$dl_current_plot <- downloadHandler(
    filename=function() paste0(input$selected_pathway, "_pathview.png"),
    content =function(file) { req(input$selected_pathway); img <- rv$pathview_imgs[[input$selected_pathway]]; req(file.exists(img)); file.copy(img, file) })
  output$dl_all_plots <- downloadHandler(
    filename=function() paste0("pathview_plots_", Sys.Date(), ".zip"),
    content =function(file) {
      req(length(rv$pathview_imgs) > 0)
      imgs <- unlist(rv$pathview_imgs); imgs <- imgs[file.exists(imgs)]
      if (requireNamespace("zip", quietly=TRUE)) zip::zip(file, files=imgs, mode="cherry-pick")
      else zip(file, files=imgs, flags="-j")
    })
  output$dl_expr_processed <- downloadHandler(
    filename=function() paste0("expression_processed_", Sys.Date(), ".csv"),
    content =function(file) { df <- if (!is.null(rv$converted_data)) rv$converted_data else rv$expr_data; req(df); write.csv(df, file, row.names=FALSE) })
  output$dl_conversion_map <- downloadHandler(
    filename=function() paste0("id_conversion_map_", Sys.Date(), ".csv"),
    content =function(file) { req(rv$id_map); write.csv(rv$id_map, file, row.names=FALSE) })
  output$dl_all_zip <- downloadHandler(
    filename=function() paste0("pathview_all_", Sys.Date(), ".zip"),
    content =function(file) {
      req(length(rv$pathview_imgs) > 0)
      imgs <- unlist(rv$pathview_imgs); imgs <- imgs[file.exists(imgs)]
      if (requireNamespace("zip", quietly=TRUE)) zip::zip(file, files=imgs, mode="cherry-pick")
      else zip(file, files=imgs, flags="-j")
    })

  output$results_summary <- renderUI({
    tags$div(
      tags$table(style="width:100%;font-size:13px;",
        tags$tr(tags$td(style="color:#718096;padding:4px 0;","Genes loaded"),
                tags$td(style="color:#1a73e8;font-family:'DM Mono',monospace;",
                  if (!is.null(rv$expr_data)) nrow(rv$expr_data) else " - ")),
        tags$tr(tags$td(style="color:#718096;padding:4px 0;","ID conversion"),
                tags$td(style="color:#1a73e8;", if (rv$conversion_done) "[OK] Complete" else "[X] Not run")),
        tags$tr(tags$td(style="color:#718096;padding:4px 0;","Genes converted"),
                tags$td(style="color:#1a73e8;font-family:'DM Mono',monospace;",
                  if (!is.null(rv$converted_data)) nrow(rv$converted_data) else " - ")),
        tags$tr(tags$td(style="color:#718096;padding:4px 0;","PathView plots"),
                tags$td(style="color:#1a73e8;font-family:'DM Mono',monospace;", length(rv$pathview_imgs))),
        tags$tr(tags$td(style="color:#718096;padding:4px 0;","Organism"),
                tags$td(style="color:#1a73e8;", input$organism_pv))
      )
    )
  })

  observeEvent(input$show_session, { rv$session_info <- capture.output(sessionInfo()) })
  output$session_info_out <- renderText({ req(rv$session_info); paste(rv$session_info, collapse="\n") })

  # ==========================================================================
  # DESeq2 SERVER MODULE
  # ==========================================================================

  make_deseq2_demo <- function(n_genes=2000, n_samp=6) {
    set.seed(42)
    gids <- paste0("Gene_", sprintf("%04d", seq_len(n_genes)))
    sids <- c(paste0("Ctrl_", 1:3), paste0("Trt_", 1:3))
    base <- rnbinom(n_genes, mu=100, size=2)
    mat  <- matrix(0L, n_genes, n_samp, dimnames=list(gids, sids))
    for (i in seq_len(n_samp))
      mat[, i] <- rnbinom(n_genes, mu=base * runif(1, 0.8, 1.2), size=5)
    de <- sample(n_genes, 200)
    for (i in 4:6) {
      mat[de[1:100],   i] <- rnbinom(100, mu=base[de[1:100]]   * runif(100,3,8),   size=5)
      mat[de[101:200], i] <- rnbinom(100, mu=base[de[101:200]] * runif(100,0.1,0.3),size=5)
    }
    cd <- data.frame(sample=sids,
                     condition=factor(c(rep("Control",3), rep("Treatment",3)),
                                      levels=c("Control","Treatment")),
                     row.names=sids)
    list(counts=mat, coldata=cd)
  }

  output$dge_status_ui <- renderUI({
    switch(dge$status,
      idle    = tags$div(class="alert alert-info",    icon("info-circle"),        " Ready."),
      running = tags$div(class="alert alert-warning", icon("hourglass-half"),     " Running..."),
      done    = tags$div(class="alert alert-success", icon("check-circle"),       " Analysis complete."),
      error   = tags$div(class="alert alert-danger",  icon("exclamation-circle"), " Error — see message above.")
    )
  })

  # Contrast selector helper
  contrast_select <- function(input_id, label) {
    if (is.null(dge$contrasts) || length(dge$contrasts) == 0)
      return(tags$div(class="alert alert-info", style="font-size:11px;padding:6px;",
        icon("info-circle"), " Run DESeq2 to enable contrast selection."))
    selectInput(input_id, label,
      choices=setNames(dge$contrasts, gsub("_vs_"," -> ",dge$contrasts)),
      selected=isolate(input[[input_id]]) %||% dge$contrasts[1], width="100%")
  }

  output$volc_contrast_ui <- renderUI({ contrast_select("volc_contrast", "Contrast") })
  output$dist_contrast_ui <- renderUI({ contrast_select("dist_contrast", "Contrast") })
  output$hm_contrast_ui   <- renderUI({ contrast_select("hm_contrast",   "Contrast") })
  output$tbl_contrast_ui  <- renderUI({ contrast_select("tbl_contrast",  "Contrast") })

  active_contrast_df <- function(cid) {
    req(dge$all_results)
    sel <- input[[cid]]
    if (is.null(sel) || !sel %in% names(dge$all_results)) return(dge$all_results[[1]])
    dge$all_results[[sel]]
  }

  output$dge_tile_total <- renderUI(make_tile(if (!is.null(dge$res_df)) nrow(dge$res_df) else "-", "Genes Tested"))
  output$dge_tile_de    <- renderUI(make_tile(if (!is.null(dge$res_df)) sum(dge$res_df$sig!="NS") else "-", "Significant", "#f4a433"))
  output$dge_tile_up    <- renderUI(make_tile(if (!is.null(dge$res_df)) sum(dge$res_df$sig=="Up") else "-", "Upregulated", "#ef5350"))
  output$dge_tile_dn    <- renderUI(make_tile(if (!is.null(dge$res_df)) sum(dge$res_df$sig=="Down") else "-", "Downregulated", "#26a69a"))

  observeEvent(input$dge_run, {
    needed <- c("DESeq2","SummarizedExperiment","matrixStats","ggrepel","tibble","tidyr")
    missing_p <- needed[!sapply(needed, requireNamespace, quietly=TRUE)]
    if (length(missing_p) > 0) {
      showNotification(paste0("Missing: ", paste(missing_p, collapse=", ")), type="error", duration=15); return()
    }
    shinyjs::show("dge_sidebar_running")
    shinyjs::html("dge_status_div", '<div class="alert alert-warning" style="font-size:12px;padding:8px;"><i class="fa fa-hourglass-half"></i> Running DESeq2...</div>')
    dge$status <- "running"
    withProgress(message="Running DESeq2...", value=0, {
      tryCatch({
        setProgress(0.05, detail="Loading data...")
        dat <- if (input$dge_source == "demo") {
          make_deseq2_demo()
        } else {
          req(input$dge_count_file, input$dge_meta_file)
          cm_raw <- read.table(input$dge_count_file$datapath, sep=input$dge_count_sep,
                               header=isTRUE(input$dge_count_header), row.names=NULL,
                               check.names=FALSE, stringsAsFactors=FALSE)
          gene_ids <- as.character(cm_raw[[1]])
          cm <- as.matrix(cm_raw[, -1, drop=FALSE]); rownames(cm) <- gene_ids
          n_dups <- sum(duplicated(gene_ids))
          if (n_dups > 0) {
            uniq <- unique(gene_ids)
            cm <- do.call(rbind, lapply(uniq, function(g) round(colMeans(cm[rownames(cm)==g,,drop=FALSE]))))
            rownames(cm) <- uniq
            showNotification(paste0("Collapsed ", n_dups, " duplicate gene name(s)."), type="warning", duration=8)
          }
          md <- read.table(input$dge_meta_file$datapath, sep=input$dge_meta_sep,
                           header=TRUE, stringsAsFactors=FALSE, check.names=FALSE)
          samp_col <- if (!is.null(input$dge_meta_samp_col) && nzchar(input$dge_meta_samp_col) &&
                          input$dge_meta_samp_col %in% colnames(md)) input$dge_meta_samp_col
                      else if ("sample" %in% colnames(md)) "sample" else colnames(md)[1]
          cond_col <- if (!is.null(input$dge_meta_cond_col) && nzchar(input$dge_meta_cond_col) &&
                          input$dge_meta_cond_col %in% colnames(md)) input$dge_meta_cond_col
                      else if ("condition" %in% colnames(md)) "condition" else colnames(md)[min(2, ncol(md))]
          if (samp_col == cond_col)
            stop("Sample and condition columns cannot be the same. Check metadata column mapping.")
          rownames(md) <- as.character(md[[samp_col]])
          if (cond_col != "condition") md$condition <- md[[cond_col]]
          shared <- intersect(colnames(cm), rownames(md))
          if (length(shared) < 2)
            stop("Sample names in count matrix do not match metadata. Check delimiter and sample names.")
          cm <- cm[, shared, drop=FALSE]; md <- md[shared, , drop=FALSE]; md$condition <- factor(md$condition)
          list(counts=cm, coldata=md)
        }
        setProgress(0.15, detail="Preprocessing...")
        cts <- dat$counts; cd <- dat$coldata
        if (anyDuplicated(rownames(cts))) {
          uniq <- unique(rownames(cts))
          cts <- do.call(rbind, lapply(uniq, function(g) round(colMeans(cts[rownames(cts)==g,,drop=FALSE]))))
          rownames(cts) <- uniq
        }
        mode(cts) <- "numeric"
        bad <- apply(cts, 1, function(r) any(is.na(r)))
        if (any(bad)) {
          cts <- cts[!bad, , drop=FALSE]
          showNotification(paste0("Removed ", sum(bad), " gene(s) with NA counts."), type="warning", duration=8)
        }
        n_neg <- sum(cts < 0, na.rm=TRUE)
        if (n_neg > 0) {
          if (n_neg/length(cts) > 0.01 || any(cts != floor(cts), na.rm=TRUE)) {
            showNotification("Detected possible log-transformed data. Back-transforming with 2^x.", type="warning", duration=12)
            cts <- floor(2^cts)
          } else { cts[cts < 0] <- 0 }
        }
        cts <- cts[rowSums(cts) >= input$dge_minct, , drop=FALSE]
        setProgress(0.4, detail="Fitting DESeq2 model...")
        dds <- DESeq2::DESeqDataSetFromMatrix(countData=round(cts), colData=cd, design=~condition)
        dds <- DESeq2::DESeq(dds, quiet=TRUE)
        setProgress(0.8, detail="Extracting results...")
        cond_levels <- levels(cd$condition)
        contrast_pairs <- combn(cond_levels, 2, simplify=FALSE)
        all_res_list <- list()
        for (cp in contrast_pairs) {
          cname <- paste0(cp[2], "_vs_", cp[1])
          r <- tryCatch(DESeq2::results(dds, contrast=c("condition",cp[2],cp[1]), alpha=input$dge_padj),
                        error=function(e) NULL)
          if (!is.null(r)) {
            rr <- as.data.frame(r); rr$gene <- rownames(rr); rr$contrast <- cname
            rr$sig <- with(rr, ifelse(!is.na(padj)&padj<input$dge_padj&log2FoldChange>input$dge_lfc,"Up",
                              ifelse(!is.na(padj)&padj<input$dge_padj&log2FoldChange< -input$dge_lfc,"Down","NS")))
            rr$neg_log10_padj <- -log10(pmax(rr$padj, 1e-300))
            all_res_list[[cname]] <- rr
          }
        }
        dc <- names(all_res_list)[1]; rdf <- all_res_list[[dc]]
        vst <- SummarizedExperiment::assay(DESeq2::varianceStabilizingTransformation(dds, blind=TRUE))
        setProgress(0.95, detail="Storing...")
        dge$dds <- dds; dge$res_df <- rdf; dge$all_results <- all_res_list
        dge$contrasts <- names(all_res_list); dge$vst_mat <- vst; dge$coldata <- cd; dge$status <- "done"
        for (cid in c("volc_contrast","dist_contrast","hm_contrast","tbl_contrast"))
          updateSelectInput(session, cid, choices=names(all_res_list), selected=dc)
        n_sig <- sum(rdf$sig!="NS"); n_up <- sum(rdf$sig=="Up"); n_dn <- sum(rdf$sig=="Down")
        setProgress(1)
        shinyjs::hide("dge_sidebar_running")
        shinyjs::html("dge_status_div", paste0(
          '<div class="alert alert-success" style="font-size:12px;padding:8px;">',
          '<i class="fa fa-check-circle"></i> Done: ', n_sig, ' significant genes (',
          n_up, ' up / ', n_dn, ' down).</div>'))
        showNotification(paste0("DESeq2 complete — ", n_sig, " significant genes."), type="message", duration=8)
      }, error=function(e) {
        dge$status <- "error"; shinyjs::hide("dge_sidebar_running")
        shinyjs::html("dge_status_div", paste0(
          '<div class="alert alert-danger" style="font-size:12px;padding:8px;">',
          '<i class="fa fa-exclamation-circle"></i> Error: ', e$message, '</div>'))
        showNotification(paste("DESeq2 error:", e$message), type="error", duration=20)
      })
    })
  })

  output$dge_pathview_msg <- renderUI(NULL)
  observeEvent(input$dge_to_pathview, {
    req(dge$res_df)
    df_pv <- dge$res_df[!is.na(dge$res_df$log2FoldChange), c("gene","log2FoldChange")]
    colnames(df_pv)[1] <- "gene_id"
    rv$expr_data <- df_pv; rv$gene_col <- "gene_id"; rv$fc_col <- "log2FoldChange"
    rv$conversion_done <- FALSE; rv$converted_data <- NULL
    output$dge_pathview_msg <- renderUI(tags$div(class="alert alert-success",
      icon("check"), " Sent! Go to Gene ID Converter then PathView."))
    showNotification("DESeq2 results sent to PathView pipeline.", type="message")
    updateTabItems(session, "tabs", "convert_tab")
  })

  output$dge_preview_table <- renderDT({
    req(dge$res_df)
    df <- dge$res_df[order(dge$res_df$padj, na.last=TRUE),
                     c("gene","baseMean","log2FoldChange","lfcSE","pvalue","padj","sig")]
    df[, 2:6] <- lapply(df[, 2:6], function(x) signif(x, 4))
    datatable(df, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE, class="compact") |>
      formatStyle("sig",
        backgroundColor=styleEqual(c("Up","Down","NS"),
          c("rgba(239,83,80,0.12)","rgba(38,166,154,0.12)","transparent")),
        color=styleEqual(c("Up","Down","NS"),c("#ef5350","#26a69a","#718096")), fontWeight="bold")
  })

  # PCA
  output$dge_pca_plot <- renderPlotly({
    req(dge$vst_mat, dge$coldata)
    if (!requireNamespace("matrixStats", quietly=TRUE)) return(NULL)
    mat <- dge$vst_mat; top_n <- min(input$pca_top_n, nrow(mat))
    mat <- mat[order(matrixStats::rowVars(mat), decreasing=TRUE)[seq_len(top_n)], ]
    pca <- prcomp(t(mat), scale.=FALSE)
    ve  <- summary(pca)$importance["Proportion of Variance",] * 100
    sc  <- as.data.frame(pca$x); sc$sample <- rownames(sc)
    sc$condition <- dge$coldata[sc$sample, "condition"]
    px <- input$pca_pc_x; py <- input$pca_pc_y
    grp_cols <- c(col_to_hex(dge_col("dge_grp1_col","royalblue")),
                  col_to_hex(dge_col("dge_grp2_col","tomato")),
                  "#0f9d58","#f4a433")
    p <- plot_ly(sc, x=~get(px), y=~get(py), color=~condition, colors=grp_cols,
      type="scatter", mode="markers",
      marker=list(size=14, opacity=0.85, line=list(width=1.5, color="white")),
      text=~paste0("<b>",sample,"</b><br>",condition), hoverinfo="text") |>
      layout(xaxis=list(title=paste0(px," (",round(ve[px],1),"%)"), gridcolor="#e0e8f5"),
             yaxis=list(title=paste0(py," (",round(ve[py],1),"%)"), gridcolor="#e0e8f5"),
             paper_bgcolor="white", plot_bgcolor="#f8faff",
             font=list(color="#2d3748"), legend=list(font=list(color="#4a5568")))
    if (isTRUE(input$pca_show_labels))
      p <- p |> add_annotations(x=sc[[px]], y=sc[[py]], text=sc$sample,
        showarrow=FALSE, yshift=14, font=list(size=10, color="#4a5568"))
    p
  })

  # Volcano
  output$dge_volcano_ui <- renderUI({
    if (isTRUE(input$volc_interactive)) plotlyOutput("dge_volcano_plotly", height="500px")
    else                                plotOutput("dge_volcano_static",   height="500px")
  })

  volcano_df <- reactive({
    req(dge$all_results)
    base <- active_contrast_df("volc_contrast")
    df <- base[!is.na(base$padj) & !is.na(base$log2FoldChange), ]
    df$sig2 <- with(df, ifelse(padj<input$volc_padj&log2FoldChange>input$volc_lfc,"Up",
                       ifelse(padj<input$volc_padj&log2FoldChange< -input$volc_lfc,"Down","NS")))
    df
  })

  output$dge_volcano_plotly <- renderPlotly({
    df  <- volcano_df()
    top <- head(df[df$sig2!="NS",][order(df[df$sig2!="NS","padj"]),], input$volc_nlabs)
    cm  <- c(Up=col_to_hex(dge_col("dge_up_col","tomato")),
             Down=col_to_hex(dge_col("dge_dn_col","turquoise")),
             NS=col_to_hex(dge_col("dge_ns_col","gray70")))
    ym <- max(df$neg_log10_padj, na.rm=TRUE); xr <- range(df$log2FoldChange, na.rm=TRUE)
    p <- plot_ly(df, x=~log2FoldChange, y=~neg_log10_padj, color=~sig2, colors=cm,
      type="scatter", mode="markers", marker=list(size=5, opacity=0.7),
      text=~paste0("<b>",gene,"</b><br>Log2FC:",round(log2FoldChange,3),"<br>padj:",signif(padj,3)),
      hoverinfo="text") |>
      add_segments(x= input$volc_lfc, xend= input$volc_lfc, y=0, yend=ym,
        line=list(dash="dot",color="#f4a433",width=1), showlegend=FALSE, inherit=FALSE) |>
      add_segments(x=-input$volc_lfc, xend=-input$volc_lfc, y=0, yend=ym,
        line=list(dash="dot",color="#f4a433",width=1), showlegend=FALSE, inherit=FALSE) |>
      add_segments(x=xr[1], xend=xr[2], y=-log10(input$volc_padj), yend=-log10(input$volc_padj),
        line=list(dash="dot",color="#f4a433",width=1), showlegend=FALSE, inherit=FALSE) |>
      layout(xaxis=list(title="Log2 Fold Change",gridcolor="#e0e8f5"),
             yaxis=list(title="-Log10(Adj. P-value)",gridcolor="#e0e8f5"),
             paper_bgcolor="white", plot_bgcolor="#f8faff",
             font=list(color="#2d3748"), legend=list(font=list(color="#4a5568")))
    if (nrow(top) > 0)
      p <- p |> add_annotations(x=top$log2FoldChange, y=top$neg_log10_padj, text=top$gene,
        showarrow=TRUE, arrowcolor="#c9d8f0", arrowsize=0.6,
        font=list(size=9,color="#2d3748"), bgcolor="white", bordercolor="#c9d8f0", borderwidth=1, borderpad=2)
    p
  })

  output$dge_volcano_static <- renderPlot({
    df  <- volcano_df()
    top <- head(df[df$sig2!="NS",][order(df[df$sig2!="NS","padj"]),], input$volc_nlabs)
    cm  <- c(Up=col_to_hex(dge_col("dge_up_col","tomato")),
             Down=col_to_hex(dge_col("dge_dn_col","turquoise")),
             NS=col_to_hex(dge_col("dge_ns_col","gray70")))
    p <- ggplot(df, aes(log2FoldChange, neg_log10_padj, colour=sig2)) +
      geom_point(size=1.5, alpha=0.7) +
      geom_vline(xintercept=c(-input$volc_lfc, input$volc_lfc),
                 linetype="dashed", colour="#f4a433", linewidth=0.5) +
      geom_hline(yintercept=-log10(input$volc_padj),
                 linetype="dashed", colour="#f4a433", linewidth=0.5) +
      scale_colour_manual(values=cm) +
      labs(x="Log2 Fold Change", y="-Log10(Adj. P-value)", colour="") +
      theme_minimal(base_size=13) +
      theme(panel.grid=element_line(colour="#e0e8f5"), axis.text=element_text(colour="#4a5568"))
    if (nrow(top) > 0 && requireNamespace("ggrepel", quietly=TRUE))
      p <- p + ggrepel::geom_text_repel(data=top, aes(label=gene), size=3, colour="#2d3748",
               box.padding=0.4, max.overlaps=20)
    p
  }, bg="white")

  # Distribution
  output$dge_dist_plot <- renderPlotly({
    req(dge$all_results, dge$vst_mat, dge$coldata)
    type <- input$dge_dist_type
    rdf  <- active_contrast_df("dist_contrast")
    if (type == "violin") {
      req(requireNamespace("tidyr", quietly=TRUE), requireNamespace("tibble", quietly=TRUE))
      mat <- dge$vst_mat
      lng <- tidyr::pivot_longer(tibble::rownames_to_column(as.data.frame(mat),"gene"),
                                 -gene, names_to="sample", values_to="vst")
      cd2 <- tibble::rownames_to_column(as.data.frame(dge$coldata), "sample")
      lng <- merge(lng, cd2, by="sample")
      if (isTRUE(input$dge_dist_log)) lng$vst <- log10(lng$vst + 1)
      plot_ly(lng, x=~sample, y=~vst, color=~condition,
        colors=c(col_to_hex(dge_col("dge_grp1_col","royalblue")),
                 col_to_hex(dge_col("dge_grp2_col","tomato"))),
        type="violin", box=list(visible=TRUE), meanline=list(visible=TRUE)) |>
        layout(xaxis=list(title="Sample",gridcolor="#e0e8f5"),
               yaxis=list(title=ifelse(isTRUE(input$dge_dist_log),"Log10(VST+1)","VST"),gridcolor="#e0e8f5"),
               paper_bgcolor="white", plot_bgcolor="#f8faff", font=list(color="#2d3748"))
    } else if (type == "lfc_hist") {
      df2 <- rdf[!is.na(rdf$log2FoldChange) & is.finite(rdf$log2FoldChange), ]
      plot_ly(df2, x=~log2FoldChange, color=~sig,
        colors=c(Down=col_to_hex(dge_col("dge_dn_col","turquoise")),
                 NS=col_to_hex(dge_col("dge_ns_col","gray70")),
                 Up=col_to_hex(dge_col("dge_up_col","tomato"))),
        type="histogram", nbinsx=80, opacity=0.75) |>
        layout(barmode="overlay", xaxis=list(title="Log2 Fold Change",gridcolor="#e0e8f5"),
               yaxis=list(title="Gene count",gridcolor="#e0e8f5"),
               paper_bgcolor="white", plot_bgcolor="#f8faff", font=list(color="#2d3748"))
    } else if (type == "pval_hist") {
      df2 <- rdf[!is.na(rdf$pvalue), ]
      plot_ly(df2, x=~pvalue, type="histogram", nbinsx=50,
        marker=list(color="#1a73e8", opacity=0.8, line=list(color="white",width=0.5))) |>
        layout(xaxis=list(title="Raw P-value",gridcolor="#e0e8f5"),
               yaxis=list(title="Gene count",gridcolor="#e0e8f5"),
               paper_bgcolor="white", plot_bgcolor="#f8faff", font=list(color="#2d3748"))
    } else if (type == "ma") {
      df2 <- rdf[!is.na(rdf$baseMean) & !is.na(rdf$log2FoldChange) & rdf$baseMean>0, ]
      df2$log_base <- log10(df2$baseMean)
      cm <- c(Up=col_to_hex(dge_col("dge_up_col","tomato")),
              Down=col_to_hex(dge_col("dge_dn_col","turquoise")),
              NS=col_to_hex(dge_col("dge_ns_col","gray70")))
      plot_ly(df2, x=~log_base, y=~log2FoldChange, color=~sig, colors=cm,
        type="scatter", mode="markers", marker=list(size=4,opacity=0.6),
        text=~paste0("<b>",gene,"</b>"), hoverinfo="text") |>
        layout(xaxis=list(title="Log10(Mean Expression)",gridcolor="#e0e8f5"),
               yaxis=list(title="Log2 Fold Change",gridcolor="#e0e8f5"),
               paper_bgcolor="white", plot_bgcolor="#f8faff", font=list(color="#2d3748"))
    } else if (type == "disp") {
      req(dge$dds)
      dd <- data.frame(mean_expr=rowMeans(DESeq2::counts(dge$dds, normalized=TRUE)),
                       dispersion=DESeq2::dispersions(dge$dds))
      dd <- dd[!is.na(dd$dispersion) & dd$mean_expr>0, ]
      plot_ly(dd, x=~log10(mean_expr), y=~log10(dispersion),
        type="scatter", mode="markers", marker=list(color="#1a73e8",size=4,opacity=0.5)) |>
        layout(xaxis=list(title="Log10(Mean Expression)",gridcolor="#e0e8f5"),
               yaxis=list(title="Log10(Dispersion)",gridcolor="#e0e8f5"),
               paper_bgcolor="white", plot_bgcolor="#f8faff", font=list(color="#2d3748"))
    }
  })

  # Heatmap
  heatmap_data <- reactive({
    req(dge$vst_mat, dge$coldata, dge$all_results)
    rdf <- active_contrast_df("hm_contrast"); req(!is.null(rdf))
    sig_df <- rdf[!is.na(rdf$padj) & rdf$sig != "NS", , drop=FALSE]
    if (nrow(sig_df) == 0) sig_df <- rdf[!is.na(rdf$padj), , drop=FALSE]
    if (input$hm_order_by == "padj") sig_df <- sig_df[order(sig_df$padj, na.last=TRUE), ]
    else                             sig_df <- sig_df[order(-abs(sig_df$log2FoldChange), na.last=TRUE), ]
    top_genes <- intersect(head(sig_df$gene, input$hm_top_n), rownames(dge$vst_mat))
    if (length(top_genes) == 0) return(NULL)
    mat <- dge$vst_mat[top_genes, , drop=FALSE]
    if (isTRUE(input$hm_scale_rows)) { mat <- t(scale(t(mat))); mat[is.nan(mat)] <- 0 }
    if (input$hm_order_by == "hclust" && nrow(mat) > 1) { hc <- hclust(dist(mat)); mat <- mat[hc$order, , drop=FALSE] }
    list(mat=mat, coldata=dge$coldata)
  })

  get_hm_palette <- function(name, n, rev) {
    cols <- if (name %in% c("viridis","plasma")) {
      if (requireNamespace("viridisLite", quietly=TRUE))
        if (name=="viridis") viridisLite::viridis(n) else viridisLite::plasma(n)
      else grDevices::colorRampPalette(c("#440154","#31688e","#35b779","#fde725"))(n)
    } else {
      if (requireNamespace("RColorBrewer", quietly=TRUE))
        RColorBrewer::brewer.pal(min(n,11), name)
      else grDevices::colorRampPalette(c("blue","white","red"))(n)
    }
    if (rev) rev(cols) else cols
  }

  draw_heatmap_base <- function(mat, cd, pal_name="RdBu", rev_pal=TRUE, show_labs=TRUE,
                                grp1_col="royalblue", grp2_col="tomato") {
    col_ramp <- grDevices::colorRampPalette(get_hm_palette(pal_name, 11, rev_pal))(100)
    cond <- as.character(cd[colnames(mat), "condition"])
    uniq_cond <- unique(cond)
    ann_cols <- setNames(c(col_to_hex(grp1_col), col_to_hex(grp2_col), "#0f9d58","#f4a433")[seq_along(uniq_cond)],
                         uniq_cond)
    col_ann <- ann_cols[cond]
    n_genes <- nrow(mat); n_samples <- ncol(mat)
    left_m <- if (show_labs) max(0.08, min(0.25, 6*max(nchar(rownames(mat)))/100)) else 0.04
    right_m <- 0.12
    graphics::layout(matrix(c(1,2,3), nrow=3), heights=c(1,1,18))
    par(mar=c(0, left_m*20, 0.5, right_m*20)*5, oma=c(2,0,2,0), bg="white")
    z_seq <- seq(min(mat,na.rm=TRUE), max(mat,na.rm=TRUE), length.out=100)
    image(matrix(z_seq,nrow=1), col=col_ramp, axes=FALSE)
    axis(1, at=c(0,0.5,1),
         labels=round(c(min(mat,na.rm=TRUE), mean(range(mat,na.rm=TRUE)), max(mat,na.rm=TRUE)), 2),
         cex.axis=0.65, col.axis="#4a5568", tick=FALSE, line=-0.8)
    mtext("Color scale", side=3, cex=0.6, col="#718096", line=0.1)
    par(mar=c(0, left_m*20, 0, right_m*20)*5)
    image(matrix(seq_along(col_ann),nrow=1), col=col_ann, axes=FALSE)
    legend("right", legend=uniq_cond, fill=ann_cols[uniq_cond], bty="n",
           cex=0.7, text.col="#2d3748", border=NA, xpd=TRUE, inset=-right_m*0.9)
    par(mar=c(2, left_m*20, 0.3, right_m*20)*5)
    image(t(mat[nrow(mat):1,,drop=FALSE]), col=col_ramp, axes=FALSE)
    axis(1, at=seq(0,1,length.out=n_samples), labels=colnames(mat), las=2,
         cex.axis=0.7, col.axis="#4a5568", tick=FALSE, line=-0.5)
    if (show_labs)
      axis(2, at=seq(0,1,length.out=n_genes), labels=rev(rownames(mat)), las=2,
           cex.axis=max(0.4, min(0.8, 10/n_genes)), col.axis="#2d3748", tick=FALSE, line=-0.5)
    mtext(paste0("Top ", n_genes, " DE genes"), side=3, cex=0.75, col="#1a73e8", line=0.1, font=2)
  }

  output$dge_heatmap <- renderPlot({
    hd <- heatmap_data(); req(!is.null(hd))
    draw_heatmap_base(hd$mat, hd$coldata,
      pal_name=if (!is.null(input$hm_palette)) input$hm_palette else "RdBu",
      rev_pal=isTRUE(input$hm_rev_pal), show_labs=isTRUE(input$hm_show_labels),
      grp1_col=dge_col("dge_grp1_col","royalblue"), grp2_col=dge_col("dge_grp2_col","tomato"))
  }, bg="white")

  output$dl_heatmap <- downloadHandler(
    filename=function() paste0("heatmap_", Sys.Date(), ".png"),
    content =function(file) {
      hd <- heatmap_data(); req(!is.null(hd))
      grDevices::png(file, width=1400, height=900, res=130)
      draw_heatmap_base(hd$mat, hd$coldata,
        pal_name=if (!is.null(input$hm_palette)) input$hm_palette else "RdBu",
        rev_pal=isTRUE(input$hm_rev_pal), show_labs=isTRUE(input$hm_show_labels),
        grp1_col=dge_col("dge_grp1_col","royalblue"), grp2_col=dge_col("dge_grp2_col","tomato"))
      grDevices::dev.off()
    })

  # DGE results table
  dge_res_filtered <- reactive({
    req(dge$all_results)
    base <- active_contrast_df("tbl_contrast")
    df <- switch(input$dge_tbl_filter, all=base, sig=base[base$sig!="NS",],
                 up=base[base$sig=="Up",], dn=base[base$sig=="Down",])
    cols <- intersect(c("gene","baseMean","log2FoldChange","lfcSE","stat","pvalue","padj","sig"), colnames(df))
    df <- df[order(df$padj, na.last=TRUE), cols, drop=FALSE]
    num_cols <- intersect(c("baseMean","log2FoldChange","lfcSE","stat","pvalue","padj"), colnames(df))
    df[, num_cols] <- lapply(df[, num_cols, drop=FALSE], function(x) signif(x, 4)); df
  })
  output$dge_full_table <- renderDT({
    req(dge_res_filtered())
    datatable(dge_res_filtered(), options=list(pageLength=15, scrollX=TRUE),
              rownames=FALSE, filter="top", class="compact") |>
      formatStyle("sig",
        backgroundColor=styleEqual(c("Up","Down","NS"),c("rgba(239,83,80,0.12)","rgba(38,166,154,0.12)","transparent")),
        color=styleEqual(c("Up","Down","NS"),c("#ef5350","#26a69a","#718096")), fontWeight="bold")
  })
  output$dge_dl_csv <- downloadHandler(
    filename=function() paste0("DESeq2_results_", Sys.Date(), ".csv"),
    content =function(file) write.csv(dge_res_filtered(), file, row.names=FALSE))

  # ==========================================================================
  # DEP SERVER MODULE
  # ==========================================================================

  make_dep_demo <- function(n_prot=500, n_samp=6) {
    set.seed(123)
    sids  <- c(paste0("Ctrl_",1:3), paste0("Trt_",1:3))
    prots <- paste0("Prot_", sprintf("%03d", seq_len(n_prot)))
    genes <- paste0("Gene_", sprintf("%03d", seq_len(n_prot)))
    mat <- matrix(rnorm(n_prot*n_samp, mean=26, sd=2), nrow=n_prot, dimnames=list(prots, sids))
    de_idx <- sample(n_prot, 80)
    mat[de_idx[1:40],  4:6] <- mat[de_idx[1:40],  4:6] + runif(40, 1.5, 4)
    mat[de_idx[41:80], 4:6] <- mat[de_idx[41:80], 4:6] - runif(40, 1.5, 4)
    mat[sample(length(mat), floor(length(mat)*0.1))] <- NA
    design <- data.frame(label=sids, condition=c(rep("Control",3),rep("Treatment",3)),
                         replicate=c(1,2,3,1,2,3), stringsAsFactors=FALSE)
    list(mat=mat, genes=genes, design=design)
  }
  output$data_preview <- DT::renderDataTable({
    req(rv$expr_data)
    head(rv$expr_data, 50)
  })
  
  output$dep_status_ui <- renderUI({
    switch(dep$status,
      idle    = tags$div(class="alert alert-info",    icon("info-circle"),        " Ready — click Run DEP Analysis."),
      running = tags$div(class="alert alert-warning", icon("hourglass-half"),     " Running DEP, please wait..."),
      done    = tags$div(class="alert alert-success", icon("check-circle"),       " Analysis complete."),
      error   = tags$div(class="alert alert-danger",  icon("exclamation-circle"), " Error — see message above.")
    )
  })

  rv_dep_meta_cols <- reactiveVal(NULL)
  observeEvent(list(input$dep_meta_file, input$dep_meta_sep), {
    req(input$dep_meta_file)
    tryCatch({
      md <- read.table(input$dep_meta_file$datapath,
                       sep=if (is.null(input$dep_meta_sep)) "," else input$dep_meta_sep,
                       header=TRUE, nrows=5, stringsAsFactors=FALSE, check.names=FALSE)
      rv_dep_meta_cols(colnames(md))
    }, error=function(e) rv_dep_meta_cols(NULL))
  })

  output$dep_meta_col_ui <- renderUI({
    cols <- rv_dep_meta_cols(); req(!is.null(cols) && length(cols) >= 1)
    label_default <- if ("label" %in% cols) "label" else cols[1]
    cond_default  <- if ("condition" %in% cols) "condition" else cols[min(2,length(cols))]
    rep_default   <- if ("replicate" %in% cols) "replicate" else cols[min(3,length(cols))]
    tagList(
      tags$div(class="section-divider", style="margin:8px 0 4px;", "Metadata Column Mapping"),
      fluidRow(
        column(4, selectInput("dep_meta_label_col","Label Column",choices=cols,selected=label_default,width="100%")),
        column(4, selectInput("dep_meta_cond_col", "Condition Column",choices=cols,selected=cond_default,width="100%")),
        column(4, selectInput("dep_meta_rep_col",  "Replicate Column",choices=cols,selected=rep_default,width="100%"))
      )
    )
  })

  dep_contrast_select <- function(input_id, label) {
    if (is.null(dep$contrasts) || length(dep$contrasts) == 0)
      return(tags$div(class="alert alert-info", style="font-size:11px;padding:6px;",
        icon("info-circle"), " Run DEP to enable contrast selection."))
    selectInput(input_id, label,
      choices=setNames(dep$contrasts, gsub("_vs_"," vs ",dep$contrasts)),
      selected=isolate(input[[input_id]]) %||% dep$contrasts[1], width="100%")
  }

  output$dep_volc_contrast_ui    <- renderUI(dep_contrast_select("dep_volc_contrast",    "Contrast"))
  output$dep_hm_contrast_ui      <- renderUI(dep_contrast_select("dep_hm_contrast",      "Contrast"))
  output$dep_tbl_contrast_ui     <- renderUI(dep_contrast_select("dep_tbl_contrast",     "Contrast"))
  output$dep_contrast_preview_ui <- renderUI(dep_contrast_select("dep_preview_contrast", "Contrast"))
  output$dep_contrast_send_ui    <- renderUI(dep_contrast_select("dep_send_contrast",    "Contrast to export"))

  active_dep_df <- function(cid) {
    req(dep$all_results)
    sel <- input[[cid]]
    if (is.null(sel) || !sel %in% names(dep$all_results)) return(dep$all_results[[1]])
    dep$all_results[[sel]]
  }

  output$dep_tile_total <- renderUI(make_tile(if (!is.null(dep$res_df)) nrow(dep$res_df) else "-", "Proteins Tested"))
  output$dep_tile_sig   <- renderUI(make_tile(if (!is.null(dep$res_df)) sum(dep$res_df$sig!="NS",na.rm=TRUE) else "-", "Significant", "#f4a433"))
  output$dep_tile_up    <- renderUI(make_tile(if (!is.null(dep$res_df)) sum(dep$res_df$sig=="Up",na.rm=TRUE) else "-", "Upregulated", "#ef5350"))
  output$dep_tile_dn    <- renderUI(make_tile(if (!is.null(dep$res_df)) sum(dep$res_df$sig=="Down",na.rm=TRUE) else "-", "Downregulated", "#26a69a"))

  output$dep_qc_plot <- renderPlot({
    req(dep$int_mat)
    mat <- dep$int_mat; miss_pct <- colMeans(is.na(mat)) * 100
    par(mar=c(6,5,2,2), bg="white")
    bp <- barplot(miss_pct, col="#1a73e8", border=NA, ylab="% Missing values",
                  ylim=c(0, max(miss_pct, 10)*1.15), names.arg=rep("",length(miss_pct)),
                  cex.axis=0.8, col.axis="#4a5568", las=1)
    text(bp, par("usr")[3]-0.5, labels=colnames(mat), srt=45, adj=1, xpd=TRUE, cex=0.75, col="#4a5568")
    mtext("Missing Values per Sample", side=3, cex=0.85, col="#1a73e8", font=2)
  }, bg="white")

  observeEvent(input$dep_run, {
    needed <- c("limma","SummarizedExperiment")
    missing_p <- needed[!sapply(needed, requireNamespace, quietly=TRUE)]
    if (length(missing_p) > 0) {
      shinyjs::html("dep_status_div", paste0(
        '<div class="alert alert-danger" style="font-size:12px;padding:8px;">',
        '<i class="fa fa-exclamation-circle"></i> Missing: ', paste(missing_p, collapse=", "),
        '. BiocManager::install(c("limma","SummarizedExperiment"))</div>'))
      return()
    }
    shinyjs::html("dep_status_div",
      '<div class="alert alert-warning" style="font-size:12px;padding:8px;"><i class="fa fa-hourglass-half"></i> Running DEP...</div>')
    dep$status <- "running"
    withProgress(message="Running DEP analysis...", value=0, {
      tryCatch({
        setProgress(0.05, detail="Loading data...")
        if (input$dep_source == "demo") {
          demo <- make_dep_demo(); mat <- demo$mat; genes <- demo$genes; design <- demo$design
        } else {
          req(input$dep_prot_file, input$dep_meta_file)
        
          prot_raw <- read.table(input$dep_prot_file$datapath, sep=input$dep_prot_sep,
                                 header=TRUE, stringsAsFactors=FALSE, check.names=FALSE)
          id_col <- input$dep_id_col
          name_col <- if (is.null(input$dep_name_col) || input$dep_name_col == "None") {
            input$dep_id_col
          } else {
            input$dep_name_col
          }
          
          if (is.null(id_col) || !id_col %in% colnames(prot_raw)) {
            stop("Please select a valid Protein ID column.")
          }
          protein_ids <- as.character(prot_raw[[id_col]])
          protein_names <- as.character(prot_raw[[name_col]])
          
          mat <- as.matrix(
            prot_raw[, setdiff(colnames(prot_raw), c(name_col, id_col)), drop = FALSE]
          )
          
          rownames(mat) <- make.unique(protein_ids)
          
          dep$protein_map <- data.frame(
            protein = make.unique(protein_ids),
            protein_id = protein_ids,
            protein_name = protein_names,
            stringsAsFactors = FALSE
          )
          design_raw <- read.table(input$dep_meta_file$datapath, sep=input$dep_meta_sep,
                                   header=TRUE, stringsAsFactors=FALSE, check.names=FALSE)
          lbl_col  <- if (!is.null(input$dep_meta_label_col) && input$dep_meta_label_col %in% colnames(design_raw)) input$dep_meta_label_col else colnames(design_raw)[1]
          cond_col <- if (!is.null(input$dep_meta_cond_col)  && input$dep_meta_cond_col  %in% colnames(design_raw)) input$dep_meta_cond_col  else colnames(design_raw)[2]
          rep_col  <- if (!is.null(input$dep_meta_rep_col)   && input$dep_meta_rep_col   %in% colnames(design_raw)) input$dep_meta_rep_col   else colnames(design_raw)[min(3,ncol(design_raw))]
          design <- data.frame(label=as.character(design_raw[[lbl_col]]),
                               condition=as.character(design_raw[[cond_col]]),
                               replicate=as.integer(design_raw[[rep_col]]), stringsAsFactors=FALSE)
          shared <- intersect(colnames(mat), design$label)
          if (length(shared) < 2) stop("Matrix column names do not match design label column.")
          mat <- mat[, shared, drop=FALSE]; design <- design[design$label %in% shared, ]
        }
        # --- CLEAN + FILTER (CRITICAL FIX) ---
        mat <- as.matrix(mat)
        mode(mat) <- "numeric"
        
        # remove all-NA proteins
        mat <- mat[rowSums(is.na(mat)) < ncol(mat), ]
        
        # keep proteins with enough data (adjust threshold if needed)
        min_valid <- ceiling(ncol(mat) * 0.5)
        mat <- mat[rowSums(!is.na(mat)) >= min_valid, ]
        
        # log2 transform if not already
        if (max(mat, na.rm = TRUE) > 50) {
          mat <- log2(mat)
        }
        dep$int_mat <- mat; dep$coldata <- design
        setProgress(0.25, detail="Normalising...")
        mat_norm <- mat
        if (input$dep_norm == "vsn") {
          if (requireNamespace("vsn", quietly=TRUE)) {
            vsn_fit <- vsn::vsn2(mat)
            mat_norm <- vsn::predict(vsn_fit, mat)
          } else {
            col_med <- apply(mat, 2, median, na.rm=TRUE)
            mat_norm <- sweep(mat, 2, col_med - median(col_med))
          }
        } else if (input$dep_norm == "quantile") {
          if (requireNamespace("limma", quietly=TRUE)) mat_norm <- limma::normalizeQuantiles(mat)
        } 
        rownames(mat_norm) <- rownames(mat)
        colnames(mat_norm) <- colnames(mat)
        setProgress(0.40, detail="Imputing...")
        mat_imp <- mat_norm
        if (input$dep_impute != "none" && any(is.na(mat_norm))) {
          if (input$dep_impute == "knn" && requireNamespace("impute", quietly=TRUE)) {
            mat_imp <- impute::impute.knn(mat_norm)$data
          } else {
            for (j in seq_len(ncol(mat_norm))) {
              col <- mat_norm[, j]; na_idx <- is.na(col)
              if (any(na_idx)) {
                col_mean <- mean(col, na.rm=TRUE)
                col_sd <- sd(col, na.rm=TRUE)
                
                if (is.na(col_sd) || col_sd == 0) col_sd <- 0.1
                shift <- if (input$dep_impute == "QRILC") 1.8 else 1.6
                col[na_idx] <- rnorm(sum(na_idx), mean=col_mean - shift*col_sd, sd=col_sd*0.3)
                mat_imp[, j] <- col
              }
            }
          }
        }
        rownames(mat_imp) <- rownames(mat)
        colnames(mat_imp) <- colnames(mat)
        setProgress(0.55, detail="Running limma...")
        cond <- factor(design$condition)
        design_mat <- model.matrix(~0 + cond); colnames(design_mat) <- levels(cond)
        fit <- limma::lmFit(mat_imp, design_mat)
        contrast_pairs <- combn(levels(cond), 2, simplify=FALSE)
        all_res_list <- list()
        for (cp in contrast_pairs) {
          cname <- paste0(cp[2], "_vs_", cp[1])
          cont_mat <- limma::makeContrasts(contrasts=paste0(cp[2],"-",cp[1]), levels=design_mat)
          fit2 <- limma::eBayes(limma::contrasts.fit(fit, cont_mat))
          tt   <- limma::topTable(fit2, coef=1, number=Inf, adjust.method="BH", sort.by="none")
          rr <- data.frame(protein=rownames(tt), log2FoldChange=tt$logFC, pvalue=tt$P.Value,
                           padj=tt$adj.P.Val, AveExpr=tt$AveExpr, t_stat=tt$t,
                           contrast=cname, stringsAsFactors=FALSE) 
          rr <- merge(rr, dep$protein_map, by = "protein", all.x = TRUE, sort = FALSE)
          rr$sig <- with(rr, ifelse(!is.na(padj)&padj<input$dep_padj&log2FoldChange>input$dep_lfc,"Up",
                            ifelse(!is.na(padj)&padj<input$dep_padj&log2FoldChange< -input$dep_lfc,"Down","NS")))
          rr$neg_log10_padj <- -log10(pmax(rr$padj, 1e-300))
          all_res_list[[cname]] <- rr
        }
        dc <- names(all_res_list)[1]; rdf <- all_res_list[[dc]]
        setProgress(0.90, detail="Storing results...")
        dep$int_mat     <- mat_imp; dep$res_df <- rdf
        dep$all_results <- all_res_list; dep$contrasts <- names(all_res_list); dep$status <- "done"
        for (cid in c("dep_volc_contrast","dep_hm_contrast","dep_tbl_contrast",
                      "dep_preview_contrast","dep_send_contrast"))
          updateSelectInput(session, cid, choices=names(all_res_list), selected=dc)
        n_sig <- sum(rdf$sig!="NS",na.rm=TRUE); n_up <- sum(rdf$sig=="Up",na.rm=TRUE); n_dn <- sum(rdf$sig=="Down",na.rm=TRUE)
        setProgress(1)
        shinyjs::html("dep_status_div", paste0(
          '<div class="alert alert-success" style="font-size:12px;padding:8px;">',
          '<i class="fa fa-check-circle"></i> Done: ', n_sig, ' significant proteins (',
          n_up, ' up / ', n_dn, ' down).</div>'))
        showNotification(paste0("DEP complete — ", n_sig, " significant proteins."), type="message", duration=8)
      }, error=function(e) {
        dep$status <- "error"
        shinyjs::html("dep_status_div", paste0(
          '<div class="alert alert-danger" style="font-size:12px;padding:8px;">',
          '<i class="fa fa-exclamation-circle"></i> Error: ', e$message, '</div>'))
        showNotification(paste("DEP error:", e$message), type="error", duration=20)
      })
    })
  })
  observeEvent(input$dep_prot_file, {
    req(input$dep_prot_file)
    
    df <- tryCatch({
      read.table(input$dep_prot_file$datapath,
                 sep = input$dep_prot_sep,
                 header = TRUE,
                 stringsAsFactors = FALSE,
                 check.names = FALSE)
    }, error = function(e) NULL)
    
    req(df)
    
    cols <- colnames(df)
    
    updateSelectInput(session, "dep_id_col",
                      choices = cols,
                      selected = cols[1])
    
    updateSelectInput(session, "dep_name_col",
                      choices = c("None", cols),
                      selected = "None")
  })
  observeEvent(input$dep_to_pathview, {
    req(dep$all_results)
    
    sel <- input$dep_send_contrast
    
    df_pv <- if (!is.null(sel) && sel %in% names(dep$all_results)) {
      dep$all_results[[sel]]
    } else {
      dep$all_results[[1]]
    }
    
    df_pv <- df_pv[!is.na(df_pv$log2FoldChange), , drop = FALSE]
    
    # Prefer gene symbols/protein names for PathView conversion
    ids_for_conversion <- if ("protein_name" %in% colnames(df_pv) &&
                              any(!is.na(df_pv$protein_name) & nzchar(df_pv$protein_name))) {
      as.character(df_pv$protein_name)
    } else if ("protein_id" %in% colnames(df_pv)) {
      as.character(df_pv$protein_id)
    } else {
      as.character(df_pv$protein)
    }
    
    # Clean common messy protein/gene-name formats
    ids_for_conversion <- trimws(ids_for_conversion)
    ids_for_conversion <- sub("^.*\\|([^|]+)\\|.*$", "\\1", ids_for_conversion)
    
    df_out <- data.frame(
      gene_id = ids_for_conversion,
      log2FoldChange = as.numeric(df_pv$log2FoldChange),
      stringsAsFactors = FALSE
    )
    
    df_out <- df_out[
      !is.na(df_out$gene_id) &
        nzchar(df_out$gene_id) &
        !is.na(df_out$log2FoldChange),
      ,
      drop = FALSE
    ]
    
    rv$expr_data <- df_out
    rv$gene_col <- "gene_id"
    rv$fc_col <- "log2FoldChange"
    
    rv$conversion_done <- FALSE
    rv$converted_data <- NULL
    rv$converted_id_type <- NULL
    
    updateSelectInput(session, "organism", selected = "hsa")
    updateSelectInput(session, "id_from", selected = "SYMBOL")
    updateSelectInput(session, "id_to", selected = "ENTREZID")
    
    output$dep_pathview_msg <- renderUI(tags$div(
      class = "alert alert-success",
      icon("check"),
      paste0(
        " Sent ", nrow(df_out),
        " DEP rows to Gene ID Converter. Use SYMBOL → ENTREZID, then PathView."
      )
    ))
    
    showNotification(
      "DEP results sent to Gene ID Converter. Run conversion before PathView.",
      type = "message",
      duration = 8
    )
    
    updateTabItems(session, "tabs", "convert_tab")
  })
    

  output$dep_preview_table <- renderDT({
    req(dep$all_results)
    df <- active_dep_df("dep_preview_contrast")
    df <- head(df[order(df$padj, na.last=TRUE), ], 50)
    show_cols <- intersect(c("protein","log2FoldChange","pvalue","padj","AveExpr","sig"), colnames(df))
    df <- df[, show_cols, drop=FALSE]
    num_cols <- intersect(c("log2FoldChange","pvalue","padj","AveExpr"), colnames(df))
    df[, num_cols] <- lapply(df[, num_cols, drop=FALSE], function(x) signif(x, 4))
    datatable(df, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE, class="compact") |>
      formatStyle("sig",
        backgroundColor=styleEqual(c("Up","Down","NS"),c("rgba(239,83,80,0.12)","rgba(38,166,154,0.12)","transparent")),
        color=styleEqual(c("Up","Down","NS"),c("#ef5350","#26a69a","#718096")), fontWeight="bold")
  })

  # DEP Volcano
  dep_volcano_df <- reactive({
    req(dep$all_results)
    df <- active_dep_df("dep_volc_contrast")
    df <- df[!is.na(df$padj) & !is.na(df$log2FoldChange), ]
    df$sig2 <- with(df, ifelse(padj<input$dep_volc_padj&log2FoldChange>input$dep_volc_lfc,"Up",
                       ifelse(padj<input$dep_volc_padj&log2FoldChange< -input$dep_volc_lfc,"Down","NS")))
    df
  })

  output$dep_volcano_ui <- renderUI({
    if (isTRUE(input$dep_volc_interactive)) plotlyOutput("dep_volcano_plotly", height="500px")
    else                                    plotOutput("dep_volcano_static",   height="500px")
  })

  output$dep_volcano_plotly <- renderPlotly({
    df  <- dep_volcano_df()
    top <- head(df[df$sig2!="NS",][order(df[df$sig2!="NS","padj"]),], input$dep_volc_nlabs)
    cm  <- c(Up=col_to_hex(dge_col("dep_up_col","tomato")),
             Down=col_to_hex(dge_col("dep_dn_col","steelblue")),
             NS=col_to_hex(dge_col("dep_ns_col","gray70")))
    ym <- max(df$neg_log10_padj, na.rm=TRUE); xr <- range(df$log2FoldChange, na.rm=TRUE)
    p <- plot_ly(df, x=~log2FoldChange, y=~neg_log10_padj, color=~sig2, colors=cm,
      type="scatter", mode="markers", marker=list(size=6, opacity=0.75),
      text=~paste0("<b>",protein,"</b><br>Log2FC:",round(log2FoldChange,3),"<br>padj:",signif(padj,3)),
      hoverinfo="text") |>
      add_segments(x= input$dep_volc_lfc, xend= input$dep_volc_lfc, y=0, yend=ym,
        line=list(dash="dot",color="#f4a433",width=1), showlegend=FALSE, inherit=FALSE) |>
      add_segments(x=-input$dep_volc_lfc, xend=-input$dep_volc_lfc, y=0, yend=ym,
        line=list(dash="dot",color="#f4a433",width=1), showlegend=FALSE, inherit=FALSE) |>
      add_segments(x=xr[1], xend=xr[2], y=-log10(input$dep_volc_padj), yend=-log10(input$dep_volc_padj),
        line=list(dash="dot",color="#f4a433",width=1), showlegend=FALSE, inherit=FALSE) |>
      layout(xaxis=list(title="Log2 Fold Change",gridcolor="#e0e8f5"),
             yaxis=list(title="-Log10(Adj. P-value)",gridcolor="#e0e8f5"),
             paper_bgcolor="white", plot_bgcolor="#f8faff",
             font=list(color="#2d3748"), legend=list(font=list(color="#4a5568")))
    if (nrow(top) > 0)
      p <- p |> add_annotations(x=top$log2FoldChange, y=top$neg_log10_padj, text=top$protein,
        showarrow=TRUE, arrowcolor="#c9d8f0", arrowsize=0.6,
        font=list(size=9,color="#2d3748"), bgcolor="white", bordercolor="#c9d8f0", borderwidth=1, borderpad=2)
    p
  })

  output$dep_volcano_static <- renderPlot({
    df  <- dep_volcano_df()
    top <- head(df[df$sig2!="NS",][order(df[df$sig2!="NS","padj"]),], input$dep_volc_nlabs)
    cm  <- c(Up=col_to_hex(dge_col("dep_up_col","tomato")),
             Down=col_to_hex(dge_col("dep_dn_col","steelblue")),
             NS=col_to_hex(dge_col("dep_ns_col","gray70")))
    p <- ggplot(df, aes(log2FoldChange, neg_log10_padj, colour=sig2)) +
      geom_point(size=2, alpha=0.75) +
      geom_vline(xintercept=c(-input$dep_volc_lfc, input$dep_volc_lfc),
                 linetype="dashed", colour="#f4a433", linewidth=0.5) +
      geom_hline(yintercept=-log10(input$dep_volc_padj),
                 linetype="dashed", colour="#f4a433", linewidth=0.5) +
      scale_colour_manual(values=cm) +
      labs(x="Log2 Fold Change", y="-Log10(Adj. P-value)", colour="") +
      theme_minimal(base_size=13) +
      theme(panel.grid=element_line(colour="#e0e8f5"), axis.text=element_text(colour="#4a5568"))
    if (nrow(top) > 0 && requireNamespace("ggrepel", quietly=TRUE))
      p <- p + ggrepel::geom_text_repel(data=top, aes(label=protein), size=3, colour="#2d3748",
               box.padding=0.4, max.overlaps=20)
    p
  }, bg="white")

  # DEP Heatmap
  dep_heatmap_data <- reactive({
    req(dep$int_mat, dep$coldata, dep$all_results)
    rdf <- active_dep_df("dep_hm_contrast")
    sig_df <- rdf[!is.na(rdf$padj) & rdf$sig != "NS", , drop=FALSE]
    if (nrow(sig_df) == 0) sig_df <- rdf[!is.na(rdf$padj), , drop=FALSE]
    if (input$dep_hm_order_by == "padj") sig_df <- sig_df[order(sig_df$padj, na.last=TRUE), ]
    else                                 sig_df <- sig_df[order(-abs(sig_df$log2FoldChange), na.last=TRUE), ]
    top_prots <- intersect(head(sig_df$protein, input$dep_hm_top_n), rownames(dep$int_mat))
    if (length(top_prots) == 0) return(NULL)
    mat <- dep$int_mat[top_prots, , drop=FALSE]
    if (isTRUE(input$dep_hm_scale_rows)) { mat <- t(scale(t(mat))); mat[is.nan(mat)] <- 0 }
    if (input$dep_hm_order_by == "hclust" && nrow(mat) > 1) { hc <- hclust(dist(mat)); mat <- mat[hc$order, , drop=FALSE] }
    list(mat=mat, coldata=dep$coldata)
  })

  dep_draw_heatmap <- function(mat, cd, pal_name="RdBu", rev_pal=TRUE, show_labs=TRUE) {
    col_ramp <- grDevices::colorRampPalette(get_hm_palette(pal_name, 11, rev_pal))(100)
    cond <- as.character(cd[match(colnames(mat), cd$label), "condition"])
    uniq_cond <- unique(cond)
    ann_cols <- setNames(c(col_to_hex(dge_col("dge_grp1_col","royalblue")),
                           col_to_hex(dge_col("dge_grp2_col","tomato")),
                           "#0f9d58","#f4a433")[seq_along(uniq_cond)], uniq_cond)
    col_ann  <- ann_cols[cond]
    n_prots  <- nrow(mat); n_samp <- ncol(mat)
    left_m   <- if (show_labs) max(0.08, min(0.30, 7*max(nchar(rownames(mat)))/100)) else 0.04
    right_m  <- 0.12
    graphics::layout(matrix(c(1,2,3), nrow=3), heights=c(1,1,18))
    par(mar=c(0, left_m*20, 0.5, right_m*20)*5, oma=c(2,0,2,0), bg="white")
    z_seq <- seq(min(mat,na.rm=TRUE), max(mat,na.rm=TRUE), length.out=100)
    image(matrix(z_seq,nrow=1), col=col_ramp, axes=FALSE)
    axis(1, at=c(0,0.5,1),
         labels=round(c(min(mat,na.rm=TRUE),mean(range(mat,na.rm=TRUE)),max(mat,na.rm=TRUE)),2),
         cex.axis=0.65, col.axis="#4a5568", tick=FALSE, line=-0.8)
    mtext("Color scale", side=3, cex=0.6, col="#718096", line=0.1)
    par(mar=c(0, left_m*20, 0, right_m*20)*5)
    image(matrix(seq_along(col_ann),nrow=1), col=col_ann, axes=FALSE)
    legend("right", legend=uniq_cond, fill=ann_cols[uniq_cond], bty="n",
           cex=0.7, text.col="#2d3748", border=NA, xpd=TRUE, inset=-right_m*0.9)
    par(mar=c(2, left_m*20, 0.3, right_m*20)*5)
    image(t(mat[nrow(mat):1,,drop=FALSE]), col=col_ramp, axes=FALSE)
    axis(1, at=seq(0,1,length.out=n_samp), labels=colnames(mat), las=2,
         cex.axis=0.7, col.axis="#4a5568", tick=FALSE, line=-0.5)
    if (show_labs)
      axis(2, at=seq(0,1,length.out=n_prots), labels=rev(rownames(mat)), las=2,
           cex.axis=max(0.4,min(0.8,10/n_prots)), col.axis="#2d3748", tick=FALSE, line=-0.5)
    mtext(paste0("Top ", n_prots, " DE proteins"), side=3, cex=0.75, col="#1a73e8", line=0.1, font=2)
  }

  output$dep_heatmap <- renderPlot({
    hd <- dep_heatmap_data(); req(!is.null(hd))
    dep_draw_heatmap(hd$mat, hd$coldata,
      pal_name=if (!is.null(input$dep_hm_palette)) input$dep_hm_palette else "RdBu",
      rev_pal=isTRUE(input$dep_hm_rev_pal), show_labs=isTRUE(input$dep_hm_show_labels))
  }, bg="white")

  output$dep_dl_heatmap <- downloadHandler(
    filename=function() paste0("dep_heatmap_", Sys.Date(), ".png"),
    content =function(file) {
      hd <- dep_heatmap_data(); req(!is.null(hd))
      grDevices::png(file, width=1400, height=900, res=130)
      dep_draw_heatmap(hd$mat, hd$coldata,
        pal_name=if (!is.null(input$dep_hm_palette)) input$dep_hm_palette else "RdBu",
        rev_pal=isTRUE(input$dep_hm_rev_pal), show_labs=isTRUE(input$dep_hm_show_labels))
      grDevices::dev.off()
    })

  dep_res_filtered <- reactive({
    req(dep$all_results)
    base <- active_dep_df("dep_tbl_contrast")
    df <- switch(input$dep_tbl_filter, all=base, sig=base[base$sig!="NS",],
                 up=base[base$sig=="Up",], dn=base[base$sig=="Down",])
    cols <- intersect(c("protein","log2FoldChange","pvalue","padj","AveExpr","t_stat","sig"), colnames(df))
    df <- df[order(df$padj, na.last=TRUE), cols, drop=FALSE]
    num_cols <- intersect(c("log2FoldChange","pvalue","padj","AveExpr","t_stat"), colnames(df))
    df[, num_cols] <- lapply(df[, num_cols, drop=FALSE], function(x) signif(x, 4)); df
  })

  output$dep_full_table <- renderDT({
    req(dep_res_filtered())
    datatable(dep_res_filtered(), options=list(pageLength=15, scrollX=TRUE),
              rownames=FALSE, filter="top", class="compact") |>
      formatStyle("sig",
        backgroundColor=styleEqual(c("Up","Down","NS"),c("rgba(239,83,80,0.12)","rgba(38,166,154,0.12)","transparent")),
        color=styleEqual(c("Up","Down","NS"),c("#ef5350","#26a69a","#718096")), fontWeight="bold")
  })

  output$dep_dl_csv <- downloadHandler(
    filename=function() paste0("DEP_results_", Sys.Date(), ".csv"),
    content =function(file) write.csv(dep_res_filtered(), file, row.names=FALSE))

}

shinyApp(ui, server)
