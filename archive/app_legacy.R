options(shiny.maxRequestSize = 25 * 1024^2)

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(ggplot2)
library(plotly)

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

        /* -- Header & Sidebar -- */
        .skin-blue .main-header .logo { background: #1a73e8; border-bottom: 1px solid #1558b0; }
        .skin-blue .main-header .logo:hover { background: #1558b0; }
        .skin-blue .main-header .navbar { background: #1a73e8; border-bottom: 1px solid #1558b0; }
        .skin-blue .main-sidebar { background: #ffffff; border-right: 1px solid #e0e8f5; }
        .skin-blue .sidebar-menu > li > a { color: #4a5568; font-weight: 400; font-size: 13px; }
        .skin-blue .sidebar-menu > li.active > a,
        .skin-blue .sidebar-menu > li > a:hover { color: #1a73e8; background: #e8f0fe; border-left: 3px solid #1a73e8; }
        .skin-blue .sidebar-menu > li > a .fa { color: #1a73e8; }
        .skin-blue .sidebar-menu > li.active > a .fa { color: #1a73e8; }

        /* -- Main content -- */
        .content-wrapper { background: #f0f4f8; }
        .main-footer { background: #ffffff; border-top: 1px solid #e0e8f5; color: #718096; }

        /* -- Boxes -- */
        .box { background: #ffffff; border: 1px solid #e0e8f5; border-top: 3px solid #1a73e8;
               border-radius: 8px; box-shadow: 0 2px 12px rgba(26,115,232,0.08); }
        .box-header { background: transparent; border-bottom: 1px solid #e8f0fe; padding: 12px 18px; }
        .box-title { color: #1a73e8; font-weight: 600; font-size: 12px; letter-spacing: 0.07em; text-transform: uppercase; }

        /* -- Buttons -- */
        .btn-primary { background: #1a73e8; border-color: #1558b0; font-weight: 600; letter-spacing: 0.04em; border-radius: 6px; }
        .btn-primary:hover { background: #1558b0; border-color: #0f3f8a; box-shadow: 0 4px 12px rgba(26,115,232,0.3); }
        .btn-success { background: #0f9d58; border-color: #0b7a43; font-weight: 600; border-radius: 6px; }
        .btn-success:hover { background: #0b7a43; border-color: #08582f; }
        .btn-warning { background: #f4a433; border-color: #e08e10; font-weight: 600; border-radius: 6px; color: #fff; }
        .btn-default { border-radius: 6px; }

        /* -- Inputs -- */
        .form-control, .selectize-input, textarea {
          background: #ffffff !important; border: 1px solid #c9d8f0 !important;
          color: #2d3748 !important; border-radius: 6px; font-size: 13px;
        }
        .form-control:focus, .selectize-input.focus {
          border-color: #1a73e8 !important; box-shadow: 0 0 0 3px rgba(26,115,232,0.15) !important;
        }
        .selectize-dropdown { background: #ffffff; border: 1px solid #c9d8f0; border-radius: 6px;
                              box-shadow: 0 4px 16px rgba(0,0,0,0.1); }
        .selectize-dropdown-content .option { color: #4a5568; }
        .selectize-dropdown-content .option:hover,
        .selectize-dropdown-content .option.active { background: #e8f0fe; color: #1a73e8; }

        label { color: #4a5568; font-size: 12px; font-weight: 600; letter-spacing: 0.05em; text-transform: uppercase; }

        .well { background: #f8faff; border: 1px solid #e0e8f5; box-shadow: none; border-radius: 6px; }
        hr { border-color: #e0e8f5; }

        /* -- Tabs -- */
        .nav-tabs { border-bottom: 2px solid #e0e8f5; }
        .nav-tabs > li > a { color: #718096; background: transparent; border: none; font-size: 13px; font-weight: 500; padding: 8px 16px; }
        .nav-tabs > li.active > a, .nav-tabs > li > a:hover { color: #1a73e8; background: transparent;
          border: none; border-bottom: 2px solid #1a73e8; margin-bottom: -2px; }

        /* -- DataTables -- */
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

        /* -- Alerts -- */
        .alert-info { background: #e8f0fe; border-color: #c9d8f0; color: #2d5fa8; font-size: 12px; border-radius: 6px; }
        .alert-success { background: #e6f4ea; border-color: #b7dfc4; color: #1e6e3e; font-size: 12px; border-radius: 6px; }
        .alert-danger { background: #fce8e6; border-color: #f5c6c2; color: #b31b1b; font-size: 12px; border-radius: 6px; }
        .alert-warning { background: #fef7e0; border-color: #f8d7a0; color: #7d5000; font-size: 12px; border-radius: 6px; }

        /* -- Status dots -- */
        .badge-pill { border-radius: 10px; padding: 3px 8px; font-size: 11px; }
        .status-dot { display:inline-block; width:8px; height:8px; border-radius:50%; margin-right:6px; }
        .status-ok { background: #0f9d58; box-shadow: 0 0 5px rgba(15,157,88,0.5); }
        .status-warn { background: #f4a433; box-shadow: 0 0 5px rgba(244,164,51,0.5); }
        .status-err { background: #db4437; box-shadow: 0 0 5px rgba(219,68,55,0.5); }

        /* -- Pathway plot & misc -- */
        .shiny-plot-output { background: transparent; }
        .sidebar-form .form-group { margin-bottom: 8px; }
        .shiny-notification { background: #ffffff; border: 1px solid #1a73e8; color: #2d3748;
                              border-radius: 8px; box-shadow: 0 4px 16px rgba(0,0,0,0.12); }

        /* -- Scrollbar -- */
        ::-webkit-scrollbar { width: 6px; height: 6px; }
        ::-webkit-scrollbar-track { background: #f0f4f8; }
        ::-webkit-scrollbar-thumb { background: #c9d8f0; border-radius: 3px; }
        ::-webkit-scrollbar-thumb:hover { background: #1a73e8; }

        /* -- Info tiles -- */
        .info-tile { background: #f8faff; border: 1px solid #e0e8f5; border-radius: 8px;
                     padding: 14px 16px; text-align: center; box-shadow: 0 1px 4px rgba(26,115,232,0.06); }
        .info-tile .tile-val { font-size: 28px; font-weight: 600; color: #1a73e8;
                               font-family: 'DM Mono', monospace; }
        .info-tile .tile-lbl { font-size: 11px; color: #718096; text-transform: uppercase;
                               letter-spacing: 0.07em; margin-top: 3px; }

        /* -- Section divider -- */
        .section-divider { color: #1a73e8; font-size: 11px; font-weight: 600;
                           letter-spacing: 0.1em; text-transform: uppercase;
                           padding: 6px 0 4px 0; margin: 8px 0 4px 0;
                           border-bottom: 2px solid #e8f0fe; }

        /* -- Page headings -- */
        h3 { color: #1a73e8 !important; }

        /* -- Sidebar status text -- */
        .sidebar-status-text { color: #4a5568 !important; font-size: 12px; }

        /* -- Sidebar bottom section -- */
        .skin-blue .sidebar-menu { padding-top: 4px; }
        .main-sidebar hr { border-color: #e0e8f5; margin: 8px 12px; }

        /* -- Pathway plot zoom -- */
        .pv-zoom-wrap {
          position: relative; overflow: hidden; border-radius: 6px;
          background: #f8faff; border: 1px solid #e0e8f5;
          box-shadow: 0 2px 12px rgba(0,0,0,0.08);
          cursor: zoom-in; user-select: none;
        }
        .pv-zoom-wrap.zoomed  { cursor: grab; }
        .pv-zoom-wrap.dragging { cursor: grabbing; }
        .pv-zoom-wrap img {
          display: block; width: 100%; max-width: none !important;
          transform-origin: 0 0; will-change: transform;
          border: none !important; box-shadow: none !important;
        }
        .pv-zoom-controls {
          display: flex; align-items: center; gap: 6px;
          margin-bottom: 8px; flex-wrap: wrap;
        }
        .pv-zoom-btn {
          background: #ffffff; border: 1px solid #c9d8f0; color: #1a73e8;
          border-radius: 6px; padding: 4px 12px; cursor: pointer;
          font-size: 12px; font-weight: 600; letter-spacing: 0.03em;
          transition: background 0.15s;
        }
        .pv-zoom-btn:hover { background: #e8f0fe; }
        .pv-zoom-level {
          font-size: 12px; font-weight: 600; color: #1a73e8;
          font-family: 'DM Mono', monospace; min-width: 44px; text-align: center;
          background: #f0f4f8; border-radius: 4px; padding: 3px 6px;
        }
        .pv-zoom-hint {
          margin-left: auto; font-size: 11px; color: #a0aec0; font-style: italic;
        }
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
      menuItem("Results Table",     tabName = "dge_res_tab", icon = icon("table")),
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

    # -- Pathway plot zoom (scroll-to-zoom + drag-to-pan) --------------------
    tags$script(HTML("
    (function() {
      var st = { scale:1, tx:0, ty:0, drag:false, sx:0, sy:0, stx:0, sty:0 };
      var iw0 = 1, ih0 = 1;   // rendered image dims at scale=1

      function clamp(wrap, tx, ty) {
        var ww = wrap.clientWidth, wh = wrap.clientHeight;
        var iw = iw0 * st.scale,  ih = ih0 * st.scale;
        return {
          tx: Math.max(Math.min(0, ww - iw), Math.min(0, tx)),
          ty: Math.max(Math.min(0, wh - ih), Math.min(0, ty))
        };
      }

      function applyT(img) {
        img.style.transform =
          'translate3d(' + st.tx + 'px,' + st.ty + 'px,0) scale(' + st.scale + ')';
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

        // Reset state for new image
        st.scale = 1; st.tx = 0; st.ty = 0;

        // Capture rendered size once image is ready
        function captureSize() {
          iw0 = img.offsetWidth  || img.clientWidth  || 600;
          ih0 = img.offsetHeight || img.clientHeight || 450;
        }
        if (img.complete && img.naturalWidth) captureSize();
        else img.addEventListener('load', captureSize);

        // Build zoom controls bar
        var ctrl = document.createElement('div');
        ctrl.className = 'pv-zoom-controls';
        ctrl.innerHTML =
          '<button class=\"pv-zoom-btn\" data-z=\"in\">＋ Zoom In</button>' +
          '<span class=\"pv-zoom-level\">100%</span>' +
          '<button class=\"pv-zoom-btn\" data-z=\"out\">－ Zoom Out</button>' +
          '<button class=\"pv-zoom-btn\" data-z=\"rst\">&#8635; Reset</button>' +
          '<span class=\"pv-zoom-hint\">Scroll to zoom &nbsp;·&nbsp; Drag to pan</span>';
        container.insertBefore(ctrl, container.firstChild);
        var lbl = ctrl.querySelector('.pv-zoom-level');

        // Wrap image for overflow clipping
        var wrap = document.createElement('div');
        wrap.className = 'pv-zoom-wrap';
        img.parentNode.insertBefore(wrap, img);
        wrap.appendChild(img);

        function updateLbl() { lbl.textContent = Math.round(st.scale * 100) + '%'; }

        // Control buttons
        ctrl.addEventListener('click', function(e) {
          var btn = e.target.closest('[data-z]');
          if (!btn) return;
          var a = btn.dataset.z;
          if      (a === 'in')  { doZoom(1.35, wrap, img); }
          else if (a === 'out') { doZoom(1/1.35, wrap, img); }
          else if (a === 'rst') { st.scale=1; st.tx=0; st.ty=0; applyT(img); wrap.classList.remove('zoomed'); }
          updateLbl();
        });

        // Scroll to zoom
        wrap.addEventListener('wheel', function(e) {
          e.preventDefault();
          doZoom(e.deltaY < 0 ? 1.12 : 1/1.12, wrap, img, e.clientX, e.clientY);
          updateLbl();
        }, { passive: false });

        // Drag to pan
        wrap.addEventListener('mousedown', function(e) {
          if (st.scale <= 1) return;
          st.drag = true;
          st.sx = e.clientX; st.sy = e.clientY;
          st.stx = st.tx;    st.sty = st.ty;
          wrap.classList.add('dragging');
          e.preventDefault();
        });
        window.addEventListener('mousemove', function(e) {
          if (!st.drag) return;
          var c = clamp(wrap, st.stx + e.clientX - st.sx, st.sty + e.clientY - st.sy);
          st.tx = c.tx; st.ty = c.ty;
          applyT(img);
        });
        window.addEventListener('mouseup', function() {
          if (!st.drag) return;
          st.drag = false;
          wrap.classList.remove('dragging');
        });
      }

      // Re-initialise whenever Shiny redraws the pathway plot output
      $(document).on('shiny:value', function(e) {
        if (e.name === 'pathway_plot_display') {
          setTimeout(function() {
            var c = document.getElementById('pathway_plot_display');
            if (c) { c._pvZoom = false; setupZoom(c); }
          }, 120);
        }
      });

      // Fallback: MutationObserver catches the initial render
      $(document).on('shiny:connected', function() {
        new MutationObserver(function() {
          var c = document.getElementById('pathway_plot_display');
          if (c && c.querySelector('img') && !c._pvZoom) setupZoom(c);
        }).observe(document.body, { childList: true, subtree: true });
      });
    })();
    ")),
    tabItems(

      # -- TAB 1: Data Input ------------------------------------------------
      tabItem(tabName = "input_tab",
        fluidRow(
          column(12,
            tags$h3(style="color:#1a73e8; font-weight:300; margin-bottom:20px; letter-spacing:0.05em;",
              icon("dna", style="margin-right:10px;"), "Gene Expression Data Input")
          )
        ),
        fluidRow(
          column(4,
            box(width = NULL, title = "Upload Expression Data",
              tags$p(style="color:#8899bb; font-size:12px; margin-bottom:12px;",
                "Upload a CSV/TSV with gene IDs in the first column and log2 fold-change values in the second."),
              fileInput("expr_file", "Expression File (.csv / .tsv)",
                        accept = c(".csv",".tsv",".txt"),
                        buttonLabel = "Browse", placeholder = "No file selected"),
              selectInput("file_sep", "Delimiter",
                          choices = c("Comma (,)"=",","Tab (\\t)"="\t","Semicolon (;)"=";"), selected=","),
              checkboxInput("has_header", "File has header row", value = TRUE),
              tags$hr(),
              tags$div(class="section-divider", "Or use example data"),
              selectInput("example_data", "Load Example Dataset",
                          choices = c("None" = "none",
                                      "Human TCGA BRCA (SYMBOL)" = "brca",
                                      "Mouse LPS Macrophage (ENSEMBL)" = "lps",
                                      "Human COVID-19 Lung (ENTREZ)" = "covid")),
              actionButton("load_example", "Load Example", class="btn-warning btn-sm",
                           icon=icon("flask"), width="100%")
            ),
            box(width = NULL, title = "Column Mapping",
              tags$p(style="color:#8899bb;font-size:12px;","Map your data columns after upload."),
              uiOutput("col_mapping_ui")
            )
          ),
          column(8,
            box(width = NULL, title = "Data Preview & Statistics",
              fluidRow(
                column(3, uiOutput("tile_genes")),
                column(3, uiOutput("tile_up")),
                column(3, uiOutput("tile_down")),
                column(3, uiOutput("tile_sig"))
              ),
              tags$br(),
              tabsetPanel(
                tabPanel("Table", DTOutput("expr_table")),
                tabPanel("Volcano Plot", plotlyOutput("volcano_plot", height="400px")),
                tabPanel("Distribution", plotlyOutput("dist_plot",    height="400px"))
              )
            )
          )
        )
      ),

      # -- TAB 2: Gene ID Converter -----------------------------------------
      tabItem(tabName = "convert_tab",
        fluidRow(
          column(12,
            tags$h3(style="color:#4fc3f7;font-weight:300;margin-bottom:20px;letter-spacing:0.05em;",
              icon("exchange-alt",style="margin-right:10px;"), "Gene ID Conversion Tool")
          )
        ),
        fluidRow(
          column(4,
            box(width=NULL, title="Conversion Settings",
              tags$p(style="color:#8899bb;font-size:12px;",
                "Convert gene IDs to ENTREZ (required by PathView). Uses Bioconductor annotation packages."),
              selectInput("organism", "Organism",
                          choices = c(
                            "Human (Homo sapiens)"     = "hsa",
                            "Mouse (Mus musculus)"     = "mmu",
                            "Rat (Rattus norvegicus)"  = "rno",
                            "Zebrafish (Danio rerio)"  = "dre",
                            "Fly (Drosophila melanog.)"= "dme",
                            "Worm (C. elegans)"        = "cel",
                            "Yeast (S. cerevisiae)"    = "sce"
                          ), selected = "hsa"),
              selectInput("id_from", "Input ID Type",
                          choices = c(
                            "Gene Symbol"      = "SYMBOL",
                            "Ensembl Gene ID"  = "ENSEMBL",
                            "Ensembl Transcript"="ENSEMBLTRANS",
                            "RefSeq mRNA"      = "REFSEQ",
                            "UniProt"          = "UNIPROT",
                            "Entrez Gene ID"   = "ENTREZID",
                            "Probe ID (Affy)"  = "PROBEID"
                          ), selected = "SYMBOL"),
              selectInput("id_to", "Output ID Type",
                          choices = c(
                            "Entrez Gene ID (for PathView)" = "ENTREZID",
                            "Gene Symbol"      = "SYMBOL",
                            "Ensembl Gene ID"  = "ENSEMBL",
                            "UniProt"          = "UNIPROT"
                          ), selected = "ENTREZID"),
              tags$hr(),
              tags$div(class="section-divider", "Duplicate handling"),
              radioButtons("dup_method", NULL,
                           choices = c("Keep first match"="first",
                                       "Keep all (1-to-many)"="all",
                                       "Keep highest |FC|"="maxfc"),
                           selected = "first"),
              tags$hr(),
              actionButton("run_conversion", "Run Conversion",
                           class="btn-primary", icon=icon("play"), width="100%"),
              tags$br(),tags$br(),
              uiOutput("pkg_install_ui")
            )
          ),
          column(8,
            box(width=NULL, title="Conversion Results",
              uiOutput("conversion_summary"),
              tags$br(),
              tabsetPanel(
                tabPanel("Converted IDs", DTOutput("converted_table")),
                tabPanel("Unmapped Genes", DTOutput("unmapped_table")),
                tabPanel("ID Map", DTOutput("id_map_table"))
              ),
              tags$br(),
              fluidRow(
                column(6,
                  actionButton("use_converted", "Use Converted Data for PathView",
                               class="btn-success", icon=icon("arrow-right"), width="100%")
                ),
                column(6,
                  downloadButton("dl_converted", "Download Converted Table",
                                 class="btn-sm", style="width:100%")
                )
              )
            )
          )
        )
      ),

      # -- TAB 3: PathView Plots ---------------------------------------------
      tabItem(tabName = "pathview_tab",
        fluidRow(
          column(12,
            tags$h3(style="color:#4fc3f7;font-weight:300;margin-bottom:20px;letter-spacing:0.05em;",
              icon("project-diagram",style="margin-right:10px;"), "PathView Pathway Visualization")
          )
        ),
        fluidRow(
          column(3,
            box(width=NULL, title="Pathway Selection",
              selectInput("organism_pv", "Organism (KEGG code)",
                          choices = c(
                            "hsa  -  Human"    = "hsa",
                            "mmu  -  Mouse"    = "mmu",
                            "rno  -  Rat"      = "rno",
                            "dre  -  Zebrafish"= "dre",
                            "dme  -  Fly"      = "dme",
                            "cel  -  Worm"     = "cel",
                            "sce  -  Yeast"    = "sce"
                          ), selected = "hsa"),
              tags$div(class="section-divider", "Pathway IDs"),
              tags$p(style="color:#8899bb;font-size:12px;",
                "Enter KEGG pathway IDs (numeric part only, e.g. 04110 for Cell Cycle). One per line, or comma-separated."),
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
              sliderInput("fc_limit", "Fold-Change Color Limit (+/-)",
                          min=0.5, max=5, value=2, step=0.25),
              checkboxInput("both_dirs", "Show both directions (up & down)", value=TRUE),
              tags$div(class="section-divider", "Color Scale"),
              # -- Color swatch pickers --------------------------------------
              tags$style(HTML("
                .swatch-group { margin-bottom: 10px; }
                .swatch-label { font-size: 11px; font-weight: 600; color: #4a5568;
                                text-transform: uppercase; letter-spacing: 0.05em;
                                margin-bottom: 5px; display: flex;
                                align-items: center; gap: 8px; }
                .swatch-preview { display: inline-block; width: 18px; height: 18px;
                                  border-radius: 4px; border: 2px solid #c9d8f0;
                                  vertical-align: middle; }
                .swatch-grid { display: flex; flex-wrap: wrap; gap: 4px; }
                .swatch-btn { width: 22px; height: 22px; border-radius: 4px;
                              border: 2px solid transparent; cursor: pointer;
                              transition: transform 0.1s, border-color 0.15s;
                              padding: 0; outline: none; }
                .swatch-btn:hover { transform: scale(1.2); border-color: #1a73e8; }
                .swatch-btn.selected { border-color: #1a73e8; transform: scale(1.15);
                                       box-shadow: 0 0 0 2px rgba(26,115,232,0.3); }
              ")),

              # Hidden inputs to hold selected values
              tags$input(type="hidden", id="low_col",  value="blue"),
              tags$input(type="hidden", id="mid_col",  value="gray"),
              tags$input(type="hidden", id="high_col", value="red"),

              # Swatch widgets
              uiOutput("swatch_low"),
              uiOutput("swatch_mid"),
              uiOutput("swatch_high"),

              # JS to handle swatch clicks and sync to hidden inputs
              tags$script(HTML("
                var rcolors = [
                  // Blues
                  {n:'navy',      h:'#001f5b'}, {n:'royalblue',  h:'#4169E1'},
                  {n:'blue',      h:'#0000FF'}, {n:'dodgerblue', h:'#1E90FF'},
                  {n:'deepskyblue',h:'#00BFFF'}, {n:'steelblue', h:'#4682B4'},
                  {n:'skyblue',   h:'#87CEEB'}, {n:'lightblue',  h:'#ADD8E6'},
                  {n:'cyan',      h:'#00FFFF'}, {n:'turquoise',  h:'#40E0D0'},
                  // Greens
                  {n:'darkgreen', h:'#006400'}, {n:'forestgreen',h:'#228B22'},
                  {n:'green',     h:'#00FF00'}, {n:'limegreen',  h:'#32CD32'},
                  {n:'chartreuse',h:'#7FFF00'}, {n:'yellowgreen',h:'#9ACD32'},
                  {n:'olivedrab', h:'#6B8E23'}, {n:'springgreen',h:'#00FF7F'},
                  {n:'aquamarine',h:'#7FFFD4'}, {n:'palegreen',  h:'#98FB98'},
                  // Reds / Pinks
                  {n:'darkred',   h:'#8B0000'}, {n:'firebrick',  h:'#B22222'},
                  {n:'red',       h:'#FF0000'}, {n:'tomato',     h:'#FF6347'},
                  {n:'orangered', h:'#FF4500'}, {n:'coral',      h:'#FF7F50'},
                  {n:'salmon',    h:'#FA8072'}, {n:'hotpink',    h:'#FF69B4'},
                  {n:'deeppink',  h:'#FF1493'}, {n:'pink',       h:'#FFC0CB'},
                  // Purples
                  {n:'purple',    h:'#800080'}, {n:'darkviolet', h:'#9400D3'},
                  {n:'magenta',   h:'#FF00FF'}, {n:'orchid',     h:'#DA70D6'},
                  {n:'plum',      h:'#DDA0DD'}, {n:'mediumpurple',h:'#9370DB'},
                  {n:'blueviolet',h:'#8A2BE2'}, {n:'indigo',     h:'#4B0082'},
                  {n:'violet',    h:'#EE82EE'}, {n:'lavender',   h:'#E6E6FA'},
                  // Yellows / Oranges
                  {n:'gold',      h:'#FFD700'}, {n:'yellow',     h:'#FFFF00'},
                  {n:'orange',    h:'#FFA500'}, {n:'darkorange', h:'#FF8C00'},
                  {n:'lightyellow',h:'#FFFFE0'}, {n:'khaki',     h:'#F0E68C'},
                  {n:'wheat',     h:'#F5DEB3'}, {n:'peachpuff',  h:'#FFDAB9'},
                  // Grays / Neutrals
                  {n:'black',     h:'#000000'}, {n:'gray10',     h:'#1A1A1A'},
                  {n:'gray30',    h:'#4D4D4D'}, {n:'gray',       h:'#808080'},
                  {n:'gray70',    h:'#B3B3B3'}, {n:'gray90',     h:'#E5E5E5'},
                  {n:'white',     h:'#FFFFFF'}, {n:'whitesmoke', h:'#F5F5F5'},
                  {n:'snow',      h:'#FFFAFA'}, {n:'ivory',      h:'#FFFFF0'}
                ];

                // defaults
                var selected = { low_col: 'blue', mid_col: 'gray', high_col: 'red' };

                function buildSwatches(targetId) {
                  var container = document.getElementById('swatches_' + targetId);
                  if (!container) return;
                  container.innerHTML = '';
                  rcolors.forEach(function(c) {
                    var btn = document.createElement('button');
                    btn.className = 'swatch-btn' + (selected[targetId] === c.n ? ' selected' : '');
                    btn.style.backgroundColor = c.h;
                    btn.title = c.n;
                    btn.addEventListener('click', function(e) {
                      e.preventDefault();
                      selected[targetId] = c.n;
                      // update hidden Shiny input
                      var el = document.getElementById(targetId);
                      el.value = c.n;
                      el.dispatchEvent(new Event('change'));
                      Shiny.setInputValue(targetId, c.n, {priority: 'event'});
                      // update preview
                      var prev = document.getElementById('preview_' + targetId);
                      if (prev) prev.style.backgroundColor = c.h;
                      // refresh selected states
                      buildSwatches(targetId);
                    });
                    container.appendChild(btn);
                  });
                }

                // Build on DOM ready and after Shiny renders
                $(document).on('shiny:value', function() {
                  ['low_col','mid_col','high_col'].forEach(buildSwatches);
                });
                setTimeout(function() {
                  ['low_col','mid_col','high_col'].forEach(buildSwatches);
                }, 800);
              ")),
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
                column(6, downloadButton("dl_all_plots",    "Download All Plots (ZIP)",
                                         class="btn-sm", style="width:100%"))
              )
            )
          ),
          column(3,
            box(width=NULL, title="Genes in Pathway",
              tags$p(style="color:#718096;font-size:11px;margin-bottom:8px;",
                "Genes from your expression data found in the selected pathway,",
                " coloured by direction."),
              uiOutput("pv_gene_summary_ui"),
              tags$hr(style="border-color:#e0e8f5;"),
              tags$div(class="section-divider", "Up-regulated"),
              DTOutput("pv_up_table"),
              tags$br(),
              tags$div(class="section-divider", "Down-regulated"),
              DTOutput("pv_dn_table"),
              tags$br(),
              downloadButton("dl_pv_genes", "Download Gene List",
                             class="btn-sm", style="width:100%;")
            )
          )
        )
      ),

      # -- TAB 4: Results & Export -------------------------------------------
      tabItem(tabName = "results_tab",
        fluidRow(
          column(12,
            tags$h3(style="color:#4fc3f7;font-weight:300;margin-bottom:20px;letter-spacing:0.05em;",
              icon("download",style="margin-right:10px;"), "Results & Export")
          )
        ),
        fluidRow(
          column(6,
            box(width=NULL, title="Export Options",
              tags$p(style="color:#8899bb;font-size:12px;","Download processed data and plots."),
              tags$hr(),
              tags$div(class="section-divider", "Data Tables"),
              downloadButton("dl_expr_processed","Processed Expression Data (.csv)",
                             style="width:100%;margin-bottom:8px;"),
              downloadButton("dl_conversion_map","Full ID Conversion Map (.csv)",
                             style="width:100%;margin-bottom:8px;"),
              tags$hr(),
              tags$div(class="section-divider", "Plots"),
              downloadButton("dl_all_zip","All PathView Plots (.zip)",
                             class="btn-success",style="width:100%;margin-bottom:8px;"),
              downloadButton("dl_volcano_pdf","Volcano Plot (.pdf)",
                             style="width:100%;margin-bottom:8px;"),
              tags$hr(),
              tags$div(class="section-divider", "R Session Info"),
              actionButton("show_session","Show Session Info",
                           class="btn-sm btn-default", icon=icon("info"), width="100%")
            )
          ),
          column(6,
            box(width=NULL, title="Analysis Summary",
              uiOutput("results_summary")
            )
          )
        ),
        fluidRow(
          column(12,
            box(width=NULL, title="Session / Package Info",
              verbatimTextOutput("session_info_out")
            )
          )
        )
      ),

      # -- TAB 5: Help -------------------------------------------------------
      tabItem(tabName = "help_tab",
        fluidRow(
          column(12,
            tags$h3(style="color:#4fc3f7;font-weight:300;margin-bottom:20px;letter-spacing:0.05em;",
              icon("question-circle",style="margin-right:10px;"), "Help & Documentation")
          )
        ),
        fluidRow(
          column(6,
            box(width=NULL, title="Quick Start",
              tags$ol(style="color:#c8d8f0;font-size:13px;line-height:2;padding-left:20px;",
                tags$li("Upload your gene expression CSV in the ", tags$b("Data Input"), " tab."),
                tags$li("Map the gene ID column and fold-change column."),
                tags$li("Go to ", tags$b("Gene ID Converter"), " and select your organism and input ID type."),
                tags$li("Click ", tags$b("Run Conversion"), " to convert IDs to ENTREZ format (required by PathView)."),
                tags$li("Click ", tags$b("Use Converted Data for PathView"), "."),
                tags$li("In the ", tags$b("PathView Plots"), " tab, enter KEGG pathway IDs and click ", tags$b("Generate PathView Plots"), "."),
                tags$li("Download results in the ", tags$b("Results & Export"), " tab.")
              )
            ),
            box(width=NULL, title="Input File Format",
              tags$p(style="color:#c8d8f0;font-size:13px;",
                "Your CSV/TSV should have at minimum two columns:"),
              tags$ul(style="color:#c8d8f0;font-size:13px;",
                tags$li(tags$code("gene_id"), "  -  gene identifiers (SYMBOL, ENSEMBL, ENTREZ, etc.)"),
                tags$li(tags$code("log2FoldChange"), "  -  log2 fold-change values"),
                tags$li("Optional: ", tags$code("padj"), ", ", tags$code("pvalue"), ", ", tags$code("baseMean"), " etc.")
              ),
              tags$pre(style="background:#050510;color:#4fc3f7;font-size:11px;border:1px solid #1e3a5f;padding:10px;",
                "gene_id,log2FoldChange,padj\nTP53,2.34,0.0001\nBRCA1,-1.87,0.0023\nMYC,3.12,0.00001\n..."
              )
            )
          ),
          column(6,
            box(width=NULL, title="Required R Packages",
              tags$p(style="color:#8899bb;font-size:12px;",
                "Run the following in R to install all dependencies:"),
              tags$pre(style="background:#050510;color:#4fc3f7;font-size:11px;border:1px solid #1e3a5f;padding:10px;",
"# CRAN
install.packages(c('shiny','shinydashboard','shinyWidgets',
                   'DT','ggplot2','plotly','zip'))

# Bioconductor
if (!requireNamespace('BiocManager', quietly=TRUE))
  install.packages('BiocManager')

BiocManager::install(c(
  'pathview',
  'AnnotationDbi',
  'org.Hs.eg.db',   # Human
  'org.Mm.eg.db',   # Mouse
  'org.Rn.eg.db',   # Rat
  'org.Dr.eg.db',   # Zebrafish
  'org.Dm.eg.db',   # Fly
  'org.Ce.eg.db',   # Worm
  'org.Sc.sgd.db',  # Yeast
  'KEGGREST'
))"
              )
            ),
            box(width=NULL, title="KEGG Pathway IDs",
              tags$p(style="color:#c8d8f0;font-size:13px;",
                "Common pathway IDs (numeric part only):"),
              tags$table(style="width:100%;font-size:12px;color:#8899bb;",
                tags$tr(tags$th(style="color:#4fc3f7;","ID"), tags$th(style="color:#4fc3f7;","Pathway")),
                tags$tr(tags$td("04110"), tags$td("Cell cycle")),
                tags$tr(tags$td("04115"), tags$td("p53 signaling pathway")),
                tags$tr(tags$td("04210"), tags$td("Apoptosis")),
                tags$tr(tags$td("04310"), tags$td("Wnt signaling")),
                tags$tr(tags$td("04630"), tags$td("JAK-STAT signaling")),
                tags$tr(tags$td("04151"), tags$td("PI3K-Akt signaling")),
                tags$tr(tags$td("04010"), tags$td("MAPK signaling")),
                tags$tr(tags$td("00010"), tags$td("Glycolysis")),
                tags$tr(tags$td("05200"), tags$td("Pathways in cancer"))
              ),
              tags$p(style="color:#8899bb;font-size:12px;margin-top:8px;",
                "Full list: ", tags$a("KEGG PATHWAY Database",
                  href="https://www.genome.jp/kegg/pathway.html", target="_blank",
                  style="color:#4fc3f7;"))
            )
          )
        )
      ),


      # ==========================================================
      # DGE ANALYSIS TABS
      # ==========================================================

      # -- TAB: Run DESeq2 ----------------------------------------
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
                choices = c("Demo data (2,000 genes x 6 samples)"="demo",
                            "Upload count matrix + metadata"="upload"),
                selected = "demo"),
              conditionalPanel("input.dge_source == 'upload'",
                fileInput("dge_count_file", "Count Matrix",
                  accept = c(".csv", ".tsv", ".txt", ".tab"),
                  placeholder = "CSV, TSV, or TXT file"),
                selectInput("dge_count_sep", "Count Matrix Delimiter",
                  choices = c("Comma (CSV)" = ",", "Tab (TSV/TXT)" = "\t",
                              "Semicolon"   = ";", "Space"         = " "),
                  selected = ","),
                checkboxInput("dge_count_header", "File has header row", value = TRUE),
                tags$hr(style="border-color:#e0e8f5; margin:6px 0;"),
                fileInput("dge_meta_file", "Sample Metadata",
                  accept = c(".csv", ".tsv", ".txt", ".tab"),
                  placeholder = "CSV, TSV, or TXT file"),
                selectInput("dge_meta_sep", "Metadata Delimiter",
                  choices = c("Comma (CSV)" = ",", "Tab (TSV/TXT)" = "\t",
                              "Semicolon"   = ";", "Space"         = " "),
                  selected = ","),
                tags$div(class="alert alert-info", style="margin-top:6px;padding:8px;font-size:11px;",
                  tags$b("Count matrix:"), " rows = genes, cols = samples (integer counts).",
                  tags$br(),
                  tags$b("Metadata:"), " must have columns ",
                  tags$code("sample"), " and ", tags$code("condition"), ".",
                  tags$br(),
                  tags$b("Example TXT:"), tags$code("gene\tS1\tS2\nGene1\t120\t98"))
              )
            ),
            box(width=NULL, title="DESeq2 Parameters",
              sliderInput("dge_padj",  "Adj. P-value Cutoff", min=0.001, max=0.1,  value=0.05, step=0.001),
              sliderInput("dge_lfc",   "|Log2FC| Cutoff",     min=0.5,   max=3,    value=1,    step=0.1),
              sliderInput("dge_minct", "Min Count Filter",    min=1,     max=20,   value=10,   step=1),
              tags$hr(),
              actionButton("dge_run", "Run DESeq2 Analysis",
                class="btn-primary", width="100%",
                style="font-size:14px;padding:10px;font-weight:600;"),
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
              column(3, uiOutput("dge_tile_total")),
              column(3, uiOutput("dge_tile_de")),
              column(3, uiOutput("dge_tile_up")),
              column(3, uiOutput("dge_tile_dn"))
            ),
            tags$br(),
            box(width=NULL, title="Results Preview (top genes by adjusted p-value)",
              DTOutput("dge_preview_table")
            )
          )
        )
      ),

      # -- TAB: PCA -----------------------------------------------
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
              sliderInput("pca_top_n", "Top variable genes", min=100, max=2000, value=500, step=100)
            )
          ),
          column(9,
            box(width=NULL, title="PCA Plot",
              plotlyOutput("dge_pca_plot", height="500px")
            )
          )
        )
      ),

      # -- TAB: Volcano -------------------------------------------
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
              sliderInput("volc_padj",  "Adj. P Cutoff",       min=0.001, max=0.1, value=0.05, step=0.001),
              sliderInput("volc_lfc",   "|Log2FC| Cutoff",     min=0.5,   max=3,   value=1,    step=0.1),
              sliderInput("volc_nlabs", "Top genes labelled",  min=0,     max=30,  value=10,   step=1),
              checkboxInput("volc_interactive", "Interactive (Plotly)", value=TRUE)
            )
          ),
          column(9,
            box(width=NULL, title="Volcano Plot",
              uiOutput("dge_volcano_ui")
            )
          )
        )
      ),

      # -- TAB: Distribution --------------------------------------
      tabItem(tabName = "dist_dge_tab",
        fluidRow(column(12,
          tags$h3(style="color:#1a73e8;font-weight:300;margin-bottom:20px;",
            icon("chart-area",style="margin-right:10px;"), "Distribution Analysis")
        )),
        fluidRow(
          column(3,
            box(width=NULL, title="Plot Controls",
              selectInput("dge_dist_type", "Plot type",
                choices = c(
                  "Normalised counts (violin)" = "violin",
                  "Log2FC distribution"        = "lfc_hist",
                  "P-value histogram"          = "pval_hist",
                  "MA plot"                    = "ma",
                  "Dispersion plot"            = "disp"
                ), selected="violin"),
              checkboxInput("dge_dist_log", "Log10 scale (violin)", value=TRUE)
            )
          ),
          column(9,
            box(width=NULL, title="Distribution Plot",
              plotlyOutput("dge_dist_plot", height="500px")
            )
          )
        )
      ),

      # -- TAB: DGE Results Table ---------------------------------
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
                          "Upregulated"="up","Downregulated"="dn"),
                selected="all")),
              column(4, downloadButton("dge_dl_csv", "Download CSV",
                style="margin-top:25px;width:100%;"))
            ),
            DTOutput("dge_full_table")
          )
        ))
      )

    ) # end tabItems
  )   # end dashboardBody
)     # end dashboardPage


# -- SERVER -------------------------------------------------------------------
server <- function(input, output, session) {

  # -- Pre-load pathview internal objects into globalenv on session start -------
  # This prevents "object not found" errors when pathview runs in Shiny's
  # reactive context, which has a different search path than a plain R session.
  local({
    if (!requireNamespace("pathview", quietly = TRUE)) return()

    safe_assign <- function(name) {
      if (exists(name, envir = globalenv(), inherits = FALSE)) return()
      # Try getFromNamespace (bods is NOT a data() dataset in recent pathview)
      val <- tryCatch(utils::getFromNamespace(name, "pathview"), error = function(e) NULL)
      # Hard-coded fallbacks for objects that cannot be found dynamically
      if (is.null(val) && name == "bods") {
        val <- data.frame(
          species  = c("Human","Mouse","Rat","Zebrafish","Fly","Worm","Yeast",
                       "Bovine","Canine","Chicken","Chimp","Rhesus","Pig",
                       "Arabidopsis","E coli K12","Malaria"),
          kegg.code = c("hsa","mmu","rno","dre","dme","cel","sce",
                        "bta","cfa","gga","ptr","mcc","ssc",
                        "ath","eco","pfa"),
          pkg.name = c("org.Hs.eg.db","org.Mm.eg.db","org.Rn.eg.db",
                       "org.Dr.eg.db","org.Dm.eg.db","org.Ce.eg.db",
                       "org.Sc.sgd.db","org.Bt.eg.db","org.Cf.eg.db",
                       "org.Gg.eg.db","org.Pt.eg.db","org.Mmu.eg.db",
                       "org.Ss.eg.db","org.At.tair.db","org.EcK12.eg.db",
                       "org.Pf.plasmo.db"),
          stringsAsFactors = FALSE
        )
      }
      if (is.null(val) && name == "KEGGEdgeSubtype") {
        val <- c("activation","inhibition","expression","repression",
                 "indirect effect","state change","binding/association",
                 "dissociation","missing interaction","phosphorylation",
                 "dephosphorylation","glycosylation","ubiquitination",
                 "methylation","compound","hidden compound")
        names(val) <- val
      }
      if (!is.null(val)) {
        # Assign to globalenv (pathview finds it via search path)
        assign(name, val, envir = globalenv())
        # Also inject into pathview namespace directly (overrides lazy-load promise)
        tryCatch({
          ns <- asNamespace("pathview")
          unlockBinding(name, ns)
          assign(name, val, envir = ns)
        }, error = function(e) NULL)
      }
    }

    safe_assign("bods")
    safe_assign("KEGGEdgeSubtype")
    safe_assign("rn.list")
  })

  # -- Reactive values ------------------------------------------------------
  rv <- reactiveValues(
    expr_data      = NULL,  # raw uploaded data
    gene_col       = NULL,
    fc_col         = NULL,
    converted_data = NULL,  # after ID conversion
    id_map         = NULL,
    unmapped       = NULL,
    pathview_imgs      = list(), # list: pathway_id -> file path
    pathway_genes      = list(), # list: pathway_id -> Entrez IDs in that pathway
    converted_id_type  = NULL,   # id_to type used in last conversion
    session_info       = NULL,
    conversion_done    = FALSE
  )

  # -- Helper: R color name -> hex string (for CSS use in DT formatStyle) ----
  col_to_hex <- function(col_name) {
    tryCatch({
      v <- col2rgb(col_name)
      sprintf("#%02X%02X%02X", v[1,1], v[2,1], v[3,1])
    }, error = function(e) "#000000")
  }

  # -- Helper: organism -> annotation package -------------------------------
  org_pkg <- function(org_code) {
    switch(org_code,
      "hsa" = "org.Hs.eg.db",
      "mmu" = "org.Mm.eg.db",
      "rno" = "org.Rn.eg.db",
      "dre" = "org.Dr.eg.db",
      "dme" = "org.Dm.eg.db",
      "cel" = "org.Ce.eg.db",
      "sce" = "org.Sc.sgd.db",
      "org.Hs.eg.db"
    )
  }

  # -- Example datasets -----------------------------------------------------
  make_example <- function(type) {
    set.seed(42)
    if (type == "brca") {
      genes <- c("TP53","BRCA1","BRCA2","MYC","EGFR","CDH1","PIK3CA","PTEN",
                 "RB1","CCND1","ESR1","ERBB2","AKT1","KRAS","VEGFA",
                 "CDK4","CDK6","MDM2","BCL2","BAX","CASP3","CASP9",
                 "CDKN1A","CDKN2A","E2F1","PCNA","MCM2","AURKA","PLK1","CDC20")
      data.frame(
        gene_symbol    = genes,
        log2FoldChange = round(rnorm(length(genes), 0, 2), 3),
        padj           = round(runif(length(genes), 0, 0.1), 5),
        baseMean       = round(runif(length(genes), 50, 5000), 1)
      )
    } else if (type == "lps") {
      genes <- c("ENSMUSG00000024190","ENSMUSG00000051747","ENSMUSG00000026395",
                 "ENSMUSG00000032012","ENSMUSG00000031764","ENSMUSG00000028037",
                 "ENSMUSG00000020108","ENSMUSG00000041498","ENSMUSG00000037944",
                 "ENSMUSG00000027852","ENSMUSG00000020122","ENSMUSG00000024401",
                 "ENSMUSG00000060550","ENSMUSG00000028028","ENSMUSG00000042396")
      data.frame(
        ensembl_id     = genes,
        log2FoldChange = round(rnorm(length(genes), 0, 2.5), 3),
        pvalue         = round(runif(length(genes), 0, 0.05), 6),
        padj           = round(runif(length(genes), 0, 0.1), 6)
      )
    } else if (type == "covid") {
      entrez <- c("7157","672","675","4609","1956","999","1029","5290","6597",
                  "10000","207","3845","7422","596","581","842","836","4616",
                  "3551","6261","2353","2354","6714","5970","5894")
      data.frame(
        entrez_id      = entrez,
        log2FoldChange = round(rnorm(length(entrez), 0, 2), 3),
        padj           = round(runif(length(entrez), 0, 0.05), 6)
      )
    }
  }

  # -- Load example ---------------------------------------------------------
  observeEvent(input$load_example, {
    req(input$example_data != "none")
    rv$expr_data         <- make_example(input$example_data)
    rv$conversion_done   <- FALSE
    rv$converted_data    <- NULL
    rv$converted_id_type <- NULL
    showNotification("Example dataset loaded.", type="message")
  })

  # -- File upload -----------------------------------------------------------
  observeEvent(input$expr_file, {
    req(input$expr_file)
    tryCatch({
      sep <- input$file_sep
      df  <- read.csv(input$expr_file$datapath, sep=sep,
                      header=input$has_header, stringsAsFactors=FALSE)
      rv$expr_data         <- df
      rv$conversion_done   <- FALSE
      rv$converted_data    <- NULL
      rv$converted_id_type <- NULL
      showNotification(paste0("Loaded ", nrow(df), " rows, ", ncol(df), " columns."), type="message")
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type="error")
    })
  })

  # -- Column mapping UI -----------------------------------------------------
  output$col_mapping_ui <- renderUI({
    req(rv$expr_data)
    cols <- colnames(rv$expr_data)
    tagList(
      selectInput("gene_col_sel", "Gene ID Column",
                  choices=cols, selected=cols[1]),
      selectInput("fc_col_sel",   "Fold-Change Column",
                  choices=cols,
                  selected=local({
                    # exclude anything that looks like a p-value
                    pval_pat <- "pval|padj|p\\.adj|p_adj|pvalue|p\\.value|fdr|qval"
                    non_pval <- cols[!grepl(pval_pat, cols, ignore.case=TRUE)]
                    fc_pat   <- "log2|fold|lfc"
                    fc_cols  <- non_pval[grepl(fc_pat, non_pval, ignore.case=TRUE)]
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

  # -- Data tiles ------------------------------------------------------------
  # -- Color swatch renderUI helpers -----------------------------
  r_named_colors <- list(
    list(n="navy",       h="#001f5b"), list(n="royalblue",   h="#4169E1"),
    list(n="blue",       h="#0000FF"), list(n="dodgerblue",  h="#1E90FF"),
    list(n="deepskyblue",h="#00BFFF"), list(n="steelblue",   h="#4682B4"),
    list(n="skyblue",    h="#87CEEB"), list(n="lightblue",   h="#ADD8E6"),
    list(n="cyan",       h="#00FFFF"), list(n="turquoise",   h="#40E0D0"),
    list(n="darkgreen",  h="#006400"), list(n="forestgreen", h="#228B22"),
    list(n="green",      h="#00FF00"), list(n="limegreen",   h="#32CD32"),
    list(n="chartreuse", h="#7FFF00"), list(n="yellowgreen", h="#9ACD32"),
    list(n="olivedrab",  h="#6B8E23"), list(n="springgreen", h="#00FF7F"),
    list(n="aquamarine", h="#7FFFD4"), list(n="palegreen",   h="#98FB98"),
    list(n="darkred",    h="#8B0000"), list(n="firebrick",   h="#B22222"),
    list(n="red",        h="#FF0000"), list(n="tomato",      h="#FF6347"),
    list(n="orangered",  h="#FF4500"), list(n="coral",       h="#FF7F50"),
    list(n="salmon",     h="#FA8072"), list(n="hotpink",     h="#FF69B4"),
    list(n="deeppink",   h="#FF1493"), list(n="pink",        h="#FFC0CB"),
    list(n="purple",     h="#800080"), list(n="darkviolet",  h="#9400D3"),
    list(n="magenta",    h="#FF00FF"), list(n="orchid",      h="#DA70D6"),
    list(n="plum",       h="#DDA0DD"), list(n="mediumpurple",h="#9370DB"),
    list(n="blueviolet", h="#8A2BE2"), list(n="indigo",      h="#4B0082"),
    list(n="violet",     h="#EE82EE"), list(n="lavender",    h="#E6E6FA"),
    list(n="gold",       h="#FFD700"), list(n="yellow",      h="#FFFF00"),
    list(n="orange",     h="#FFA500"), list(n="darkorange",  h="#FF8C00"),
    list(n="lightyellow",h="#FFFFE0"), list(n="khaki",       h="#F0E68C"),
    list(n="wheat",      h="#F5DEB3"), list(n="peachpuff",   h="#FFDAB9"),
    list(n="black",      h="#000000"), list(n="gray10",      h="#1A1A1A"),
    list(n="gray30",     h="#4D4D4D"), list(n="gray",        h="#808080"),
    list(n="gray70",     h="#B3B3B3"), list(n="gray90",      h="#E5E5E5"),
    list(n="white",      h="#FFFFFF"), list(n="whitesmoke",  h="#F5F5F5"),
    list(n="snow",       h="#FFFAFA"), list(n="ivory",       h="#FFFFF0")
  )

  make_swatch_ui <- function(input_id, label, default_name, default_hex) {
    cur_val <- tryCatch(
      if (!is.null(input[[input_id]]) && nchar(input[[input_id]]) > 0)
        input[[input_id]] else default_name,
      error = function(e) default_name
    )
    cur_hex <- tryCatch({
      matches <- Filter(function(c) c$n == cur_val, r_named_colors)
      if (length(matches) > 0) matches[[1]]$h else default_hex
    }, error = function(e) default_hex)

    tagList(
      tags$div(class="swatch-group",
        tags$div(class="swatch-label",
          tags$span(label),
          tags$span(id=paste0("preview_", input_id),
                    class="swatch-preview",
                    style=paste0("background-color:", cur_hex, ";")),
          tags$span(style="font-size:11px;color:#718096;font-family:'DM Mono',monospace;",
                    cur_val)
        ),
        tags$div(id=paste0("swatches_", input_id), class="swatch-grid")
      )
    )
  }

  output$swatch_low  <- renderUI({ make_swatch_ui("low_col",  "Low Color",  "blue", "#0000FF") })
  output$swatch_mid  <- renderUI({ make_swatch_ui("mid_col",  "Mid Color",  "gray", "#808080") })
  output$swatch_high <- renderUI({ make_swatch_ui("high_col", "High Color", "red",  "#FF0000") })

  make_tile <- function(val, lbl, col="#4fc3f7") {
    tags$div(class="info-tile",
      tags$div(class="tile-val", style=paste0("color:",col,";"), val),
      tags$div(class="tile-lbl", lbl)
    )
  }

  output$tile_genes <- renderUI({
    if(is.null(rv$expr_data)) return(make_tile(" - ","Genes"))
    make_tile(nrow(rv$expr_data), "Total Genes")
  })
  output$tile_up <- renderUI({
    if(is.null(rv$expr_data)||is.null(rv$fc_col)) return(make_tile(" - ","Up-regulated"))
    n <- sum(rv$expr_data[[rv$fc_col]] > 1, na.rm=TRUE)
    make_tile(n, "Up-regulated (|FC|>1)", "#ef5350")
  })
  output$tile_down <- renderUI({
    if(is.null(rv$expr_data)||is.null(rv$fc_col)) return(make_tile(" - ","Down-regulated"))
    n <- sum(rv$expr_data[[rv$fc_col]] < -1, na.rm=TRUE)
    make_tile(n, "Down-reg. (|FC|>1)", "#26a69a")
  })
  output$tile_sig <- renderUI({
    if(is.null(rv$expr_data)) return(make_tile(" - ","Significant"))
    if(!is.null(input$padj_col_sel) && input$padj_col_sel != "none" &&
       input$padj_col_sel %in% colnames(rv$expr_data)) {
      n <- sum(rv$expr_data[[input$padj_col_sel]] < 0.05, na.rm=TRUE)
      make_tile(n, "Sig. (padj<0.05)", "#ffa726")
    } else {
      make_tile("N/A", "Sig. (no p-val col)")
    }
  })

  # -- Expr table ------------------------------------------------------------
  output$expr_table <- renderDT({
    req(rv$expr_data)
    datatable(rv$expr_data, options=list(pageLength=10, scrollX=TRUE),
              rownames=FALSE, class="compact")
  })

  # -- Volcano ---------------------------------------------------------------
  output$volcano_plot <- renderPlotly({
    req(rv$expr_data, rv$fc_col)
    df <- rv$expr_data
    fc <- df[[rv$fc_col]]
    pval_col <- if(!is.null(input$padj_col_sel) && input$padj_col_sel!="none" &&
                    input$padj_col_sel %in% colnames(df)) input$padj_col_sel else NULL
    pv <- if(!is.null(pval_col)) -log10(df[[pval_col]] + 1e-300) else rep(0, nrow(df))
    color <- ifelse(fc > 1, "#ef5350", ifelse(fc < -1, "#26a69a", "#8899bb"))

    plot_ly(x=fc, y=pv, type="scatter", mode="markers",
            marker=list(color=color, size=5, opacity=0.7),
            text=if(!is.null(rv$gene_col)) df[[rv$gene_col]] else NULL,
            hoverinfo="text+x+y") %>%
      layout(
        paper_bgcolor="#fdfdf6", plot_bgcolor="#fdfaf6",
        font=list(color="#322b26", family="IBM Plex Mono"),
        xaxis=list(title="log2 Fold Change", gridcolor="#1a2a4a", zerolinecolor="#1e3a5f"),
        yaxis=list(title=if(!is.null(pval_col)) "-log10(padj)" else "Index",
                   gridcolor="#adaaa8"),
        shapes=list(
          list(type="line",x0=-1,x1=-1,y0=0,y1=max(pv,na.rm=TRUE),
               line=list(color="#26a69a",dash="dash",width=1)),
          list(type="line",x0=1,x1=1,y0=0,y1=max(pv,na.rm=TRUE),
               line=list(color="#ef5350",dash="dash",width=1))
        )
      )
  })

  # -- Distribution ---------------------------------------------------------
  output$dist_plot <- renderPlotly({
    req(rv$expr_data, rv$fc_col)
    fc <- rv$expr_data[[rv$fc_col]]
    plot_ly(x=fc, type="histogram",
            marker=list(color="#4fc3f7", line=list(color="#1e3a5f",width=0.5))) %>%
      layout(
        paper_bgcolor="#fdfdf6", plot_bgcolor="#fdfaf6",
        font=list(color="#322b26"),
        xaxis=list(title="log2 Fold Change", gridcolor="#1a2a4a"),
        yaxis=list(title="Count", gridcolor="#1a2a4a")
      )
  })

  # -- Sidebar status --------------------------------------------------------
  output$sidebar_status <- renderUI({
    # DESeq2 status helpers
    dge_dot <- switch(dge$status,
      idle    = "status-warn",
      running = "status-warn",
      done    = "status-ok",
      error   = "status-err",
      "status-warn"
    )
    dge_label <- switch(dge$status,
      idle    = "DESeq2: not run",
      running = "DESeq2: running...",
      done    = paste0("DESeq2: ",
                  if (!is.null(dge$res_df)) sum(dge$res_df$sig != "NS") else 0,
                  " DE genes"),
      error   = "DESeq2: error",
      "DESeq2: not run"
    )
    tags$div(
      # -- DESeq2 rows -------------------------------------------
      tags$div(class="section-divider", style="margin-bottom:6px;", "DGE"),
      tags$div(
        tags$span(class=paste0("status-dot ", dge_dot)),
        tags$span(style="color:#4a5568;font-size:12px;", dge_label)
      ),
      tags$div(style="margin-top:4px;",
        tags$span(class=paste0("status-dot ",
          if (!is.null(dge$res_df) && dge$status=="done") "status-ok" else "status-warn")),
        tags$span(style="color:#4a5568;font-size:12px;",
          if (!is.null(dge$res_df) && dge$status=="done")
            paste0(nrow(dge$res_df), " genes tested")
          else "No results yet")
      ),
      # -- PathView rows -----------------------------------------
      tags$div(class="section-divider", style="margin-top:8px;margin-bottom:6px;", "PathView"),
      tags$div(
        tags$span(class=paste0("status-dot ", if(!is.null(rv$expr_data)) "status-ok" else "status-warn")),
        tags$span(style="color:#4a5568;font-size:12px;",
          if(!is.null(rv$expr_data)) paste0(nrow(rv$expr_data)," genes loaded") else "No data loaded")
      ),
      tags$div(style="margin-top:4px;",
        tags$span(class=paste0("status-dot ", if(rv$conversion_done) "status-ok" else "status-warn")),
        tags$span(style="color:#4a5568;font-size:12px;",
          if(rv$conversion_done) "IDs converted" else "IDs not converted")
      ),
      tags$div(style="margin-top:4px;",
        tags$span(class=paste0("status-dot ", if(length(rv$pathview_imgs)>0) "status-ok" else "status-warn")),
        tags$span(style="color:#4a5568;font-size:12px;",
          if(length(rv$pathview_imgs)>0) paste0(length(rv$pathview_imgs)," plots ready") else "No plots yet")
      )
    )
  })

  # -- Package install UI ----------------------------------------------------
  output$pkg_install_ui <- renderUI({
    pkg <- org_pkg(input$organism)
    installed <- requireNamespace(pkg, quietly=TRUE) &&
                 requireNamespace("pathview", quietly=TRUE) &&
                 requireNamespace("AnnotationDbi", quietly=TRUE)
    if (!installed) {
      tags$div(class="alert alert-warning",
        icon("exclamation-triangle"), " ",
        tags$strong("Missing packages."), " Run in R:",
        tags$pre(style="font-size:10px;margin:4px 0 0;",
          paste0('BiocManager::install(c("pathview","AnnotationDbi","', pkg, '"))')
        )
      )
    } else {
      tags$div(class="alert alert-success",
        icon("check"), " Required packages installed.")
    }
  })

  # -- Run conversion --------------------------------------------------------
  observeEvent(input$run_conversion, {
    req(rv$expr_data, rv$gene_col, rv$fc_col)

    withProgress(message="Converting gene IDs...", value=0.3, {
      pkg <- org_pkg(input$organism)

      # Check packages
      pkgs_needed <- c("AnnotationDbi", pkg)
      missing_pkgs <- pkgs_needed[!sapply(pkgs_needed, requireNamespace, quietly=TRUE)]
      if (length(missing_pkgs) > 0) {
        showNotification(paste("Missing packages:", paste(missing_pkgs, collapse=", ")),
                         type="error", duration=10)
        return()
      }

      tryCatch({
        db     <- get(pkg, envir=loadNamespace(pkg))
        genes  <- as.character(rv$expr_data[[rv$gene_col]])
        fc_vec <- as.numeric(rv$expr_data[[rv$fc_col]])

        # Strip leading/trailing whitespace
        genes <- trimws(genes)

        # Strip ENSEMBL version suffixes e.g. ENSG00000012345.2 -> ENSG00000012345
        if (input$id_from %in% c("ENSEMBL", "ENSEMBLTRANS", "ENSEMBLPROT")) {
          genes <- sub("\\.\\d+$", "", genes)
        }

        setProgress(0.4, "Validating keys against database...")

        # Check that at least some keys are valid before querying
        valid_keys <- tryCatch(
          AnnotationDbi::keys(db, keytype = input$id_from),
          error = function(e) character(0)
        )
        n_valid <- sum(genes %in% valid_keys)
        if (n_valid == 0) {
          showNotification(
            paste0("None of your ", input$id_from, " IDs matched the ",
                   pkg, " database. ",
                   "Double-check organism and ID type. ",
                   "Example valid key: ", head(valid_keys, 1)),
            type = "error", duration = 20
          )
          return()
        }
        showNotification(
          paste0(n_valid, "/", length(genes), " IDs found in database. Converting..."),
          type = "message", duration = 5
        )

        setProgress(0.5, "Querying annotation database...")

        # Map IDs
        mapped <- AnnotationDbi::mapIds(
          db, keys=genes,
          column=input$id_to,
          keytype=input$id_from,
          multiVals=if(input$dup_method=="all") "list" else "first"
        )

        setProgress(0.8, "Processing results...")

        if (input$dup_method == "all") {
          # flatten list
          id_map <- data.frame(
            original_id = rep(genes, sapply(mapped, length)),
            converted_id = unlist(mapped),
            stringsAsFactors = FALSE
          )
        } else {
          id_map <- data.frame(
            original_id  = genes,
            converted_id = as.character(mapped),
            stringsAsFactors = FALSE
          )
        }

        # Join with FC
        df_orig <- data.frame(
          original_id    = genes,
          log2FoldChange = fc_vec,
          stringsAsFactors = FALSE
        )
        merged <- merge(df_orig, id_map, by="original_id", all.x=TRUE)

        if (input$dup_method == "maxfc") {
          merged <- merged[order(abs(merged$log2FoldChange), decreasing=TRUE),]
          merged <- merged[!duplicated(merged$converted_id),]
        }

        unmapped <- merged[is.na(merged$converted_id) | merged$converted_id=="NA", ]
        converted <- merged[!is.na(merged$converted_id) & merged$converted_id!="NA", ]

        rv$id_map            <- id_map
        rv$converted_data    <- converted
        rv$unmapped          <- unmapped
        rv$converted_id_type <- input$id_to
        rv$conversion_done   <- TRUE

        showNotification(
          paste0("Conversion complete: ", nrow(converted), " mapped, ",
                 nrow(unmapped), " unmapped."),
          type="message"
        )
      }, error = function(e) {
        showNotification(paste("Conversion error:", e$message), type="error", duration=15)
      })
    })
  })

  # -- Conversion summary ----------------------------------------------------
  output$conversion_summary <- renderUI({
    if (!rv$conversion_done) {
      return(tags$div(class="alert alert-info",
        icon("info-circle"), " Run conversion to see results."))
    }
    n_conv <- nrow(rv$converted_data)
    n_unm  <- nrow(rv$unmapped)
    pct    <- round(100 * n_conv / (n_conv + n_unm), 1)
    tags$div(
      fluidRow(
        column(4, tags$div(class="info-tile",
          tags$div(class="tile-val", n_conv), tags$div(class="tile-lbl","Mapped"))),
        column(4, tags$div(class="info-tile",
          tags$div(class="tile-val", style="color:#ffa726;", n_unm), tags$div(class="tile-lbl","Unmapped"))),
        column(4, tags$div(class="info-tile",
          tags$div(class="tile-val", style="color:#26a69a;", paste0(pct,"%")), tags$div(class="tile-lbl","Success Rate")))
      )
    )
  })

  output$converted_table <- renderDT({
    req(rv$converted_data)
    datatable(rv$converted_data, options=list(pageLength=10,scrollX=TRUE),
              rownames=FALSE, class="compact")
  })
  output$unmapped_table <- renderDT({
    req(rv$unmapped)
    datatable(rv$unmapped, options=list(pageLength=10,scrollX=TRUE),
              rownames=FALSE, class="compact")
  })
  output$id_map_table <- renderDT({
    req(rv$id_map)
    datatable(rv$id_map, options=list(pageLength=10,scrollX=TRUE),
              rownames=FALSE, class="compact")
  })

  # -- Use converted for PathView --------------------------------------------
  observeEvent(input$use_converted, {
    req(rv$converted_data)
    updateTabItems(session, "tabs", "pathview_tab")
    showNotification("Converted data ready for PathView.", type="message")
  })

  # -- Pathway ID search -----------------------------------------------------
  observeEvent(input$search_pathways, {
    withProgress(message="Fetching KEGG pathway list...", value=0.5, {
      tryCatch({
        if (!requireNamespace("KEGGREST", quietly=TRUE)) {
          showNotification("Install KEGGREST: BiocManager::install('KEGGREST')", type="error")
          return()
        }
        pathlist <- KEGGREST::keggList("pathway", input$organism_pv)
        rv$kegg_pathways <- data.frame(
          id   = sub("path:","",names(pathlist)),
          name = as.character(pathlist),
          stringsAsFactors = FALSE
        )
        showNotification(paste0("Fetched ", nrow(rv$kegg_pathways), " pathways for ", input$organism_pv),
                         type="message")
      }, error=function(e) {
        showNotification(paste("KEGG query error:", e$message), type="error")
      })
    })
  })

  output$pathway_search_ui <- renderUI({
    req(rv$kegg_pathways)
    df <- rv$kegg_pathways
    df$short_id <- sub(input$organism_pv, "", df$id)
    selectizeInput("add_pathway", "Add pathway from list",
                   choices = setNames(df$short_id, paste0(df$short_id,"  -  ",df$name)),
                   multiple = TRUE,
                   options  = list(maxItems=20, placeholder="Search pathway..."))
  })

  observeEvent(input$add_pathway, {
    req(input$add_pathway)
    current <- strsplit(trimws(input$pathway_ids), "[,\n\r ]+")[[1]]
    current <- current[nchar(current)>0]
    new_ids <- setdiff(input$add_pathway, current)
    if (length(new_ids) > 0) {
      updated <- paste(c(current, new_ids), collapse="\n")
      updateTextAreaInput(session, "pathway_ids", value=updated)
    }
  })

  # -- Helper: pathview with kegg.native auto-fallback ---------------------
  # Suppress pathview dir.create warnings (Windows)
  pv_warn_handler <- function(w) {
    if (grepl("cannot create dir", conditionMessage(w), fixed = TRUE)) {
      m <- regmatches(conditionMessage(w), regexpr("'[^']+'", conditionMessage(w)))
      if (length(m) > 0)
        dir.create(gsub("'", "", m), recursive = TRUE, showWarnings = FALSE)
      invokeRestart("muffleWarning")
    }
  }

  # 4-strategy pathview wrapper.
  # Strategy 2 (same.layer=FALSE) bypasses the node compositor that causes
  # most "subscript out of bounds" crashes on non-disease pathways like hsa04110.
  run_pathview_safe <- function(fc_named, pid, species, kegg_native,
                                fc_limit, low_col, mid_col, high_col) {
    pv_call <- function(native, same_layer) {
      withCallingHandlers(
        pathview::pathview(
          gene.data = fc_named, pathway.id = pid, species = species,
          kegg.native = native,
          limit  = list(gene = fc_limit, cpd = 1),
          low    = list(gene = low_col,  cpd = "blue"),
          mid    = list(gene = mid_col,  cpd = "gray"),
          high   = list(gene = high_col, cpd = "red"),
          out.suffix = "pathview", new.signature = FALSE, same.layer = same_layer
        ),
        warning = pv_warn_handler
      )
    }
    strategies <- list(
      list(native = kegg_native,  same_layer = TRUE,  note = NULL),
      list(native = kegg_native,  same_layer = FALSE, note = paste0(pid, ": used same.layer=FALSE.")),
      list(native = !kegg_native, same_layer = TRUE,  note = paste0(pid, ": switched render mode.")),
      list(native = !kegg_native, same_layer = FALSE, note = paste0(pid, ": last-resort render mode."))
    )
    last_err <- NULL
    for (s in strategies) {
      result <- tryCatch(pv_call(s$native, s$same_layer),
                         error = function(e) { last_err <<- e; NULL })
      if (!is.null(result)) {
        if (!is.null(s$note)) showNotification(s$note, type = "warning", duration = 10)
        return(result)
      }
    }
    stop(last_err)
  }



  # -- Run PathView ----------------------------------------------------------
  observeEvent(input$run_pathview, {

    # Determine data source
    use_converted <- rv$conversion_done && !is.null(rv$converted_data)
    expr_df <- if(use_converted) rv$converted_data else rv$expr_data

    req(expr_df)

    # -- BLOCK 1: Require gene ID conversion before running PathView ---------
    # PathView indexes all KEGG nodes by Entrez Gene ID internally.
    # Passing symbols, Ensembl IDs, or UniProt accessions causes
    # 'subscript out of bounds' because none of those exist in its tables.
    if (!use_converted) {
      showNotification(
        tags$div(
          tags$b("[!] Gene ID conversion required"),
          tags$br(),
          "PathView requires Entrez Gene IDs, but your data has not been converted yet.",
          tags$br(),
          "Go to ", tags$b("Gene ID Converter"),
          " -> set Input type to match your IDs (e.g. Gene Symbol) -> ",
          "Output type = ", tags$b("Entrez Gene ID"),
          " -> click ", tags$b("Run Conversion"), "."
        ),
        type = "error", duration = 20
      )
      return()
    }

    # -- BLOCK 2: Validate converted ID type is ENTREZID ---------------------
    actual_type <- rv$converted_id_type
    if (!is.null(actual_type) && actual_type != "ENTREZID") {
      showNotification(
        tags$div(
          tags$b("[!] Wrong ID type in converted data"),
          tags$br(),
          paste0("Your last conversion produced '", actual_type,
                 "' IDs. PathView requires Entrez Gene IDs."),
          tags$br(),
          "Go to Gene ID Converter -> set Output ID Type to ",
          tags$b("Entrez Gene ID"), " -> click Run Conversion again."
        ),
        type = "error", duration = 20
      )
      return()
    }

    # -- BLOCK 3: Detect p-value column used as fold-change ------------------
    # converted_data always uses log2FoldChange column, so this only fires
    # when running without conversion (already blocked above), but kept as
    # a safety net for future code paths.
    fc_col_check <- if (use_converted) "log2FoldChange" else rv$fc_col
    pval_name    <- grepl("pval|padj|p[._]adj|p_adj|pvalue|p[._]value|fdr|qval",
                          fc_col_check, ignore.case = TRUE)
    if (!use_converted && pval_name) {
      showNotification(
        tags$div(
          tags$b("[!] Wrong fold-change column selected"),
          tags$br(),
          paste0("Column '", fc_col_check, "' looks like a p-value column. "),
          "PathView colours genes by log2 fold-change. ",
          tags$br(),
          "Please select the correct fold-change column in ",
          tags$b("Data Input -> Fold-Change Column"), "."
        ),
        type = "error", duration = 20
      )
      return()
    }

    # -- Build named FC vector: Entrez IDs -> log2FC --------------------------
    # After conversion: converted_data columns are:
    #   original_id (symbol/ensembl) | log2FoldChange | converted_id (Entrez)
    fc_vec   <- as.numeric(rv$converted_data$log2FoldChange)
    gene_ids <- as.character(rv$converted_data$converted_id)

    fc_named <- setNames(fc_vec, gene_ids)
    fc_named <- fc_named[!is.na(names(fc_named)) & names(fc_named) != "NA"]
    fc_named <- fc_named[!is.na(fc_named)]
    fc_named <- fc_named[!duplicated(names(fc_named))]

    # Parse pathway IDs
    raw_ids <- strsplit(trimws(input$pathway_ids), "[,\n\r ]+")[[1]]
    raw_ids <- raw_ids[nchar(trimws(raw_ids)) > 0]
    if (length(raw_ids) == 0) {
      showNotification("Please enter at least one pathway ID.", type="error"); return()
    }
    # Prepend organism code if missing
    pathway_ids <- sapply(raw_ids, function(x) {
      if (!grepl("^[a-z]{3}", x)) paste0(input$organism_pv, x) else x
    })

    if (!requireNamespace("pathview", quietly=TRUE)) {
      showNotification("pathview not installed. Run: BiocManager::install('pathview')",
                       type="error", duration=10)
      return()
    }

    # Temp dir for outputs
    out_dir <- file.path(tempdir(), paste0("pathview_", as.integer(Sys.time())))
    dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)

    img_list          <- list()
    pathway_gene_list <- list()
    n <- length(pathway_ids)

    withProgress(message="Generating PathView plots...", value=0, {
      for (i in seq_along(pathway_ids)) {
        pid <- pathway_ids[i]
        setProgress(i/n, detail=paste("Pathway:", pid))

        tryCatch({
          old_wd <- getwd()
          setwd(out_dir)
          on.exit(setwd(old_wd), add=TRUE)

          local({
            ensure_pv_obj <- function(name) {
              if (exists(name, envir = globalenv(), inherits = FALSE)) return(invisible())
              val <- tryCatch(utils::getFromNamespace(name, "pathview"), error = function(e) NULL)
              if (is.null(val) && name == "bods") {
                val <- data.frame(
                  species   = c("Human","Mouse","Rat","Zebrafish","Fly","Worm","Yeast",
                                "Bovine","Canine","Chicken","Chimp","Rhesus","Pig",
                                "Arabidopsis","E coli K12","Malaria"),
                  kegg.code = c("hsa","mmu","rno","dre","dme","cel","sce",
                                "bta","cfa","gga","ptr","mcc","ssc",
                                "ath","eco","pfa"),
                  pkg.name  = c("org.Hs.eg.db","org.Mm.eg.db","org.Rn.eg.db",
                                "org.Dr.eg.db","org.Dm.eg.db","org.Ce.eg.db",
                                "org.Sc.sgd.db","org.Bt.eg.db","org.Cf.eg.db",
                                "org.Gg.eg.db","org.Pt.eg.db","org.Mmu.eg.db",
                                "org.Ss.eg.db","org.At.tair.db","org.EcK12.eg.db",
                                "org.Pf.plasmo.db"),
                  stringsAsFactors = FALSE
                )
              }
              if (is.null(val) && name == "KEGGEdgeSubtype") {
                val <- c("activation","inhibition","expression","repression",
                         "indirect effect","state change","binding/association",
                         "dissociation","missing interaction","phosphorylation",
                         "dephosphorylation","glycosylation","ubiquitination",
                         "methylation","compound","hidden compound")
                names(val) <- val
              }
              if (!is.null(val)) {
        # Assign to globalenv (pathview finds it via search path)
        assign(name, val, envir = globalenv())
        # Also inject into pathview namespace directly (overrides lazy-load promise)
        tryCatch({
          ns <- asNamespace("pathview")
          unlockBinding(name, ns)
          assign(name, val, envir = ns)
        }, error = function(e) NULL)
      }
            }
            ensure_pv_obj("bods")
            ensure_pv_obj("KEGGEdgeSubtype")
            ensure_pv_obj("rn.list")
          })

          pv_result <- run_pathview_safe(
            fc_named=fc_named, pid=pid, species=input$organism_pv,
            kegg_native=as.logical(input$kegg_native),
            fc_limit=input$fc_limit, low_col=input$low_col,
            mid_col=input$mid_col,   high_col=input$high_col
          )
          # keggLink returns named vector: NAMES = gene IDs ("hsa:1029"),
          # VALUES = pathway IDs ("path:hsa04110"). Extract genes from names().
          pathway_entrez <- tryCatch({
            links <- KEGGREST::keggLink(input$organism_pv, paste0("path:", pid))
            unique(sub("^[a-z]+:", "", names(links)))
          }, error = function(e) {
            tryCatch({
              pd <- pv_result[[pid]]$plot.data.gene
              if (is.null(pd)) {
                short_pid <- sub("^[a-z]+", "", pid)
                pd <- pv_result[[short_pid]]$plot.data.gene
              }
              if (!is.null(pd) && "kegg.names" %in% colnames(pd))
                unique(as.character(pd$kegg.names[!is.na(pd$kegg.names)]))
              else character(0)
            }, error = function(e2) character(0))
          })
          pathway_gene_list[[pid]] <- pathway_entrez
          setwd(old_wd)

          # Find output PNG
          pngs <- list.files(out_dir, pattern=paste0(pid,".*\\.png$"), full.names=TRUE)
          if (length(pngs) > 0) {
            img_list[[pid]] <- pngs[1]
          } else {
            # Try PDF
            pdfs <- list.files(out_dir, pattern=paste0(pid,".*\\.(pdf|png)$"), full.names=TRUE)
            if (length(pdfs)>0) img_list[[pid]] <- pdfs[1]
          }
        }, error = function(e) {
          showNotification(paste0("Error on ", pid, ": ", e$message),
                           type="warning", duration=8)
        })
      }
    })

    rv$pathview_imgs   <- img_list
    rv$pathway_genes   <- pathway_gene_list
    rv$pathview_outdir <- out_dir

    if (length(img_list) > 0) {
      showNotification(paste0(length(img_list), " PathView plot(s) generated!"), type="message")
    } else {
      showNotification("No plots generated. Check pathway IDs and organism.", type="error")
    }
  })

  # -- Gene panel: genes in the currently-selected pathway --------------------
  pv_gene_df <- reactive({
    req(input$selected_pathway, rv$conversion_done, rv$converted_data)

    df <- data.frame(
      EntrezID = as.character(rv$converted_data$converted_id),
      Symbol   = as.character(rv$converted_data$original_id),
      Log2FC   = as.numeric(rv$converted_data$log2FoldChange),
      stringsAsFactors = FALSE
    )
    df <- df[!is.na(df$Log2FC) & !is.na(df$EntrezID) & df$EntrezID != "NA", ]
    df$Symbol[is.na(df$Symbol) | df$Symbol == "NA" | df$Symbol == ""] <-
      df$EntrezID[is.na(df$Symbol) | df$Symbol == "NA" | df$Symbol == ""]

    # Filter to only the genes belonging to the selected pathway
    pathway_entrez <- rv$pathway_genes[[input$selected_pathway]]
    if (!is.null(pathway_entrez) && length(pathway_entrez) > 0)
      df <- df[df$EntrezID %in% pathway_entrez, ]

    if (nrow(df) == 0) return(NULL)

    lim <- if (!is.null(input$fc_limit)) input$fc_limit else 1
    df$Direction <- ifelse(df$Log2FC >=  lim, "Up",
                    ifelse(df$Log2FC <= -lim, "Down", "Within limit"))
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
    n_up <- sum(df$Direction == "Up")
    n_dn <- sum(df$Direction == "Down")
    col_up <- col_to_hex(if (!is.null(input$high_col) && nchar(input$high_col) > 0) input$high_col else "red")
    col_dn <- col_to_hex(if (!is.null(input$low_col)  && nchar(input$low_col)  > 0) input$low_col  else "blue")
    tags$div(
      fluidRow(
        column(6, tags$div(class="info-tile",
          tags$div(class="tile-val", style=paste0("color:", col_up, ";font-size:20px;"), n_up),
          tags$div(class="tile-lbl", "Up"))),
        column(6, tags$div(class="info-tile",
          tags$div(class="tile-val", style=paste0("color:", col_dn, ";font-size:20px;"), n_dn),
          tags$div(class="tile-lbl", "Down")))
      )
    )
  })

  output$pv_up_table <- renderDT({
    df <- pv_gene_df()
    req(!is.null(df))
    col_up <- col_to_hex(if (!is.null(input$high_col) && nchar(input$high_col) > 0) input$high_col else "red")
    up <- df[df$Direction == "Up", c("Symbol","EntrezID","Log2FC"), drop=FALSE]
    if (nrow(up) == 0) return(datatable(
      data.frame(Symbol=character(), EntrezID=character(), Log2FC=numeric()),
      options=list(dom="t"), rownames=FALSE, colnames=c("Symbol","Entrez ID","Log2FC")))
    datatable(up,
      options=list(pageLength=8, dom="tp", scrollX=TRUE,
                   order=list(list(2,"desc")),
                   columnDefs=list(list(width="75px",targets=0),
                                   list(width="65px",targets=1))),
      rownames=FALSE, class="compact", colnames=c("Symbol","Entrez ID","Log2FC")) |>
      formatStyle("Log2FC", color=col_up, fontWeight="bold") |>
      formatStyle("Symbol", fontWeight="600", color="#2d3748")
  })

  output$pv_dn_table <- renderDT({
    df <- pv_gene_df()
    req(!is.null(df))
    col_dn <- col_to_hex(if (!is.null(input$low_col) && nchar(input$low_col) > 0) input$low_col else "blue")
    dn <- df[df$Direction == "Down", c("Symbol","EntrezID","Log2FC"), drop=FALSE]
    if (nrow(dn) == 0) return(datatable(
      data.frame(Symbol=character(), EntrezID=character(), Log2FC=numeric()),
      options=list(dom="t"), rownames=FALSE, colnames=c("Symbol","Entrez ID","Log2FC")))
    datatable(dn,
      options=list(pageLength=8, dom="tp", scrollX=TRUE,
                   order=list(list(2,"asc")),
                   columnDefs=list(list(width="75px",targets=0),
                                   list(width="65px",targets=1))),
      rownames=FALSE, class="compact", colnames=c("Symbol","Entrez ID","Log2FC")) |>
      formatStyle("Log2FC", color=col_dn, fontWeight="bold") |>
      formatStyle("Symbol", fontWeight="600", color="#2d3748")
  })

  output$dl_pv_genes <- downloadHandler(
    filename = function() paste0("pathway_genes_", input$selected_pathway, "_",
                                 Sys.Date(), ".csv"),
    content  = function(file) {
      df <- pv_gene_df()
      req(df)
      write.csv(df, file, row.names=FALSE)
    }
  )

  # -- Pathway plot display --------------------------------------------------
  output$pathview_status <- renderUI({
    if (length(rv$pathview_imgs) == 0) {
      tags$div(class="alert alert-info",
        icon("info-circle"), " ",
        if (!rv$conversion_done)
          "Tip: Convert gene IDs first (Gene ID Converter tab), then generate plots."
        else
          "Configure settings and click 'Generate PathView Plots'."
      )
    } else {
      tags$div(class="alert alert-success",
        icon("check-circle"), " ",
        paste0(length(rv$pathview_imgs), " pathway plot(s) ready.")
      )
    }
  })

  output$pathway_selector_ui <- renderUI({
    req(length(rv$pathview_imgs) > 0)
    selectInput("selected_pathway", "Select Pathway to View",
                choices = names(rv$pathview_imgs), width="100%")
  })

  output$pathway_plot_display <- renderUI({
    req(input$selected_pathway, rv$pathview_imgs[[input$selected_pathway]])
    img_path <- rv$pathview_imgs[[input$selected_pathway]]

    if (!file.exists(img_path)) {
      return(tags$div(class="alert alert-danger", "Plot file not found: ", img_path))
    }

    # Encode as base64
    img_data <- base64enc::base64encode(img_path)
    ext <- tolower(tools::file_ext(img_path))
    mime <- if(ext=="pdf") "application/pdf" else paste0("image/",ext)

    if (ext == "png" || ext == "jpg" || ext == "jpeg") {
      tags$div(id="pathway_plot_display",
        tags$img(src=paste0("data:", mime, ";base64,", img_data),
                 style="max-width:100%;border:1px solid #1e3a5f;border-radius:4px;"),
        tags$p(style="color:#8899bb;font-size:11px;margin-top:6px;",
          icon("map-marker"), " Pathway: ", input$selected_pathway,
          " | File: ", basename(img_path))
      )
    } else {
      tags$div(class="alert alert-warning",
        "Plot generated as PDF. Download below.",
        tags$br(), tags$code(img_path))
    }
  })

  # -- Downloads -------------------------------------------------------------
  output$dl_converted <- downloadHandler(
    filename = function() paste0("converted_ids_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(rv$converted_data)
      write.csv(rv$converted_data, file, row.names=FALSE)
    }
  )

  output$dl_current_plot <- downloadHandler(
    filename = function() paste0(input$selected_pathway, "_pathview.png"),
    content  = function(file) {
      req(input$selected_pathway)
      img_path <- rv$pathview_imgs[[input$selected_pathway]]
      req(file.exists(img_path))
      file.copy(img_path, file)
    }
  )

  output$dl_all_plots <- downloadHandler(
    filename = function() paste0("pathview_plots_", Sys.Date(), ".zip"),
    content  = function(file) {
      req(length(rv$pathview_imgs) > 0)
      imgs <- unlist(rv$pathview_imgs)
      imgs <- imgs[file.exists(imgs)]
      if (requireNamespace("zip", quietly=TRUE)) {
        zip::zip(file, files=imgs, mode="cherry-pick")
      } else {
        zip(file, files=imgs, flags="-j")
      }
    }
  )

  output$dl_expr_processed <- downloadHandler(
    filename = function() paste0("expression_processed_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- if(!is.null(rv$converted_data)) rv$converted_data else rv$expr_data
      req(df); write.csv(df, file, row.names=FALSE)
    }
  )

  output$dl_conversion_map <- downloadHandler(
    filename = function() paste0("id_conversion_map_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(rv$id_map); write.csv(rv$id_map, file, row.names=FALSE)
    }
  )

  output$dl_all_zip <- downloadHandler(
    filename = function() paste0("pathview_all_", Sys.Date(), ".zip"),
    content  = function(file) {
      req(length(rv$pathview_imgs) > 0)
      imgs <- unlist(rv$pathview_imgs); imgs <- imgs[file.exists(imgs)]
      if (requireNamespace("zip", quietly=TRUE)) {
        zip::zip(file, files=imgs, mode="cherry-pick")
      } else {
        zip(file, files=imgs, flags="-j")
      }
    }
  )

  # -- Results summary -------------------------------------------------------
  output$results_summary <- renderUI({
    tags$div(style="color:#c8d8f0;font-size:13px;",
      tags$table(style="width:100%;",
        tags$tr(
          tags$td(style="color:#8899bb;padding:4px 0;","Genes loaded"),
          tags$td(style="color:#4fc3f7;font-family:'IBM Plex Mono';",
            if(!is.null(rv$expr_data)) nrow(rv$expr_data) else " - ")
        ),
        tags$tr(
          tags$td(style="color:#8899bb;padding:4px 0;","ID conversion"),
          tags$td(style="color:#4fc3f7;", if(rv$conversion_done) "[OK] Complete" else "[X] Not run")
        ),
        tags$tr(
          tags$td(style="color:#8899bb;padding:4px 0;","Genes converted"),
          tags$td(style="color:#4fc3f7;font-family:'IBM Plex Mono';",
            if(!is.null(rv$converted_data)) nrow(rv$converted_data) else " - ")
        ),
        tags$tr(
          tags$td(style="color:#8899bb;padding:4px 0;","PathView plots"),
          tags$td(style="color:#4fc3f7;font-family:'IBM Plex Mono';",
            length(rv$pathview_imgs))
        ),
        tags$tr(
          tags$td(style="color:#8899bb;padding:4px 0;","Organism"),
          tags$td(style="color:#4fc3f7;", input$organism_pv)
        )
      )
    )
  })

  # -- Session info ----------------------------------------------------------
  observeEvent(input$show_session, {
    rv$session_info <- capture.output(sessionInfo())
  })
  output$session_info_out <- renderText({
    req(rv$session_info)
    paste(rv$session_info, collapse="\n")
  })

  # ============================================================
  # DESeq2 MODULE SERVER
  # ============================================================

  # Helper: generate synthetic demo count data
  make_deseq2_demo <- function(n_genes=2000, n_samp=6) {
    set.seed(42)
    gids <- paste0("Gene_", sprintf("%04d", seq_len(n_genes)))
    sids <- c(paste0("Ctrl_", 1:3), paste0("Trt_", 1:3))
    base <- rnbinom(n_genes, mu=100, size=2)
    mat  <- matrix(0L, n_genes, n_samp, dimnames=list(gids, sids))
    for (i in seq_len(n_samp))
      mat[, i] <- rnbinom(n_genes, mu=base * runif(1, 0.8, 1.2), size=5)
    de  <- sample(n_genes, 200)
    for (i in 4:6) {
      mat[de[1:100],   i] <- rnbinom(100, mu=base[de[1:100]]   * runif(100,3,8),   size=5)
      mat[de[101:200], i] <- rnbinom(100, mu=base[de[101:200]] * runif(100,0.1,0.3),size=5)
    }
    cd <- data.frame(
      sample    = sids,
      condition = factor(c(rep("Control",3), rep("Treatment",3)),
                         levels=c("Control","Treatment")),
      row.names = sids
    )
    list(counts=mat, coldata=cd)
  }

  # DESeq2 reactive store
  dge <- reactiveValues(
    dds         = NULL,
    res_df      = NULL,   # default contrast results
    all_results = NULL,   # named list: contrast -> res data.frame
    contrasts   = NULL,   # character vector of contrast names
    vst_mat     = NULL,
    coldata     = NULL,
    status      = "idle"
  )

  # Status badge
  output$dge_status_ui <- renderUI({
    switch(dge$status,
      idle    = tags$div(class="alert alert-info",
                  icon("info-circle"), " Ready  -  click Run DESeq2 Analysis."),
      running = tags$div(class="alert alert-warning",
                  icon("hourglass-half"), " Running DESeq2, please wait..."),
      done    = tags$div(class="alert alert-success",
                  icon("check-circle"), " Analysis complete."),
      error   = tags$div(class="alert alert-danger",
                  icon("exclamation-circle"), " Error  -  see R console.")
    )
  })

  # -- Contrast selector UIs -----------------------------------
  contrast_select <- function(input_id, label) {
    if (is.null(dge$contrasts) || length(dge$contrasts) == 0)
      return(tags$div(class="alert alert-info",
        style="font-size:11px;padding:6px;",
        icon("info-circle"), " Run DESeq2 to enable contrast selection."))
    selectInput(input_id, label,
      choices  = setNames(dge$contrasts,
                          gsub("_vs_", " -> ", dge$contrasts)),
      selected = isolate(input[[input_id]]) %||% dge$contrasts[1],
      width    = "100%")
  }
  # %||% helper
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && a != "") a else b

  output$volc_contrast_ui <- renderUI({
    contrast_select("volc_contrast", "Contrast (numerator -> denominator)")
  })
  output$tbl_contrast_ui <- renderUI({
    contrast_select("tbl_contrast", "Contrast")
  })

  # -- Reactive: get results for the chosen contrast -----------
  active_contrast_df <- function(contrast_input_id) {
    req(dge$all_results)
    sel <- input[[contrast_input_id]]
    if (is.null(sel) || !sel %in% names(dge$all_results))
      return(dge$all_results[[1]])
    dge$all_results[[sel]]
  }

  # Tile helper
  dge_tile <- function(val, lbl, col="#1a73e8") {
    tags$div(class="info-tile",
      tags$div(class="tile-val", style=paste0("color:",col,";"), val),
      tags$div(class="tile-lbl", lbl))
  }
  output$dge_tile_total <- renderUI(dge_tile(
    if(!is.null(dge$res_df)) nrow(dge$res_df) else "-", "Genes Tested"))
  output$dge_tile_de <- renderUI(dge_tile(
    if(!is.null(dge$res_df)) sum(dge$res_df$sig!="NS") else "-",
    "Significant", "#f4a433"))
  output$dge_tile_up <- renderUI(dge_tile(
    if(!is.null(dge$res_df)) sum(dge$res_df$sig=="Up") else "-",
    "Upregulated", "#ef5350"))
  output$dge_tile_dn <- renderUI(dge_tile(
    if(!is.null(dge$res_df)) sum(dge$res_df$sig=="Down") else "-",
    "Downregulated", "#26a69a"))

  # Run DESeq2
  observeEvent(input$dge_run, {
    needed <- c("DESeq2","matrixStats","ggrepel","tibble","tidyr")
    missing_p <- needed[!sapply(needed, requireNamespace, quietly=TRUE)]
    if (length(missing_p) > 0) {
      showNotification(
        paste0("Missing packages: ", paste(missing_p, collapse=", "),
               ". Run install_packages.R first."),
        type="error", duration=15)
      return()
    }
    shinyjs::show("dge_sidebar_running")
    shinyjs::html("dge_status_div", '<div class="alert alert-warning" style="font-size:12px;padding:8px;"><i class="fa fa-hourglass-half"></i> Running DESeq2, please wait...</div>')
    dge$status <- "running"
    withProgress(message = "Running DESeq2...", value = 0, {
    tryCatch({
      setProgress(0.05, detail = "Loading data...")
      dat <- if (input$dge_source == "demo") {
        make_deseq2_demo()
      } else {
        req(input$dge_count_file, input$dge_meta_file)
        # -- Read count matrix WITHOUT row.names to avoid duplicate error --
        cm_sep <- input$dge_count_sep
        cm_raw <- read.table(input$dge_count_file$datapath,
                             sep = cm_sep,
                             header = isTRUE(input$dge_count_header),
                             row.names = NULL,
                             check.names = FALSE,
                             stringsAsFactors = FALSE)
        # First column = gene IDs
        gene_ids <- as.character(cm_raw[[1]])
        cm <- as.matrix(cm_raw[, -1, drop=FALSE])
        rownames(cm) <- gene_ids

        # -- Collapse duplicate gene names by mean (before anything else) --
        n_dups <- sum(duplicated(gene_ids))
        if (n_dups > 0) {
          uniq_genes <- unique(gene_ids)
          cm <- do.call(rbind, lapply(uniq_genes, function(g) {
            rows <- cm[rownames(cm) == g, , drop=FALSE]
            round(colMeans(rows))
          }))
          rownames(cm) <- uniq_genes
          showNotification(
            paste0("Collapsed ", n_dups, " duplicate gene name(s) by row mean."),
            type = "warning", duration = 8)
        }

        # -- Read metadata -----------------------------------------
        md_sep <- input$dge_meta_sep
        md <- read.table(input$dge_meta_file$datapath,
                         sep = md_sep,
                         header = TRUE,
                         stringsAsFactors = FALSE)
        if ("sample" %in% colnames(md)) {
          rownames(md) <- md$sample
        } else {
          rownames(md) <- md[[1]]
          if (!"condition" %in% colnames(md))
            colnames(md)[2] <- "condition"
        }
        # reorder to match
        shared <- intersect(colnames(cm), rownames(md))
        if (length(shared) < 2)
          stop("Sample names in count matrix columns do not match metadata rows. ",
               "Check delimiter selection and that sample names match exactly.")
        cm <- cm[, shared, drop=FALSE]
        md <- md[shared, , drop=FALSE]
        md$condition <- factor(md$condition)
        list(counts=cm, coldata=md)
      }
      setProgress(0.15, detail = "Processing count matrix...")
      cts <- dat$counts
      cd  <- dat$coldata

      # -- Collapse duplicates in demo data (safety net) ----------
      if (anyDuplicated(rownames(cts))) {
        uniq_genes <- unique(rownames(cts))
        cts <- do.call(rbind, lapply(uniq_genes, function(g) {
          rows <- cts[rownames(cts) == g, , drop=FALSE]
          round(colMeans(rows))
        }))
        rownames(cts) <- uniq_genes
      }

      # -- Coerce to numeric and remove NA values -----------------
      mode(cts) <- "numeric"
      n_na_genes <- sum(apply(cts, 1, function(r) any(is.na(r))))
      if (n_na_genes > 0) {
        cts <- cts[apply(cts, 1, function(r) !any(is.na(r))), , drop=FALSE]
        showNotification(
          paste0("Removed ", n_na_genes, " gene(s) with NA counts."),
          type = "warning", duration = 8)
      }
      n_na_cols <- sum(apply(cts, 2, function(c) any(is.na(c))))
      if (n_na_cols > 0)
        stop(paste0(n_na_cols, " sample column(s) still contain NA values after ",
                    "gene filtering. Check your input file for missing data."))

      # -- Handle negative values ----------------------------------
      # DESeq2 requires raw non-negative integer counts.
      # Negative values usually mean the file contains log-transformed
      # or normalised data rather than raw counts.
      n_neg <- sum(cts < 0, na.rm = TRUE)
      if (n_neg > 0) {
        # Check if the whole matrix looks log-transformed (many negatives / decimals)
        frac_neg <- n_neg / length(cts)
        has_decimals <- any(cts != floor(cts), na.rm = TRUE)
        if (frac_neg > 0.01 || has_decimals) {
          # Looks like log-transformed data  -  back-transform with 2^x then floor
          showNotification(
            paste0("Detected ", n_neg, " negative value(s)  -  data may be log-transformed. ",
                   "Applying 2^x back-transformation and rounding to integers."),
            type = "warning", duration = 12)
          cts <- floor(2^cts)
        } else {
          # Only a few negatives  -  likely artefacts, clamp to 0
          showNotification(
            paste0("Clamped ", n_neg, " negative count value(s) to 0."),
            type = "warning", duration = 8)
          cts[cts < 0] <- 0
        }
      }

      cts <- cts[rowSums(cts) >= input$dge_minct, , drop=FALSE]

      setProgress(0.4, detail = "Fitting DESeq2 model...")
      dds <- DESeq2::DESeqDataSetFromMatrix(
        countData = round(cts), colData = cd, design = ~condition)
      dds <- DESeq2::DESeq(dds, quiet=TRUE)
      setProgress(0.8, detail = "Extracting results...")
      # Build all pairwise contrasts from condition levels
      cond_levels <- levels(cd$condition)
      contrast_pairs <- combn(cond_levels, 2, simplify = FALSE)
      all_res_list <- list()
      for (cp in contrast_pairs) {
        cname <- paste0(cp[2], "_vs_", cp[1])
        r <- tryCatch(
          DESeq2::results(dds, contrast = c("condition", cp[2], cp[1]),
                          alpha = input$dge_padj),
          error = function(e) NULL)
        if (!is.null(r)) {
          rr <- as.data.frame(r)
          rr$gene <- rownames(rr)
          rr$contrast <- cname
          rr$sig <- with(rr, ifelse(
            !is.na(padj) & padj < input$dge_padj & log2FoldChange >  input$dge_lfc, "Up",
            ifelse(!is.na(padj) & padj < input$dge_padj & log2FoldChange < -input$dge_lfc,
                   "Down", "NS")))
          rr$neg_log10_padj <- -log10(pmax(rr$padj, 1e-300))
          all_res_list[[cname]] <- rr
        }
      }
      # Default contrast = first pair
      default_contrast <- names(all_res_list)[1]
      rdf <- all_res_list[[default_contrast]]

      vst_obj <- DESeq2::varianceStabilizingTransformation(dds, blind=TRUE)
      vst <- SummarizedExperiment::assay(vst_obj)

      setProgress(0.95, detail = "Storing results...")
      dge$dds         <- dds
      dge$res_df      <- rdf
      dge$all_results <- all_res_list
      dge$contrasts   <- names(all_res_list)
      dge$vst_mat     <- vst
      dge$coldata     <- cd
      dge$status      <- "done"
      # Populate contrast selectors
      updateSelectInput(session, "volc_contrast",   choices = names(all_res_list), selected = default_contrast)
      updateSelectInput(session, "tbl_contrast",    choices = names(all_res_list), selected = default_contrast)
      n_sig <- sum(rdf$sig != "NS")
      n_up  <- sum(rdf$sig == "Up")
      n_dn  <- sum(rdf$sig == "Down")
      setProgress(1, detail = "Done.")
      shinyjs::hide("dge_sidebar_running")
      shinyjs::html("dge_status_div", paste0(
        '<div class="alert alert-success" style="font-size:12px;padding:8px;">',
        '<i class="fa fa-check-circle"></i> Done: ', n_sig, ' significant genes (',
        n_up, ' up / ', n_dn, ' down).</div>'))
      showNotification(
        paste0("DESeq2 complete  -  ", n_sig, " significant genes found."),
        type="message", duration=8)
    }, error=function(e) {
      dge$status <- "error"
      shinyjs::hide("dge_sidebar_running")
      shinyjs::html("dge_status_div", paste0(
        '<div class="alert alert-danger" style="font-size:12px;padding:8px;">',
        '<i class="fa fa-exclamation-circle"></i> Error: ', e$message, '</div>'))
      showNotification(paste("DESeq2 error:", e$message), type="error", duration=20)
    })
    }) # end withProgress
  })

  # Send to PathView
  output$dge_pathview_msg <- renderUI(NULL)
  observeEvent(input$dge_to_pathview, {
    req(dge$res_df)
    df_pv <- dge$res_df[!is.na(dge$res_df$log2FoldChange),
                         c("gene","log2FoldChange")]
    colnames(df_pv)[1] <- "gene_id"
    rv$expr_data       <- df_pv
    rv$gene_col        <- "gene_id"
    rv$fc_col          <- "log2FoldChange"
    rv$conversion_done <- FALSE
    rv$converted_data  <- NULL
    output$dge_pathview_msg <- renderUI(
      tags$div(class="alert alert-success",
        icon("check"), " Sent! Go to Gene ID Converter then PathView Plots."))
    showNotification("DESeq2 results sent to PathView pipeline.", type="message")
    updateTabItems(session, "tabs", "convert_tab")
  })

  # Preview table
  output$dge_preview_table <- renderDT({
    req(dge$res_df)
    df <- dge$res_df[order(dge$res_df$padj, na.last=TRUE),
                     c("gene","baseMean","log2FoldChange","lfcSE","pvalue","padj","sig")]
    df[,2:6] <- lapply(df[,2:6], function(x) signif(x, 4))
    datatable(df, options=list(pageLength=10, scrollX=TRUE),
              rownames=FALSE, class="compact") |>
      formatStyle("sig",
        backgroundColor = styleEqual(c("Up","Down","NS"),
          c("rgba(239,83,80,0.12)","rgba(38,166,154,0.12)","transparent")),
        color = styleEqual(c("Up","Down","NS"), c("#ef5350","#26a69a","#718096")),
        fontWeight = "bold")
  })

  # PCA
  output$dge_pca_plot <- renderPlotly({
    req(dge$vst_mat, dge$coldata)
    if (!requireNamespace("matrixStats", quietly=TRUE)) return(NULL)
    mat   <- dge$vst_mat
    top_n <- min(input$pca_top_n, nrow(mat))
    rv_g  <- matrixStats::rowVars(mat)
    mat   <- mat[order(rv_g, decreasing=TRUE)[seq_len(top_n)], ]
    pca   <- prcomp(t(mat), scale.=FALSE)
    ve    <- summary(pca)$importance["Proportion of Variance",] * 100
    sc    <- as.data.frame(pca$x)
    sc$sample    <- rownames(sc)
    sc$condition <- dge$coldata[sc$sample, "condition"]
    px <- input$pca_pc_x; py <- input$pca_pc_y
    p <- plot_ly(sc, x=~get(px), y=~get(py), color=~condition,
      colors=c("#1a73e8","#ef5350","#0f9d58","#f4a433"),
      type="scatter", mode="markers",
      marker=list(size=14, opacity=0.85, line=list(width=1.5, color="white")),
      text=~paste0("<b>",sample,"</b><br>",condition), hoverinfo="text") |>
      layout(
        xaxis=list(title=paste0(px," (",round(ve[px],1),"%)"), gridcolor="#e0e8f5"),
        yaxis=list(title=paste0(py," (",round(ve[py],1),"%)"), gridcolor="#e0e8f5"),
        paper_bgcolor="white", plot_bgcolor="#f8faff",
        font=list(color="#2d3748"), legend=list(font=list(color="#4a5568")))
    if (isTRUE(input$pca_show_labels))
      p <- p |> add_annotations(x=sc[[px]], y=sc[[py]], text=sc$sample,
        showarrow=FALSE, yshift=14, font=list(size=10, color="#4a5568"))
    p
  })

  # Volcano UI switch
  output$dge_volcano_ui <- renderUI({
    if (isTRUE(input$volc_interactive))
      plotlyOutput("dge_volcano_plotly", height="500px")
    else
      plotOutput("dge_volcano_static",  height="500px")
  })

  volcano_df <- reactive({
    req(dge$all_results)
    base <- active_contrast_df("volc_contrast")
    df <- base[!is.na(base$padj) & !is.na(base$log2FoldChange), ]
    df$sig2 <- with(df, ifelse(
      padj < input$volc_padj & log2FoldChange >  input$volc_lfc, "Up",
      ifelse(padj < input$volc_padj & log2FoldChange < -input$volc_lfc, "Down", "NS")))
    df
  })

  output$dge_volcano_plotly <- renderPlotly({
    df  <- volcano_df()
    top <- head(df[df$sig2 != "NS", ][order(df[df$sig2!="NS","padj"]),], input$volc_nlabs)
    cm  <- c(Up="#ef5350", Down="#26a69a", NS="#c9d8f0")
    ym  <- max(df$neg_log10_padj, na.rm=TRUE)
    xr  <- range(df$log2FoldChange, na.rm=TRUE)
    p <- plot_ly(df, x=~log2FoldChange, y=~neg_log10_padj,
      color=~sig2, colors=cm, type="scatter", mode="markers",
      marker=list(size=5, opacity=0.7),
      text=~paste0("<b>",gene,"</b><br>Log2FC: ",round(log2FoldChange,3),
                   "<br>padj: ",signif(padj,3)), hoverinfo="text") |>
      add_segments(x= input$volc_lfc, xend= input$volc_lfc, y=0, yend=ym,
        line=list(dash="dot",color="#f4a433",width=1), showlegend=FALSE, inherit=FALSE) |>
      add_segments(x=-input$volc_lfc, xend=-input$volc_lfc, y=0, yend=ym,
        line=list(dash="dot",color="#f4a433",width=1), showlegend=FALSE, inherit=FALSE) |>
      add_segments(x=xr[1], xend=xr[2],
        y=-log10(input$volc_padj), yend=-log10(input$volc_padj),
        line=list(dash="dot",color="#f4a433",width=1), showlegend=FALSE, inherit=FALSE) |>
      layout(
        xaxis=list(title="Log2 Fold Change", gridcolor="#e0e8f5"),
        yaxis=list(title="-Log10(Adj. P-value)", gridcolor="#e0e8f5"),
        paper_bgcolor="white", plot_bgcolor="#f8faff",
        font=list(color="#2d3748"), legend=list(font=list(color="#4a5568")))
    if (nrow(top) > 0)
      p <- p |> add_annotations(x=top$log2FoldChange, y=top$neg_log10_padj,
        text=top$gene, showarrow=TRUE, arrowcolor="#c9d8f0", arrowsize=0.6,
        font=list(size=9,color="#2d3748"), bgcolor="white",
        bordercolor="#c9d8f0", borderwidth=1, borderpad=2)
    p
  })

  output$dge_volcano_static <- renderPlot({
    df  <- volcano_df()
    top <- head(df[df$sig2!="NS",][order(df[df$sig2!="NS","padj"]),], input$volc_nlabs)
    cm  <- c(Up="#ef5350", Down="#26a69a", NS="#c9d8f0")
    p <- ggplot(df, aes(log2FoldChange, neg_log10_padj, colour=sig2)) +
      geom_point(size=1.5, alpha=0.7) +
      geom_vline(xintercept=c(-input$volc_lfc, input$volc_lfc),
                 linetype="dashed", colour="#f4a433", linewidth=0.5) +
      geom_hline(yintercept=-log10(input$volc_padj),
                 linetype="dashed", colour="#f4a433", linewidth=0.5) +
      scale_colour_manual(values=cm) +
      labs(x="Log2 Fold Change", y="-Log10(Adj. P-value)", colour="") +
      theme_minimal(base_size=13) +
      theme(panel.grid=element_line(colour="#e0e8f5"),
            axis.text=element_text(colour="#4a5568"),
            axis.title=element_text(colour="#2d3748"))
    if (nrow(top) > 0 && requireNamespace("ggrepel", quietly=TRUE))
      p <- p + ggrepel::geom_text_repel(data=top, aes(label=gene),
               size=3, colour="#2d3748", box.padding=0.4, max.overlaps=20)
    p
  }, bg="white")

  # Distribution
  output$dge_dist_plot <- renderPlotly({
    req(dge$res_df, dge$vst_mat, dge$coldata)
    type <- input$dge_dist_type
    rdf  <- dge$res_df

    if (type == "violin") {
      if (!requireNamespace("tidyr", quietly=TRUE) ||
          !requireNamespace("tibble", quietly=TRUE)) return(NULL)
      mat <- dge$vst_mat
      lng <- tidyr::pivot_longer(
        tibble::rownames_to_column(as.data.frame(mat), "gene"),
        -gene, names_to="sample", values_to="vst")
      cd2 <- tibble::rownames_to_column(as.data.frame(dge$coldata), "sample")
      lng <- merge(lng, cd2, by="sample")
      if (isTRUE(input$dge_dist_log)) lng$vst <- log10(lng$vst + 1)
      plot_ly(lng, x=~sample, y=~vst, color=~condition,
        colors=c("#1a73e8","#ef5350"),
        type="violin", box=list(visible=TRUE), meanline=list(visible=TRUE)) |>
        layout(xaxis=list(title="Sample",gridcolor="#e0e8f5"),
               yaxis=list(title=ifelse(isTRUE(input$dge_dist_log),"Log10(VST+1)","VST"),
                          gridcolor="#e0e8f5"),
               paper_bgcolor="white", plot_bgcolor="#f8faff",
               font=list(color="#2d3748"), legend=list(font=list(color="#4a5568")))

    } else if (type == "lfc_hist") {
      df2 <- rdf[!is.na(rdf$log2FoldChange) & is.finite(rdf$log2FoldChange), ]
      plot_ly(df2, x=~log2FoldChange, color=~sig,
        colors=c(Down="#26a69a", NS="#c9d8f0", Up="#ef5350"),
        type="histogram", nbinsx=80, opacity=0.75) |>
        layout(barmode="overlay",
               xaxis=list(title="Log2 Fold Change",gridcolor="#e0e8f5"),
               yaxis=list(title="Gene count",gridcolor="#e0e8f5"),
               paper_bgcolor="white", plot_bgcolor="#f8faff",
               font=list(color="#2d3748"), legend=list(font=list(color="#4a5568")))

    } else if (type == "pval_hist") {
      df2 <- rdf[!is.na(rdf$pvalue), ]
      plot_ly(df2, x=~pvalue, type="histogram", nbinsx=50,
        marker=list(color="#1a73e8", opacity=0.8,
                    line=list(color="white", width=0.5))) |>
        layout(xaxis=list(title="Raw P-value",gridcolor="#e0e8f5"),
               yaxis=list(title="Gene count",gridcolor="#e0e8f5"),
               paper_bgcolor="white", plot_bgcolor="#f8faff",
               font=list(color="#2d3748"))

    } else if (type == "ma") {
      df2 <- rdf[!is.na(rdf$baseMean) & !is.na(rdf$log2FoldChange) & rdf$baseMean>0, ]
      df2$log_base <- log10(df2$baseMean)
      cm <- c(Up="#ef5350", Down="#26a69a", NS="#c9d8f0")
      plot_ly(df2, x=~log_base, y=~log2FoldChange, color=~sig, colors=cm,
        type="scatter", mode="markers", marker=list(size=4, opacity=0.6),
        text=~paste0("<b>",gene,"</b><br>BaseMean:",round(baseMean,1),
                     "<br>Log2FC:",round(log2FoldChange,3)), hoverinfo="text") |>
        add_lines(x=c(0, max(df2$log_base,na.rm=TRUE)), y=c(0,0),
          line=list(color="#f4a433",dash="dash",width=1),
          showlegend=FALSE, inherit=FALSE) |>
        layout(xaxis=list(title="Log10(Mean Expression)",gridcolor="#e0e8f5"),
               yaxis=list(title="Log2 Fold Change",gridcolor="#e0e8f5"),
               paper_bgcolor="white", plot_bgcolor="#f8faff",
               font=list(color="#2d3748"), legend=list(font=list(color="#4a5568")))

    } else if (type == "disp") {
      req(dge$dds)
      dd <- data.frame(
        mean_expr  = rowMeans(DESeq2::counts(dge$dds, normalized=TRUE)),
        dispersion = DESeq2::dispersions(dge$dds))
      dd <- dd[!is.na(dd$dispersion) & dd$mean_expr > 0, ]
      plot_ly(dd, x=~log10(mean_expr), y=~log10(dispersion),
        type="scatter", mode="markers",
        marker=list(color="#1a73e8", size=4, opacity=0.5)) |>
        layout(xaxis=list(title="Log10(Mean Expression)",gridcolor="#e0e8f5"),
               yaxis=list(title="Log10(Dispersion)",gridcolor="#e0e8f5"),
               paper_bgcolor="white", plot_bgcolor="#f8faff",
               font=list(color="#2d3748"))
    }
  })

  # Full results table
  dge_res_filtered <- reactive({
    req(dge$all_results)
    base <- active_contrast_df("tbl_contrast")
    df <- switch(input$dge_tbl_filter,
      all = base,
      sig = base[base$sig != "NS", ],
      up  = base[base$sig == "Up", ],
      dn  = base[base$sig == "Down", ]
    )
    cols <- intersect(c("gene","baseMean","log2FoldChange","lfcSE","stat","pvalue","padj","sig"),
                      colnames(df))
    df <- df[order(df$padj, na.last=TRUE), cols, drop=FALSE]
    num_cols <- intersect(c("baseMean","log2FoldChange","lfcSE","stat","pvalue","padj"), colnames(df))
    df[, num_cols] <- lapply(df[, num_cols, drop=FALSE], function(x) signif(x, 4))
    df
  })
  output$dge_full_table <- renderDT({
    req(dge_res_filtered())
    datatable(dge_res_filtered(),
      options=list(pageLength=15, scrollX=TRUE),
      rownames=FALSE, filter="top", class="compact") |>
      formatStyle("sig",
        backgroundColor=styleEqual(c("Up","Down","NS"),
          c("rgba(239,83,80,0.12)","rgba(38,166,154,0.12)","transparent")),
        color=styleEqual(c("Up","Down","NS"),c("#ef5350","#26a69a","#718096")),
        fontWeight="bold")
  })
  output$dge_dl_csv <- downloadHandler(
    filename=function() paste0("DESeq2_results_", Sys.Date(), ".csv"),
    content =function(file) write.csv(dge_res_filtered(), file, row.names=FALSE)
  )


}

shinyApp(ui, server)
