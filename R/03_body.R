body <- dashboardBody(
  div(
    id = "collapse-logo-overlay",
    tags$img(src = "logo_eksplorasi_pdrb.png", alt = "Eksplorasi PDRB")
  ),
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      :root {
        --pdrb-navy: #17324D;
        --pdrb-blue: #2C5F8A;
        --pdrb-teal: #2D7F78;
        --pdrb-green: #4F806B;
        --pdrb-gold: #C49A45;
        --pdrb-orange: #C97845;
        --pdrb-ink: #1F2937;
        --pdrb-muted: #68737D;
        --pdrb-bg: #F5F7F6;
        --pdrb-surface: #FFFFFF;
        --pdrb-border: #E3E8E5;
        --pdrb-soft-blue: #EEF4F8;
        --pdrb-soft-teal: #EDF6F4;
        --pdrb-soft-gold: #FAF5E9;
        --pdrb-danger: #B45151;
      }

      html, body { background: var(--pdrb-bg); }
      body, .content-wrapper, .right-side {
        font-family: 'Segoe UI', -apple-system, BlinkMacSystemFont, Arial, sans-serif;
        color: var(--pdrb-ink);
      }
      .content-wrapper, .right-side { background: var(--pdrb-bg); }
      .content { padding: 22px 24px 34px; }

      /* Header */
      .skin-blue .main-header .logo,
      .skin-blue .main-header .navbar {
        background: var(--pdrb-navy);
      }
      .skin-blue .main-header .logo {
        color: #FFFFFF;
        font-weight: 700;
        letter-spacing: .1px;
        border-right: 1px solid rgba(255,255,255,.08);
      }
      .skin-blue .main-header .logo:hover { background: var(--pdrb-navy); }
      .skin-blue .main-header .navbar .sidebar-toggle:hover {
        background: rgba(255,255,255,.08);
      }
      .brand-mark { color: #E2BE6A; margin-right: 8px; }
      .brand-title { font-size: 16px; }

      /* Sidebar */
      .skin-blue .main-sidebar {
        background: #12283A;
        border-right: 1px solid rgba(255,255,255,.04);
        box-shadow: 3px 0 18px rgba(20, 43, 61, .13);
      }
      .skin-blue .sidebar-menu > li > a {
        color: #D5E0E7;
        border-left: 3px solid transparent;
        padding-top: 13px;
        padding-bottom: 13px;
      }
      .skin-blue .sidebar-menu > li:hover > a {
        color: #FFFFFF;
        background: rgba(255,255,255,.055);
        border-left-color: #74AAA2;
      }
      .skin-blue .sidebar-menu > li.active > a {
        color: #FFFFFF;
        background: rgba(196,154,69,.13);
        border-left-color: var(--pdrb-gold);
        font-weight: 600;
      }
      .skin-blue .sidebar-menu > li.header.sidebar-section-label {
        padding: 18px 18px 7px;
        background: transparent;
        color: #83A4B7;
        font-size: 10px;
        font-weight: 800;
        letter-spacing: .12em;
        line-height: 1.2;
      }
      .skin-blue .sidebar-menu > li.header.sidebar-section-label:first-of-type {
        padding-top: 14px;
      }
      .skin-blue .sidebar-menu > li > a > .fa {
        width: 20px;
        margin-right: 7px;
        text-align: center;
      }
      .sidebar-divider {
        border-color: rgba(255,255,255,.10);
        margin: 12px 17px 16px;
      }
      .sidebar-filter { padding: 0 17px 22px; }
.filter-heading {
        display: flex;
        align-items: center;
        gap: 8px;
        color: #F0D28F;
        font-weight: 700;
        margin: 0 0 15px;
      }
      .sidebar-filter label {
        color: #D6E0E6;
        font-size: 12px;
        font-weight: 600;
      }
      .sidebar-time-note {
        display: flex;
        align-items: flex-start;
        gap: 8px;
        margin-top: 8px;
        padding: 11px 12px;
        border: 1px solid rgba(226,190,106,.22);
        border-radius: 9px;
        background: rgba(196,154,69,.10);
        color: #E8D7AC;
        font-size: 11.5px;
        line-height: 1.45;
      }
      .sidebar-time-note i {
        margin-top: 2px;
        color: #F0D28F;
      }
      .filter-short-note {
        display: flex;
        align-items: flex-start;
        gap: 8px;
        margin: 0 0 14px;
        padding: 11px 12px;
        border: 1px solid rgba(116,170,162,.25);
        border-radius: 9px;
        background: rgba(45,127,120,.12);
        color: #DCEDE9;
        font-size: 11.7px;
        line-height: 1.45;
      }
      .filter-short-note i { margin-top: 2px; color: #8FCBC3; }
      .advanced-filter-details {
        margin: 10px 0 14px;
        border: 1px solid rgba(255,255,255,.12);
        border-radius: 10px;
        background: rgba(255,255,255,.045);
      }
      .advanced-filter-details > summary {
        cursor: pointer;
        list-style: none;
        display: flex;
        align-items: center;
        gap: 8px;
        padding: 11px 12px;
        color: #F0D28F;
        font-size: 12px;
        font-weight: 700;
      }
      .advanced-filter-details > summary::-webkit-details-marker { display: none; }
      .advanced-filter-details[open] > summary .fa-chevron-right { transform: rotate(90deg); }
      .advanced-filter-details > summary .fa-chevron-right { transition: transform .16s ease; }
      .advanced-filter-content { padding: 0 12px 12px; }
      .content .advanced-filter-details {
        border-color: var(--pdrb-border);
        background: #FBFCFC;
      }
      .content .advanced-filter-details > summary {
        color: var(--pdrb-navy);
      }
      .analysis-filter-flex,
      .tabel-filter-flex {
        display: flex;
        flex-wrap: wrap;
        gap: 12px;
        align-items: flex-start;
        width: 100%;
      }
      .analysis-filter-flex .analysis-filter-item,
      .tabel-filter-flex .tabel-filter-item {
        min-width: 170px;
      }
      .analysis-filter-flex .selectize-control,
      .tabel-filter-flex .selectize-control {
        margin-bottom: 0;
      }
      .analysis-filter-row .analysis-filter-item.type { flex: 0 1 16.6667%; }
      .analysis-filter-row .analysis-filter-item.lq-extra { flex: 1 1 20%; }
      .analysis-filter-row .analysis-filter-item.dlq-extra { flex: 1 1 25%; }
      .analysis-filter-row .analysis-filter-item.shift-extra { flex: 1 1 41%; }
      .analysis-filter-row > div[data-display-if] { flex: 1 1 80%; }
      .analysis-extra-flex { display: flex; flex-wrap: wrap; gap: 12px; align-items: flex-start; }
      .analysis-extra-flex .analysis-filter-item { min-width: 170px; }

      /* Card Metode Analisis Potensi Wilayah: susunan final per metode */
      .potential-method-stack {
        display: flex;
        flex-direction: column;
        gap: 14px;
        width: 100%;
      }
      .potential-method-primary {
        display: flex;
        flex-wrap: wrap;
        gap: 12px;
        align-items: flex-end;
        width: 100%;
      }
      .potential-method-primary > .potential-method-item,
      .potential-method-primary > div[data-display-if] {
        flex: 1 1 calc(25% - 12px);
        min-width: 210px;
      }
      .potential-method-primary > div[data-display-if] > .potential-method-item {
        width: 100%;
      }
      .potential-method-secondary {
        display: grid;
        gap: 12px;
        align-items: end;
        width: 100%;
      }
      .potential-method-secondary-3 {
        grid-template-columns: repeat(3, minmax(0, 1fr));
      }
      .potential-method-secondary-2 {
        grid-template-columns: repeat(2, minmax(0, 1fr));
      }
      .potential-method-stack .form-group,
      .potential-method-stack .selectize-control {
        margin-bottom: 0;
      }
      .potential-method-item {
        min-width: 0;
      }
      @media (max-width: 1200px) {
        .potential-method-primary > .potential-method-item,
        .potential-method-primary > div[data-display-if] {
          flex-basis: calc(50% - 12px);
        }
        .potential-method-secondary-3,
        .potential-method-secondary-2 {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
      }
      @media (max-width: 767px) {
        .potential-method-primary > .potential-method-item,
        .potential-method-primary > div[data-display-if] {
          flex-basis: 100%;
          min-width: 100%;
        }
        .potential-method-secondary-3,
        .potential-method-secondary-2 {
          grid-template-columns: 1fr;
        }
      }
      .tabel-filter-shift .tabel-filter-item { flex: 1 1 18%; }
      @media (max-width: 992px) {
        .analysis-filter-flex .analysis-filter-item,
        .tabel-filter-flex .tabel-filter-item {
          flex: 1 1 100% !important;
        }
      }
      .sidebar-filter .form-control,
      .sidebar-filter .selectize-input {
        min-height: 36px;
        border: 1px solid rgba(255,255,255,.13);
        border-radius: 8px;
        box-shadow: none;
      }
      .sidebar-filter .selectize-dropdown { border-radius: 8px; }
      .content-wrapper,
      .right-side,
      .tab-content,
      .tab-pane,
      .box,
      .box-body {
        overflow: visible !important;
      }
      .hero-panel,
      .small-box {
        overflow: hidden;
      }
      .selectize-control {
        position: relative;
        z-index: 20;
      }
      .selectize-control.dropdown-active {
        z-index: 4000;
      }
      body > .selectize-dropdown,
      .selectize-dropdown,
      .select2-drop,
      .select2-dropdown,
      .dropdown-menu {
        z-index: 5000 !important;
      }
      .main-sidebar .selectize-control.dropdown-active {
        z-index: 6000;
      }
      .main-sidebar .selectize-dropdown {
        z-index: 6001 !important;
      }
      .dataTables_wrapper,
      .plotly,
      .js-plotly-plot {
        overflow: visible !important;
      }
      /* Stabilitas ukuran seluruh grafik Plotly */
      .plotly.html-widget-output,
      .plotly.html-widget,
      .js-plotly-plot,
      .plot-container,
      .svg-container {
        width: 100% !important;
        max-width: 100% !important;
      }
      /* Semua grafik Plotly disembunyikan sampai layout dan ukurannya stabil. */
      .pdrb-plot-shell {
        position: relative;
        width: 100%;
        min-height: var(--pdrb-plot-height, 400px);
        overflow: hidden;
        isolation: isolate;
      }
      .pdrb-plot-shell > .plotly.html-widget-output {
        width: 100% !important;
        min-height: var(--pdrb-plot-height, 400px);
        opacity: 1;
        transition: opacity .16s ease;
      }
      .pdrb-plot-shell.pdrb-plot-loading > .plotly.html-widget-output,
      .pdrb-plot-shell.pdrb-plot-unavailable > .plotly.html-widget-output {
        opacity: 0;
        pointer-events: none;
      }
      .pdrb-plot-loading-overlay {
        position: absolute;
        inset: 0;
        z-index: 4;
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 11px;
        padding: 24px;
        color: var(--pdrb-muted);
        background: var(--pdrb-surface);
        font-weight: 650;
        text-align: center;
        opacity: 0;
        visibility: hidden;
        pointer-events: none;
        transition: opacity .14s ease;
      }
      .pdrb-plot-shell.pdrb-plot-loading .pdrb-plot-loading-overlay,
      .pdrb-plot-shell.pdrb-plot-unavailable .pdrb-plot-loading-overlay {
        opacity: 1;
        visibility: visible;
        pointer-events: auto;
      }
      .pdrb-plot-spinner,
      .pdrb-global-spinner {
        flex: 0 0 auto;
        width: 25px;
        height: 25px;
        border: 3px solid var(--pdrb-border);
        border-top-color: var(--pdrb-primary, var(--pdrb-blue));
        border-radius: 50%;
        animation: pdrbPlotSpin .72s linear infinite;
      }
      .pdrb-plot-shell.pdrb-plot-unavailable .pdrb-plot-spinner {
        display: none;
      }
      @keyframes pdrbPlotSpin {
        to { transform: rotate(360deg); }
      }

      /* Loading global mencegah pengguna melihat card/tabel/grafik dalam keadaan setengah jadi. */
      .pdrb-global-loader {
        position: fixed;
        inset: 0;
        z-index: 10050;
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 12px;
        width: 100vw;
        height: 100vh;
        background: rgba(245, 247, 246, .97);
        color: var(--pdrb-navy);
        font-size: 15px;
        font-weight: 700;
        opacity: 0;
        visibility: hidden;
        pointer-events: none;
        backdrop-filter: blur(2px);
        transition: opacity .14s ease, visibility .14s ease;
      }
      .pdrb-global-loader.is-visible {
        opacity: 1;
        visibility: visible;
        pointer-events: auto;
      }
      .pdrb-global-loader-content {
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 13px;
        max-width: min(560px, calc(100vw - 40px));
        padding: 18px 22px;
        border: 1px solid rgba(26, 91, 125, .12);
        border-radius: 14px;
        background: rgba(255, 255, 255, .86);
        box-shadow: 0 14px 38px rgba(17, 51, 75, .10);
      }
      .pdrb-global-loader-copy {
        display: flex;
        flex-direction: column;
        gap: 3px;
        line-height: 1.35;
      }
      #pdrb_global_loader_message {
        display: block;
        font-size: 15px;
        font-weight: 700;
      }
      #pdrb_global_loader_detail {
        display: block;
        color: var(--pdrb-muted);
        font-size: 12.5px;
        font-weight: 500;
        max-width: 470px;
        min-height: 17px;
      }
      .pdrb-global-progress-row {
        display: flex;
        align-items: center;
        gap: 10px;
        margin-top: 8px;
        min-width: min(420px, calc(100vw - 110px));
      }
      #pdrb_global_loader_percent {
        flex: 0 0 48px;
        color: var(--pdrb-primary, var(--pdrb-blue));
        font-size: 16px;
        font-weight: 800;
        text-align: right;
        font-variant-numeric: tabular-nums;
      }
      .pdrb-global-progress-track {
        position: relative;
        flex: 1 1 auto;
        height: 9px;
        overflow: hidden;
        border-radius: 999px;
        background: #E4ECEF;
      }
      #pdrb_global_loader_bar {
        display: block;
        width: 0%;
        height: 100%;
        border-radius: inherit;
        background: var(--pdrb-primary, var(--pdrb-blue));
        transition: width .18s ease;
      }
      body.pdrb-global-loading {
        overflow: hidden !important;
      }
      /* Progress bawaan Shiny disembunyikan karena progres ditampilkan
         penuh pada overlay Proses Data. */
      .shiny-progress-container,
      .shiny-progress {
        display: none !important;
      }

      /* Intro halaman */
      .page-intro {
        display: flex;
        align-items: flex-start;
        gap: 16px;
        background: var(--pdrb-surface);
        border: 1px solid var(--pdrb-border);
        border-radius: 15px;
        padding: 21px 23px;
        margin-bottom: 18px;
        box-shadow: 0 5px 18px rgba(31, 41, 55, .045);
      }
      .page-intro-icon {
        display: flex;
        align-items: center;
        justify-content: center;
        flex: 0 0 42px;
        width: 42px;
        height: 42px;
        border-radius: 11px;
        background: var(--pdrb-soft-blue);
        color: var(--pdrb-blue);
        font-size: 17px;
      }
      .page-intro-copy { min-width: 0; }
      .eyebrow {
        display: block;
        margin-bottom: 4px;
        color: var(--pdrb-teal);
        font-size: 11px;
        font-weight: 800;
        letter-spacing: .09em;
        text-transform: uppercase;
      }
      .page-intro h2 {
        margin: 0 0 5px;
        color: var(--pdrb-navy);
        font-size: 24px;
        font-weight: 750;
        letter-spacing: -.25px;
      }
      .page-intro p {
        margin: 0;
        color: var(--pdrb-muted);
        font-size: 14px;
        line-height: 1.55;
      }

      /* Langkah awal */
      .steps-row { margin-bottom: 30px; }
      .step-card {
        display: flex;
        align-items: flex-start;
        gap: 13px;
        min-height: 92px;
        padding: 17px;
        background: var(--pdrb-surface);
        border: 1px solid var(--pdrb-border);
        border-radius: 13px;
      }
      .step-number {
        display: flex;
        align-items: center;
        justify-content: center;
        flex: 0 0 30px;
        width: 30px;
        height: 30px;
        border-radius: 9px;
        background: var(--pdrb-navy);
        color: #FFFFFF;
        font-size: 12px;
        font-weight: 800;
      }
      .step-copy strong { color: var(--pdrb-navy); font-size: 14px; }
      .step-copy p {
        margin: 4px 0 0;
        color: var(--pdrb-muted);
        font-size: 12.5px;
        line-height: 1.45;
      }

      /* Hero ringkasan */
      .hero-panel {
        position: relative;
        overflow: hidden;
        background: linear-gradient(122deg, #17324D 0%, #285777 58%, #2D7F78 100%);
        border-radius: 16px;
        padding: 25px 28px;
        margin-bottom: 18px;
        color: #FFFFFF;
        box-shadow: 0 10px 26px rgba(23, 50, 77, .16);
      }
      .hero-panel:after {
        content: '';
        position: absolute;
        width: 170px;
        height: 170px;
        right: -45px;
        top: -75px;
        border-radius: 50%;
        background: rgba(226,190,106,.13);
      }
      .hero-panel h2 {
        position: relative;
        z-index: 1;
        margin: 0 0 7px;
        font-size: 25px;
        font-weight: 750;
      }
      .hero-panel p {
        position: relative;
        z-index: 1;
        margin: 0;
        color: #E6EEF2;
      }
      .dataset-badge {
        display: inline-block;
        position: relative;
        z-index: 1;
        margin-top: 13px;
        padding: 7px 11px;
        border: 1px solid rgba(255,255,255,.18);
        border-radius: 999px;
        background: rgba(255,255,255,.10);
        font-size: 12px;
      }
      .palette-key {
        position: relative;
        z-index: 1;
        margin-top: 11px;
        color: #E7EFF2;
        font-size: 12px;
      }
      .palette-dot {
        display: inline-block;
        width: 8px;
        height: 8px;
        margin: 0 5px 0 12px;
        border-radius: 50%;
      }
      .palette-dot:first-child { margin-left: 0; }
      .dot-adhb { background: #79A7D0; }
      .dot-adhk { background: #71B4A6; }

      /* Box */
      .box {
        border: 1px solid var(--pdrb-border);
        border-top: 0;
        border-radius: 14px;
        overflow: visible;
        background: var(--pdrb-surface);
        box-shadow: 0 5px 18px rgba(31, 41, 55, .055);
      }
      .box-body { overflow: visible; }
      .box.box-solid > .box-header {
        border-radius: 14px 14px 0 0;
      }
      .filter-panel .box,
      .filter-panel .box-body,
      .distribution-period-filter,
      .structure-period-filter,
      .comparison-period-filter,
      .preview-control-panel,
      .subcategory-selector {
        overflow: visible !important;
      }
      .box-header { padding: 13px 15px; }
      .box-title { font-size: 15px; font-weight: 700; }
      .box-body { padding: 17px; }
      .box.box-solid.box-primary > .box-header { background: var(--pdrb-blue); }
      .box.box-solid.box-info > .box-header { background: var(--pdrb-teal); }
      .box.box-solid.box-success > .box-header { background: var(--pdrb-green); }
      .box.box-solid.box-warning > .box-header {
        background: var(--pdrb-gold);
        color: #2E2A20;
      }

      /* Panel pilihan struktur ekonomi */
      .structure-filter-panel {
        min-height: 520px;
        display: flex;
        flex-direction: column;
        padding: 4px 2px 2px;
      }
      .structure-filter-panel .form-group {
        margin-bottom: 22px;
      }
      .structure-filter-panel label {
        color: var(--pdrb-navy);
        font-size: 12px;
        font-weight: 700;
        letter-spacing: .01em;
      }
      .structure-filter-panel .form-control,
      .structure-filter-panel .selectize-input {
        min-height: 40px;
        border: 1px solid var(--pdrb-border);
        border-radius: 9px;
        box-shadow: none;
      }
      .structure-filter-panel .selectize-dropdown {
        z-index: 5000;
        border-radius: 9px;
      }
      .structure-filter-hint {
        margin-top: auto;
        padding: 14px 15px;
        border: 1px solid #E8DFC8;
        border-radius: 11px;
        background: var(--pdrb-soft-gold);
        color: #67562F;
        font-size: 12.5px;
        line-height: 1.55;
      }
      .structure-filter-hint i {
        margin-right: 7px;
        color: var(--pdrb-gold);
      }

      /* KPI */
      .small-box {
        border-radius: 14px;
        overflow: hidden;
        box-shadow: 0 7px 20px rgba(31, 41, 55, .10);
      }
      .small-box h3 { font-size: 25px; letter-spacing: -.35px; }
      .small-box p { font-size: 13px; }
      .small-box .icon { opacity: .16; }
      .small-box.bg-blue {
        background: linear-gradient(135deg, #17324D, #2C5F8A) !important;
      }
      .small-box.bg-aqua {
        background: linear-gradient(135deg, #315F57, #2D7F78) !important;
      }

      /* Catatan dan upload */
      .info-note,
      .privacy-note,
      .soft-note {
        padding: 13px 15px;
        border-radius: 10px;
        margin-bottom: 15px;
        font-size: 13px;
        line-height: 1.5;
      }
      .info-note {
        border-left: 3px solid var(--pdrb-teal);
        background: var(--pdrb-soft-teal);
        color: #28564F;
      }
      .privacy-note {
        border-left: 3px solid var(--pdrb-green);
        background: #EFF6F1;
        color: #315743;
      }
      .soft-note {
        border-left: 3px solid var(--pdrb-gold);
        background: var(--pdrb-soft-gold);
        color: #665427;
      }
      .upload-card {
        border: 1px dashed #91AAA2;
        background: #FAFCFB;
        border-radius: 12px;
        padding: 18px;
      }
      .upload-card p { color: var(--pdrb-muted); font-size: 12.5px; }
      .upload-actions { margin-top: 8px; }
      .upload-actions .btn { margin: 4px 6px 4px 0; }

      /* Pemilih pratinjau data */
      .preview-control-panel {
        max-width: 720px;
        margin-bottom: 17px;
        padding: 16px 17px 13px;
        border: 1px solid var(--pdrb-border);
        border-radius: 12px;
        background: #F9FBFA;
      }
      .preview-control-panel .form-group { margin-bottom: 8px; }
      .preview-control-panel label {
        color: var(--pdrb-navy);
        font-size: 12.5px;
        font-weight: 750;
      }
      .preview-control-panel .selectize-input {
        min-height: 43px;
        padding: 10px 12px;
        border: 1px solid #C9D4CF;
        border-radius: 9px;
        box-shadow: none;
      }
      .preview-control-panel .selectize-input.focus {
        border-color: var(--pdrb-teal);
        box-shadow: 0 0 0 3px rgba(45,127,120,.10);
      }
      .preview-control-panel .selectize-dropdown {
        z-index: 6000;
        border: 1px solid #C9D4CF;
        border-radius: 9px;
        box-shadow: 0 10px 24px rgba(31,41,55,.12);
      }
      .preview-control-panel .selectize-dropdown .optgroup-header {
        padding: 9px 11px 6px;
        background: #F2F6F4;
        color: var(--pdrb-teal);
        font-size: 11px;
        font-weight: 800;
        letter-spacing: .05em;
        text-transform: uppercase;
      }
      .preview-control-panel .selectize-dropdown .option {
        padding: 9px 12px;
      }
      .preview-help {
        display: flex;
        align-items: center;
        gap: 7px;
        color: var(--pdrb-muted);
        font-size: 12px;
        line-height: 1.45;
      }
      .preview-help i { color: var(--pdrb-teal); }
      .preview-empty-state {
        padding: 28px 18px;
        border: 1px dashed #B8C7C1;
        border-radius: 12px;
        background: #FAFCFB;
        color: var(--pdrb-muted);
        text-align: center;
      }
      .preview-empty-state > i {
        display: block;
        margin-bottom: 9px;
        color: var(--pdrb-teal);
        font-size: 24px;
      }
      .preview-empty-state strong {
        display: block;
        margin-bottom: 4px;
        color: var(--pdrb-navy);
      }
      .preview-empty-state p { margin: 0; font-size: 12.5px; }

      .btn {
        border-radius: 8px;
        font-weight: 600;
        box-shadow: none !important;
      }
      .btn-primary, .btn-economy {
        background: var(--pdrb-blue) !important;
        border-color: var(--pdrb-blue) !important;
        color: #FFFFFF !important;
      }
      .btn-success {
        background: var(--pdrb-green) !important;
        border-color: var(--pdrb-green) !important;
      }
      .btn-default { border-color: #C5CFCA; color: var(--pdrb-ink); }
      .status-ok { color: var(--pdrb-green); font-weight: 700; }
      .status-error { color: var(--pdrb-danger); font-weight: 700; }
      .status-idle { color: var(--pdrb-muted); font-weight: 700; }

      /* Tentang */
      .about-grid {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 14px;
        margin-bottom: 16px;
      }
      .about-card {
        min-height: 175px;
        padding: 18px;
        border: 1px solid var(--pdrb-border);
        border-radius: 12px;
        background: #FBFCFC;
      }
      .about-card .fa { color: var(--pdrb-teal); font-size: 17px; }
      .about-card h4 {
        margin: 11px 0 8px;
        color: var(--pdrb-navy);
        font-size: 15px;
        font-weight: 750;
      }
      .about-card p,
      .about-card li {
        color: var(--pdrb-muted);
        font-size: 13px;
        line-height: 1.55;
      }
      .about-card ul { padding-left: 18px; margin-bottom: 0; }

      /* Distribusi data dan statistika deskriptif */
      .kernel-context {
        margin-bottom: 14px;
        padding: 12px 14px;
        border-left: 3px solid var(--pdrb-teal);
        border-radius: 8px;
        background: var(--pdrb-soft-teal);
        color: #315F57;
        font-size: 12.5px;
        line-height: 1.5;
      }
      .distribution-note {
        display: flex;
        align-items: flex-start;
        gap: 8px;
        margin-bottom: 14px;
        padding: 12px 14px;
        border: 1px solid #D9E7E2;
        border-radius: 10px;
        background: #F7FBF9;
        color: #3D625A;
        font-size: 12.5px;
        line-height: 1.5;
      }
      .distribution-note i {
        margin-top: 2px;
        color: var(--pdrb-teal);
      }
      .distribution-period-filter .form-group { margin-bottom: 8px; }
      .distribution-period-filter label {
        color: var(--pdrb-navy);
        font-size: 12px;
        font-weight: 700;
      }
      .distribution-period-filter .form-control,
      .distribution-period-filter .selectize-input {
        min-height: 40px;
        border: 1px solid #C9D4CF;
        border-radius: 9px;
        box-shadow: none;
      }
      .distribution-valuebox-row {
        margin-bottom: 8px;
      }
      .distribution-valuebox-row .small-box h3 {
        font-size: 21px;
      }
      .distribution-valuebox-row .small-box p {
        font-size: 12px;
      }
      .structure-period-filter .form-group { margin-bottom: 8px; }
      .structure-period-filter label,
      .subcategory-selector label {
        color: var(--pdrb-navy);
        font-size: 12px;
        font-weight: 700;
      }
      .structure-period-filter .form-control,
      .subcategory-selector .form-control,
      .subcategory-selector .selectize-input {
        min-height: 40px;
        border: 1px solid #C9D4CF;
        border-radius: 9px;
        box-shadow: none;
      }
      .subcategory-selector {
        position: relative;
        z-index: 20;
        margin-bottom: 14px;
        padding: 14px 15px 8px;
        border: 1px solid #D9E7E2;
        border-radius: 11px;
        background: #F7FBF9;
      }
      .subcategory-selector .selectize-dropdown {
        z-index: 10000 !important;
        border-radius: 9px;
      }
      /* Dropdown filter analisis ditempatkan pada body agar tidak terpotong card. */
      body > .selectize-dropdown,
      .selectize-dropdown.dropdown-active,
      .selectize-dropdown {
        z-index: 25000 !important;
      }
      .selectize-control { z-index: 30; }
      .selectize-control.dropdown-active { z-index: 25001 !important; }
      .main-sidebar .selectize-control.dropdown-active { z-index: 26000 !important; }
      .main-sidebar .selectize-dropdown { z-index: 26001 !important; }
      .distribution-period-filter .selectize-control,
      .structure-period-filter .selectize-control {
        width: 100%;
      }
      .distribution-period-filter .selectize-input,
      .structure-period-filter .selectize-input {
        min-height: 40px;
        border: 1px solid #C9D4CF;
        border-radius: 9px;
        box-shadow: none;
      }

      /* Ringkasan filter aktif */
      .filter-summary-note {
        display: flex;
        align-items: flex-start;
        gap: 9px;
        padding: 12px 14px;
        border: 1px solid #D9E7E2;
        border-radius: 10px;
        background: #F7FBF9;
        color: #3D625A;
        font-size: 12.5px;
        line-height: 1.5;
      }
      .filter-summary-note i { margin-top: 2px; color: var(--pdrb-teal); }
      .compact-filter-row .form-group { margin-bottom: 8px; }
      .compact-filter-row label {
        color: var(--pdrb-navy);
        font-size: 12px;
        font-weight: 700;
      }
      .compact-filter-row .form-control,
      .compact-filter-row .selectize-input {
        min-height: 40px;
        border: 1px solid #C9D4CF;
        border-radius: 9px;
        box-shadow: none;
      }

      /* Card ringkasan */
      .summary-valuebox-row .small-box {
        min-height: 86px;
        border-radius: 10px;
      }
      .summary-valuebox-row .small-box .inner {
        padding: 12px 15px;
      }
      .summary-valuebox-row .small-box h3 {
        max-width: calc(100% - 42px);
        margin-bottom: 6px;
        font-size: 22px;
        line-height: 1.15;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      .summary-valuebox-row .small-box p {
        max-width: calc(100% - 42px);
        margin-bottom: 0;
        font-size: 12px;
        line-height: 1.35;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      .summary-valuebox-row .small-box .icon {
        top: 8px;
        right: 10px;
        font-size: 58px;
      }
      .trend-valuebox-row {
        margin-bottom: 8px;
      }
      .trend-valuebox-row .small-box h3 {
        font-size: 21px;
      }
      .trend-valuebox-row .small-box p {
        font-size: 12px;
      }

      /* DataTables */
      table.dataTable thead th {
        background: #EEF2F0;
        color: #284655;
        border-bottom: 1px solid var(--pdrb-border) !important;
      }
      table.dataTable tbody td { vertical-align: middle; }
      .dt-buttons .btn {
        background: var(--pdrb-teal) !important;
        border-color: var(--pdrb-teal) !important;
        color: #FFFFFF !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background: var(--pdrb-blue) !important;
        border-color: var(--pdrb-blue) !important;
        color: #FFFFFF !important;
      }



      /* Progress loading saat membaca file Excel */
      .shiny-progress .progress {
        height: 13px;
        border-radius: 999px;
        background: #E8EFEA;
        box-shadow: inset 0 1px 2px rgba(31,41,55,.08);
      }
      .shiny-progress .bar {
        border-radius: 999px;
        background: linear-gradient(90deg, var(--pdrb-teal), var(--pdrb-gold));
      }
      .shiny-progress .progress-text {
        top: 70px !important;
        right: 24px !important;
        left: auto !important;
        width: min(430px, calc(100% - 48px));
        padding: 14px 16px 13px;
        border: 1px solid var(--pdrb-border);
        border-radius: 14px;
        background: rgba(255,255,255,.98);
        color: var(--pdrb-navy);
        box-shadow: 0 12px 30px rgba(31,41,55,.18);
      }
      .shiny-progress .progress-text .progress-message {
        font-weight: 800;
        letter-spacing: -.1px;
      }
      .shiny-progress .progress-text .progress-detail {
        margin-top: 4px;
        color: var(--pdrb-muted);
        font-size: 12.5px;
        line-height: 1.4;
      }



      /* Upload dan panduan publik */
      .compact-upload-card .form-group {
        margin-bottom: 12px;
      }
      .upload-status-panel p {
        margin-bottom: 7px;
        line-height: 1.45;
      }
      .upload-requirements-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 11px;
        margin-top: 13px;
      }
      .requirement-card {
        min-height: 82px;
        padding: 13px 14px;
        border: 1px solid var(--pdrb-border);
        border-radius: 11px;
        background: #FBFCFC;
      }
      .requirement-card strong {
        display: block;
        color: var(--pdrb-navy);
        margin-bottom: 5px;
      }
      .requirement-card p {
        margin: 0;
        color: var(--pdrb-muted);
        font-size: 12.5px;
        line-height: 1.45;
      }
      .compact-note {
        margin: 0;
        color: var(--pdrb-muted);
        font-size: 13px;
        line-height: 1.5;
      }
      .compact-guide-table th,
      .compact-guide-table td {
        vertical-align: top !important;
        font-size: 13px;
        line-height: 1.45;
      }


      /* FINAL HEADER LOGO + COLLAPSE SIDEBAR FIX */
      .main-header .logo {
        display: flex !important;
        align-items: center !important;
        justify-content: flex-start !important;
        gap: 10px !important;
        padding: 0 16px !important;
        overflow: hidden !important;
      }
      .main-header .logo .pdrb-logo-full {
        display: inline-flex !important;
        align-items: center !important;
        justify-content: flex-start !important;
        gap: 10px !important;
        min-width: 0 !important;
      }
      .main-header .logo .pdrb-logo-mini {
        display: none !important;
        align-items: center !important;
        justify-content: center !important;
      }
      .main-header .logo .app-logo-img {
        width: 34px !important;
        height: 34px !important;
        object-fit: contain !important;
        display: block !important;
        flex: 0 0 34px !important;
      }
      .main-header .logo .app-logo-text {
        color: #FFFFFF !important;
        font-size: 16px !important;
        font-weight: 800 !important;
        letter-spacing: .2px !important;
        white-space: nowrap !important;
        line-height: 1 !important;
      }

      @media (min-width: 768px) {
        body.sidebar-mini.sidebar-collapse .main-header .logo,
        body.sidebar-collapse .main-header .logo {
          width: 64px !important;
          min-width: 64px !important;
          max-width: 64px !important;
          padding: 0 !important;
          justify-content: center !important;
          text-align: center !important;
        }
        body.sidebar-mini.sidebar-collapse .main-header .logo .pdrb-logo-full,
        body.sidebar-collapse .main-header .logo .pdrb-logo-full {
          display: none !important;
        }
        body.sidebar-mini.sidebar-collapse .main-header .logo .pdrb-logo-mini,
        body.sidebar-collapse .main-header .logo .pdrb-logo-mini {
          display: inline-flex !important;
          width: 64px !important;
          height: 50px !important;
          align-items: center !important;
          justify-content: center !important;
        }
        body.sidebar-mini.sidebar-collapse .main-header .logo .app-logo-img,
        body.sidebar-collapse .main-header .logo .app-logo-img {
          width: 32px !important;
          height: 32px !important;
          margin: 0 auto !important;
        }

        body.sidebar-mini.sidebar-collapse .main-sidebar,
        body.sidebar-collapse .main-sidebar,
        body.sidebar-mini.sidebar-collapse .left-side,
        body.sidebar-collapse .left-side,
        body.sidebar-mini.sidebar-collapse .main-sidebar:hover,
        body.sidebar-collapse .main-sidebar:hover {
          width: 64px !important;
          left: 0 !important;
          margin-left: 0 !important;
          transform: translate(0, 0) !important;
          -webkit-transform: translate(0, 0) !important;
          overflow: visible !important;
          z-index: 1050 !important;
          background: #12283A !important;
        }
        body.sidebar-mini.sidebar-collapse .main-header .navbar,
        body.sidebar-collapse .main-header .navbar,
        body.sidebar-mini.sidebar-collapse .content-wrapper,
        body.sidebar-collapse .content-wrapper,
        body.sidebar-mini.sidebar-collapse .right-side,
        body.sidebar-collapse .right-side,
        body.sidebar-mini.sidebar-collapse .main-footer,
        body.sidebar-collapse .main-footer {
          margin-left: 64px !important;
        }
        body.sidebar-mini.sidebar-collapse .main-sidebar .sidebar,
        body.sidebar-collapse .main-sidebar .sidebar,
        body.sidebar-mini.sidebar-collapse .main-sidebar:hover .sidebar,
        body.sidebar-collapse .main-sidebar:hover .sidebar,
        body.sidebar-mini.sidebar-collapse .sidebar-menu,
        body.sidebar-collapse .sidebar-menu,
        body.sidebar-mini.sidebar-collapse .main-sidebar:hover .sidebar-menu,
        body.sidebar-collapse .main-sidebar:hover .sidebar-menu {
          display: block !important;
          width: 64px !important;
          overflow: visible !important;
        }
        body.sidebar-mini.sidebar-collapse .sidebar-menu,
        body.sidebar-collapse .sidebar-menu {
          margin: 0 !important;
          padding: 10px 0 0 !important;
        }
        body.sidebar-mini.sidebar-collapse .sidebar-menu > li.header.sidebar-section-label,
        body.sidebar-collapse .sidebar-menu > li.header.sidebar-section-label,
        body.sidebar-mini.sidebar-collapse .sidebar-divider,
        body.sidebar-collapse .sidebar-divider,
        body.sidebar-mini.sidebar-collapse .modern-sidebar-filter,
        body.sidebar-collapse .modern-sidebar-filter,
        body.sidebar-mini.sidebar-collapse .sidebar-filter,
        body.sidebar-collapse .sidebar-filter {
          display: none !important;
        }
        body.sidebar-mini.sidebar-collapse .sidebar-menu > li:not(.header),
        body.sidebar-collapse .sidebar-menu > li:not(.header) {
          position: relative !important;
          display: block !important;
          width: 64px !important;
          min-height: 48px !important;
          overflow: visible !important;
        }
        body.sidebar-mini.sidebar-collapse .sidebar-menu > li > a,
        body.sidebar-collapse .sidebar-menu > li > a,
        body.sidebar-mini.sidebar-collapse .main-sidebar:hover .sidebar-menu > li > a,
        body.sidebar-collapse .main-sidebar:hover .sidebar-menu > li > a {
          position: relative !important;
          display: flex !important;
          align-items: center !important;
          justify-content: center !important;
          width: 64px !important;
          height: 48px !important;
          padding: 0 !important;
          margin: 0 !important;
          text-align: center !important;
          border-left: 3px solid transparent !important;
          overflow: visible !important;
          color: #D5E0E7 !important;
          background: transparent !important;
        }
        body.sidebar-mini.sidebar-collapse .sidebar-menu > li:hover > a,
        body.sidebar-collapse .sidebar-menu > li:hover > a {
          color: #FFFFFF !important;
          background: rgba(255,255,255,.07) !important;
          border-left-color: #74AAA2 !important;
        }
        body.sidebar-mini.sidebar-collapse .sidebar-menu > li.active > a,
        body.sidebar-collapse .sidebar-menu > li.active > a {
          color: #FFFFFF !important;
          background: rgba(196,154,69,.16) !important;
          border-left-color: var(--pdrb-gold) !important;
        }
        body.sidebar-mini.sidebar-collapse .sidebar-menu > li > a > .fa,
        body.sidebar-collapse .sidebar-menu > li > a > .fa,
        body.sidebar-mini.sidebar-collapse .sidebar-menu > li > a > i,
        body.sidebar-collapse .sidebar-menu > li > a > i {
          display: inline-block !important;
          visibility: visible !important;
          opacity: 1 !important;
          width: 20px !important;
          min-width: 20px !important;
          margin: 0 !important;
          padding: 0 !important;
          text-align: center !important;
          font-size: 16px !important;
          line-height: 1 !important;
        }
        body.sidebar-mini.sidebar-collapse .sidebar-menu > li > a > span:not(.pull-right),
        body.sidebar-collapse .sidebar-menu > li > a > span:not(.pull-right) {
          display: none !important;
          position: absolute !important;
          left: 64px !important;
          top: 0 !important;
          min-width: 190px !important;
          height: 48px !important;
          line-height: 48px !important;
          padding: 0 16px !important;
          background: #FFFFFF !important;
          color: var(--pdrb-navy) !important;
          border-radius: 0 12px 12px 0 !important;
          box-shadow: 0 10px 26px rgba(18, 40, 58, .18) !important;
          font-size: 14px !important;
          font-weight: 650 !important;
          white-space: nowrap !important;
          z-index: 20000 !important;
        }
        body.sidebar-mini.sidebar-collapse .sidebar-menu > li:hover > a > span:not(.pull-right),
        body.sidebar-collapse .sidebar-menu > li:hover > a > span:not(.pull-right) {
          display: block !important;
        }
        body.sidebar-mini.sidebar-collapse .sidebar-menu > li > a > .pull-right-container,
        body.sidebar-collapse .sidebar-menu > li > a > .pull-right-container,
        body.sidebar-mini.sidebar-collapse .sidebar-menu > li > a > .pull-right,
        body.sidebar-collapse .sidebar-menu > li > a > .pull-right,
        body.sidebar-mini.sidebar-collapse .sidebar-menu > li > .treeview-menu,
        body.sidebar-collapse .sidebar-menu > li > .treeview-menu {
          display: none !important;
        }
      }

      @media (max-width: 991px) {
        .upload-requirements-grid { grid-template-columns: 1fr; }
      }




      /* OVERRIDE FINAL: logo transparan + collapse hanya logo */
      .main-header .logo,
      .skin-blue .main-header .logo {
        background: linear-gradient(90deg, #102B42 0%, #17324D 55%, #214A66 100%) !important;
        display: flex !important;
        align-items: center !important;
        justify-content: flex-start !important;
        padding: 0 14px !important;
        overflow: hidden !important;
      }
      .main-header .logo .pdrb-logo-full {
        display: inline-flex !important;
        align-items: center !important;
        gap: 10px !important;
      }
      .main-header .logo .pdrb-logo-mini {
        display: none !important;
      }
      .main-header .logo .app-logo-img {
        width: 34px !important;
        height: 34px !important;
        object-fit: contain !important;
        background: transparent !important;
        border: 0 !important;
        box-shadow: none !important;
      }
      .main-header .logo .app-logo-text {
        color: #FFFFFF !important;
        font-size: 16px !important;
        font-weight: 800 !important;
        white-space: nowrap !important;
      }
      @media (min-width: 768px) {
        body.sidebar-mini.sidebar-collapse .main-header .logo,
        body.sidebar-collapse .main-header .logo {
          width: 64px !important;
          min-width: 64px !important;
          max-width: 64px !important;
          padding: 0 !important;
          justify-content: center !important;
        }
        body.sidebar-mini.sidebar-collapse .main-header .logo .pdrb-logo-full,
        body.sidebar-collapse .main-header .logo .pdrb-logo-full {
          display: none !important;
        }
        body.sidebar-mini.sidebar-collapse .main-header .logo .pdrb-logo-mini,
        body.sidebar-collapse .main-header .logo .pdrb-logo-mini {
          display: inline-flex !important;
          align-items: center !important;
          justify-content: center !important;
          width: 64px !important;
          height: 50px !important;
        }
        body.sidebar-mini.sidebar-collapse .main-header .logo .app-logo-img,
        body.sidebar-collapse .main-header .logo .app-logo-img {
          width: 34px !important;
          height: 34px !important;
        }
        body.sidebar-mini.sidebar-collapse .main-header .navbar,
        body.sidebar-collapse .main-header .navbar,
        body.sidebar-mini.sidebar-collapse .content-wrapper,
        body.sidebar-collapse .content-wrapper,
        body.sidebar-mini.sidebar-collapse .right-side,
        body.sidebar-collapse .right-side,
        body.sidebar-mini.sidebar-collapse .main-footer,
        body.sidebar-collapse .main-footer {
          margin-left: 64px !important;
        }
      }

      /* Tabel Data */
      .tabel-filter-panel {
        position: relative;
        z-index: 25;
        padding: 6px 2px 18px;
        overflow: visible !important;
      }
      .tabel-filter-panel .row {
        margin-bottom: 12px;
      }
      .tabel-filter-panel .row:after {
        content: '';
        display: table;
        clear: both;
      }
      .tabel-filter-panel [class*='col-sm-'] {
        margin-bottom: 8px;
        overflow: visible !important;
      }
      .tabel-filter-panel .form-group {
        margin-bottom: 14px;
      }
      .tabel-filter-panel label {
        color: var(--pdrb-ink);
        font-size: 12.5px;
        font-weight: 700;
        margin-bottom: 6px;
      }
      .tabel-filter-panel .selectize-control {
        margin-bottom: 0;
        z-index: 30;
      }
      .tabel-filter-panel .selectize-control.dropdown-active {
        z-index: 12000;
      }
      .tabel-filter-panel .selectize-input {
        min-height: 39px;
        border-radius: 8px;
        border-color: #D7DFE2;
        box-shadow: none;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      .tabel-filter-panel .selectize-input > input {
        min-width: 90px !important;
      }
      .tabel-filter-panel .selectize-dropdown {
        z-index: 14000 !important;
        max-height: 250px;
        overflow-y: auto;
        border-radius: 9px;
        box-shadow: 0 12px 28px rgba(31, 41, 55, .18);
      }
      .tabel-filter-panel .selectize-dropdown .option,
      .tabel-filter-panel .selectize-dropdown .optgroup-header {
        white-space: normal;
        line-height: 1.35;
      }
      .tabel-filter-section-title {
        display: flex;
        align-items: center;
        gap: 8px;
        margin: 8px 0 10px;
        padding-top: 6px;
        color: var(--pdrb-navy);
        font-size: 13px;
        font-weight: 800;
      }
      .tabel-soft-note {
        margin-top: 8px;
        margin-bottom: 8px;
      }
      .table-action-note {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        margin-bottom: 12px;
        padding: 9px 12px;
        border-radius: 10px;
        background: var(--pdrb-soft-blue);
        color: var(--pdrb-navy);
        font-size: 12.5px;
        font-weight: 600;
      }
      .dataTables_wrapper .dt-buttons .btn,
      .dataTables_wrapper .dt-buttons button {
        border-radius: 7px !important;
        margin-right: 5px;
      }
      .dataTables_wrapper table.dataTable thead th {
        white-space: nowrap;
      }
      .dataTables_wrapper table.dataTable tbody td {
        vertical-align: top;
      }
      body > .selectize-dropdown {
        z-index: 10000 !important;
      }



      .upload-revision-layout { align-items: stretch; }
      .upload-steps-column .step-card {
        min-height: 82px;
        margin-bottom: 12px;
        padding: 14px 15px;
      }
      .upload-steps-column .step-card:last-child { margin-bottom: 0; }
      .upload-main-box .box-body,
      .upload-structure-box .box-body { min-height: 300px; }
      .upload-main-actions {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 10px;
        margin-top: 8px;
      }
      .upload-main-actions .btn { width: 100%; white-space: normal; }
      .upload-primary-note {
        margin-top: 12px;
        padding: 12px 13px;
        border-radius: 10px;
        background: var(--pdrb-soft-teal);
        color: var(--pdrb-green);
        font-size: 12.5px;
        line-height: 1.45;
      }
      .structure-dynamic-filters { margin-top: 14px; }
      @media (max-width: 991px) {
        .upload-main-actions { grid-template-columns: 1fr; }
        .upload-steps-column .step-card { margin-bottom: 10px; }
      }

      /* Revisi tata letak: halaman panduan, upload, tabel, dan dropdown */
      #shiny-tab-penjelasan .page-intro {
        margin-bottom: 12px;
        padding: 17px 20px;
      }
      #shiny-tab-penjelasan .steps-row {
        margin-bottom: 14px;
      }
      #shiny-tab-penjelasan .step-card {
        min-height: 74px;
        padding: 13px 15px;
      }
      #shiny-tab-penjelasan .box {
        margin-bottom: 14px;
      }
      #shiny-tab-penjelasan .compact-guide-table th,
      #shiny-tab-penjelasan .compact-guide-table td {
        padding: 7px 9px;
        font-size: 12px;
        line-height: 1.32;
      }
      .upload-equal-row {
        display: flex;
        flex-wrap: wrap;
        align-items: stretch;
      }
      .upload-equal-row > [class*='col-sm-'] {
        display: flex;
      }
      .upload-equal-row .box {
        width: 100%;
        display: flex;
        flex-direction: column;
      }
      .upload-equal-row .box-body {
        flex: 1 1 auto;
      }
      .upload-equal-row .upload-requirements-grid {
        grid-template-columns: repeat(2, minmax(0, 1fr));
      }
      .upload-equal-row .requirement-card {
        min-height: 82px;
      }
      .tabel-filter-grid {
        margin: 0;
      }
      .tabel-filter-grid.tabel-filter-one-row {
        display: flex;
        flex-wrap: nowrap;
        align-items: flex-start;
        gap: 16px;
        width: 100%;
        overflow-x: auto;
        padding: 0 4px 2px;
      }
      .tabel-filter-grid.tabel-filter-one-row:before,
      .tabel-filter-grid.tabel-filter-one-row:after {
        content: none;
        display: none;
      }
      .tabel-filter-grid.tabel-filter-one-row > .tabel-filter-item {
        float: none !important;
        width: auto !important;
        flex: 1 1 0;
        min-width: 138px;
        padding-left: 0;
        padding-right: 0;
      }
      .tabel-filter-grid .form-group {
        margin-bottom: 0;
      }
      .tabel-filter-grid .col-sm-2:empty,
      .tabel-filter-grid .col-sm-3:empty,
      .tabel-filter-grid .col-sm-4:empty,
      .tabel-filter-grid .col-sm-6:empty {
        display: none;
      }
      @media (max-width: 1100px) {
        .tabel-filter-grid.tabel-filter-one-row {
          flex-wrap: wrap;
        }
        .tabel-filter-grid.tabel-filter-one-row > .tabel-filter-item {
          flex: 1 1 220px;
        }
      }
      .selectize-input {
        min-height: 36px;
      }

      /* Notifikasi */
      .shiny-notification {
        border-radius: 10px;
        box-shadow: 0 8px 24px rgba(31,41,55,.14);
      }

      .content-header { padding-bottom: 0; }

      @media (max-width: 991px) {
        .about-grid { grid-template-columns: 1fr; }
        .step-card { margin-bottom: 10px; }
      }
      @media (max-width: 767px) {
        .content { padding: 15px; }
        .page-intro { padding: 17px; }
        .page-intro h2 { font-size: 21px; }
        .hero-panel { padding: 20px; }
      }


      /* Modern compact layer */
      .main-header .logo { width: 270px !important; }
      .main-header .navbar { margin-left: 270px !important; }
      .main-sidebar, .left-side { width: 270px !important; }
      .content-wrapper, .right-side, .main-footer { margin-left: 270px !important; }
      .brand-title { font-weight: 800; letter-spacing: .15px; }
      .skin-blue .main-header .logo,
      .skin-blue .main-header .navbar {
        background: linear-gradient(90deg, #102B42 0%, #17324D 55%, #214A66 100%);
      }
      .skin-blue .main-sidebar {
        background: linear-gradient(180deg, #0F2435 0%, #132D41 54%, #102536 100%);
      }
      .content { padding: 20px 22px 34px; }
      .page-intro {
        align-items: center;
        padding: 16px 18px;
        margin-bottom: 14px;
        border-radius: 18px;
        background: linear-gradient(135deg, #FFFFFF 0%, #F8FBFA 100%);
        box-shadow: 0 10px 26px rgba(15, 36, 53, .07);
      }
      .page-intro-icon {
        width: 40px;
        height: 40px;
        flex-basis: 40px;
        border-radius: 13px;
        background: linear-gradient(135deg, #EEF4F8, #EDF6F4);
      }
      .page-intro h2 { font-size: 22px; margin-bottom: 2px; }
      .page-intro p { font-size: 12.5px; line-height: 1.35; }
      .eyebrow { font-size: 10px; letter-spacing: .12em; }
      .box {
        border-radius: 18px;
        border: 1px solid rgba(204, 216, 211, .95);
        box-shadow: 0 10px 28px rgba(15, 36, 53, .07);
      }
      .box.box-solid > .box-header { border-radius: 18px 18px 0 0; }
      .box-header { padding: 12px 16px; }
      .box-title { font-size: 14px; font-weight: 800; letter-spacing: -.05px; }
      .box-body { padding: 16px; }
      .hero-panel {
        border-radius: 20px;
        padding: 22px 24px;
        background: radial-gradient(circle at top right, rgba(226,190,106,.22) 0, rgba(226,190,106,0) 28%), linear-gradient(135deg, #102B42 0%, #235777 58%, #2D7F78 100%);
      }
      .hero-panel h2 { font-size: 23px; }
      .hero-panel p { font-size: 13px; }
      .dataset-badge, .palette-key { font-size: 11.5px; }
      .small-box {
        border-radius: 18px;
        box-shadow: 0 12px 28px rgba(15,36,53,.12);
      }
      .small-box .inner { padding: 14px 16px; }
      .small-box h3 { font-size: 23px; }
      .small-box p { font-size: 12px; font-weight: 650; }
      .steps-row { margin-bottom: 14px; }
      .step-card {
        min-height: 74px;
        border-radius: 16px;
        box-shadow: 0 8px 20px rgba(15,36,53,.055);
      }
      .step-copy p { font-size: 12px; margin-top: 2px; }
      .info-note, .privacy-note, .soft-note, .distribution-note, .filter-summary-note {
        border-radius: 13px;
        font-size: 12px;
        margin-bottom: 12px;
      }
      .compact-note { font-size: 12px; color: var(--pdrb-muted); line-height: 1.45; }
      .requirement-card p { font-size: 12px; line-height: 1.4; }
      .upload-card { border-radius: 16px; padding: 16px; }
      .btn { border-radius: 10px; }
      .nav-tabs-custom, .tabbable, .tab-content { border-radius: 16px; }
      .quick-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 14px;
        margin-bottom: 16px;
      }
      .quick-grid.two { grid-template-columns: repeat(2, minmax(0, 1fr)); }
      .quick-grid.three { grid-template-columns: repeat(3, minmax(0, 1fr)); }
      .quick-grid.four { grid-template-columns: repeat(4, minmax(0, 1fr)); }
      .quick-card {
        display: flex;
        gap: 12px;
        min-height: 94px;
        padding: 16px;
        border: 1px solid var(--pdrb-border);
        border-radius: 18px;
        background: #FFFFFF;
        box-shadow: 0 9px 24px rgba(15,36,53,.06);
      }
      .quick-card.compact { min-height: 76px; }
      .quick-card-icon {
        display: flex;
        align-items: center;
        justify-content: center;
        flex: 0 0 38px;
        width: 38px;
        height: 38px;
        border-radius: 13px;
        background: var(--pdrb-soft-teal);
        color: var(--pdrb-teal);
      }
      .quick-card-copy strong {
        display: block;
        color: var(--pdrb-navy);
        font-size: 13.5px;
        margin-bottom: 3px;
      }
      .quick-card-copy p {
        margin: 0;
        color: var(--pdrb-muted);
        font-size: 12px;
        line-height: 1.4;
      }
      .pill-row { display: flex; flex-wrap: wrap; gap: 8px; margin: 0 0 15px; }
      .pill-badge {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        padding: 8px 11px;
        border: 1px solid #DDE7E3;
        border-radius: 999px;
        background: #FFFFFF;
        color: var(--pdrb-navy);
        font-size: 12px;
        font-weight: 700;
        box-shadow: 0 5px 14px rgba(15,36,53,.045);
      }
      .pill-badge i { color: var(--pdrb-teal); }
      .mini-guide-panel {
        padding: 18px;
        border: 1px solid var(--pdrb-border);
        border-radius: 18px;
        background: linear-gradient(135deg, #FFFFFF 0%, #F8FBFA 100%);
        box-shadow: 0 10px 26px rgba(15,36,53,.06);
        margin-bottom: 16px;
      }
      .mini-guide-panel h4 {
        margin: 0 0 10px;
        color: var(--pdrb-navy);
        font-weight: 800;
      }
      .mini-guide-panel p {
        margin: 0;
        color: var(--pdrb-muted);
        font-size: 12.5px;
        line-height: 1.5;
      }
      .guide-metric {
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 11px 0;
        border-bottom: 1px solid #E8EEEB;
      }
      .guide-metric:last-child { border-bottom: 0; }
      .guide-metric strong { color: var(--pdrb-navy); font-size: 13px; }
      .guide-metric span { color: var(--pdrb-muted); font-size: 12px; text-align: right; }
      /* Menu Bantuan V9.17: layout panduan terarah dan responsif */
      .help-section-box { margin-bottom: 18px; }
      .help-section-box .box-body { padding: 18px; }
      .help-step-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 14px;
      }
      .help-step-card {
        min-height: 190px;
        padding: 18px;
        border: 1px solid var(--pdrb-border);
        border-radius: 17px;
        background: linear-gradient(145deg, #FFFFFF 0%, #F8FBFA 100%);
        box-shadow: 0 9px 24px rgba(15,36,53,.055);
      }
      .help-step-top {
        display: flex;
        align-items: center;
        justify-content: space-between;
        margin-bottom: 14px;
      }
      .help-step-number {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 32px;
        height: 32px;
        border-radius: 11px;
        background: var(--pdrb-navy);
        color: #FFFFFF;
        font-size: 13px;
        font-weight: 800;
      }
      .help-step-icon,
      .help-item-icon,
      .help-menu-icon {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 40px;
        height: 40px;
        border-radius: 13px;
        background: var(--pdrb-soft-teal);
        color: var(--pdrb-teal);
        flex: 0 0 40px;
      }
      .help-step-card h4 {
        margin: 0 0 8px;
        color: var(--pdrb-navy);
        font-size: 14px;
        font-weight: 800;
      }
      .help-step-card p,
      .help-item-copy p,
      .help-menu-copy p,
      .help-accordion-body p {
        margin: 0;
        color: var(--pdrb-muted);
        font-size: 12.5px;
        line-height: 1.5;
      }
      .help-step-note {
        display: block;
        margin-top: 12px;
        padding-top: 10px;
        border-top: 1px solid #E6EEEA;
        color: #2D6F69;
        font-size: 11.5px;
        font-weight: 700;
      }
      .help-two-column {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 16px;
      }
      .help-subpanel {
        padding: 16px;
        border: 1px solid var(--pdrb-border);
        border-radius: 16px;
        background: #FBFDFC;
      }
      .help-subpanel-title,
      .help-group-title {
        display: flex;
        align-items: center;
        gap: 8px;
        margin: 0 0 12px;
        color: var(--pdrb-navy);
        font-size: 13px;
        font-weight: 800;
      }
      .help-subpanel-title i,
      .help-group-title i { color: var(--pdrb-teal); }
      .help-item-list {
        display: grid;
        grid-template-columns: 1fr;
        gap: 10px;
      }
      .help-item-card {
        display: flex;
        align-items: flex-start;
        gap: 12px;
        padding: 13px;
        border: 1px solid #E3ECE8;
        border-radius: 14px;
        background: #FFFFFF;
      }
      .help-item-card.compact { min-height: 92px; }
      .help-item-copy strong,
      .help-menu-copy strong {
        display: block;
        margin-bottom: 3px;
        color: var(--pdrb-navy);
        font-size: 13px;
      }
      .help-template-actions {
        display: flex;
        flex-wrap: wrap;
        gap: 10px;
        margin-top: 16px;
        padding-top: 15px;
        border-top: 1px solid #E4ECE8;
      }
      .help-template-actions .btn {
        min-height: 42px;
        display: inline-flex;
        align-items: center;
        justify-content: center;
      }
      .help-menu-group { margin-bottom: 16px; }
      .help-menu-group:last-child { margin-bottom: 0; }
      .help-menu-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 11px;
      }
      .help-menu-card {
        display: flex;
        align-items: flex-start;
        gap: 12px;
        min-height: 92px;
        padding: 14px;
        border: 1px solid var(--pdrb-border);
        border-radius: 15px;
        background: #FFFFFF;
      }
      .help-indicator-groups {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 15px;
      }
      .help-indicator-group {
        padding: 15px;
        border: 1px solid var(--pdrb-border);
        border-radius: 16px;
        background: linear-gradient(145deg, #FFFFFF 0%, #F9FCFB 100%);
      }
      .help-indicator-group .help-item-card {
        padding: 11px 0;
        border: 0;
        border-bottom: 1px solid #E6EEEA;
        border-radius: 0;
        background: transparent;
      }
      .help-indicator-group .help-item-card:last-child { border-bottom: 0; }
      .help-accordion-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 11px;
        align-items: start;
      }
      .help-accordion-item {
        border: 1px solid var(--pdrb-border);
        border-radius: 14px;
        background: #FFFFFF;
        overflow: hidden;
        align-self: start;
        height: auto;
      }
      .help-accordion-item summary {
        display: flex;
        align-items: center;
        gap: 10px;
        padding: 14px 15px;
        color: var(--pdrb-navy);
        font-size: 13px;
        font-weight: 800;
        cursor: pointer;
        list-style: none;
      }
      .help-accordion-item summary::-webkit-details-marker { display: none; }
      .help-accordion-item summary > i:first-child { color: var(--pdrb-teal); }
      .help-accordion-item summary > .custom-location-icon {
        color: var(--pdrb-teal);
        flex: 0 0 18px;
      }
      .help-accordion-item summary > span:not(.custom-location-icon) { flex: 1; }
      .help-accordion-chevron { transition: transform .2s ease; color: var(--pdrb-muted); }
      .help-accordion-item[open] .help-accordion-chevron { transform: rotate(180deg); }
      .help-accordion-body {
        padding: 0 15px 15px 39px;
        border-top: 1px solid #EDF2F0;
      }
      .help-accordion-body p { padding-top: 12px; }
      .help-important-note {
        display: flex;
        align-items: flex-start;
        gap: 14px;
        padding: 18px;
        border: 1px solid #D9E7E2;
        border-radius: 17px;
        background: linear-gradient(135deg, #F3F8F6 0%, #FFFFFF 100%);
        margin-bottom: 16px;
      }
      .help-important-note > i {
        margin-top: 2px;
        color: var(--pdrb-teal);
        font-size: 20px;
      }
      .help-important-note strong {
        display: block;
        margin-bottom: 5px;
        color: var(--pdrb-navy);
        font-size: 13.5px;
      }
      .help-important-note p {
        margin: 0;
        color: var(--pdrb-muted);
        font-size: 12.5px;
        line-height: 1.5;
      }
      .help-note-grid {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 11px;
      }
      @media (max-width: 1100px) {
        .help-step-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
        .help-indicator-groups { grid-template-columns: 1fr; }
      }
      @media (max-width: 767px) {
        .help-step-grid,
        .help-two-column,
        .help-menu-grid,
        .help-accordion-grid,
        .help-note-grid { grid-template-columns: 1fr; }
        .help-template-actions .btn { width: 100%; }
      }

      .table-action-note { font-size: 12px; }
      .modern-sidebar-filter {
        padding-bottom: 24px;
      }
      .modern-sidebar-filter .filter-heading {
        margin-bottom: 10px;
        font-size: 12px;
        letter-spacing: .03em;
      }
      .modern-sidebar-filter .form-group { margin-bottom: 12px; }
      .modern-sidebar-filter .advanced-filter-details {
        margin-top: 12px;
        border-radius: 14px;
        background: rgba(255,255,255,.05);
        border: 1px solid rgba(255,255,255,.10);
        backdrop-filter: blur(4px);
      }
      .modern-sidebar-filter .advanced-filter-content {
        padding-top: 2px;
      }
      .sidebar-filter .selectize-input,
      .sidebar-filter .form-control,
      .content .selectize-input,
      .content .form-control,
      .content .selectize-control.single .selectize-input {
        min-height: 42px;
        border-radius: 12px;
        border: 1px solid #D8E3DE;
        box-shadow: none;
      }
      .sidebar-filter .selectize-input {
        background: rgba(255,255,255,.09);
        border-color: rgba(255,255,255,.10);
      }
      .sidebar-filter .selectize-input > input,
      .sidebar-filter .selectize-input.full,
      .sidebar-filter .item {
        color: #F4F8FA !important;
      }
      .sidebar-filter .selectize-control.single .selectize-input:after {
        border-color: #D6E0E6 transparent transparent transparent;
      }
      .content .selectize-input,
      .content .form-control,
      .content .selectize-control.single .selectize-input {
        background: linear-gradient(180deg, #FFFFFF 0%, #FBFDFC 100%);
      }
      .box, .page-intro, .quick-card, .mini-guide-panel, .step-card, .small-box {
        transition: none !important;
      }
      .box:hover, .page-intro:hover, .quick-card:hover, .mini-guide-panel:hover, .step-card:hover, .small-box:hover {
        transform: none !important;
      }
      .nav-tabs-custom > .nav-tabs > li.active {
        border-top-color: transparent;
      }
      .nav-tabs-custom > .nav-tabs > li > a {
        border-radius: 10px 10px 0 0;
        font-weight: 600;
      }
      .nav-tabs-custom > .nav-tabs > li.active > a {
        background: #FFFFFF;
        color: var(--pdrb-navy);
      }
      .page-intro-copy p:empty { display: none; }
      .page-intro-copy p { margin-top: 2px; }
      @media (min-width: 768px) {
        body.sidebar-collapse .main-header .logo {
          width: 54px !important;
        }
        body.sidebar-collapse .main-sidebar,
        body.sidebar-collapse .left-side {
          width: 54px !important;
        }
        body.sidebar-collapse .main-header .navbar {
          margin-left: 54px !important;
        }
        body.sidebar-collapse .content-wrapper,
        body.sidebar-collapse .right-side,
        body.sidebar-collapse .main-footer {
          margin-left: 54px !important;
        }
        body.sidebar-collapse .main-header .logo {
          padding: 0 !important;
          text-align: center !important;
        }
        body.sidebar-collapse .main-header .logo .brand-title {
          display: none !important;
        }
        body.sidebar-collapse .main-header .logo .brand-mark {
          margin-right: 0 !important;
        }
        body.sidebar-collapse .sidebar-menu > li.header.sidebar-section-label,
        body.sidebar-collapse .sidebar-divider,
        body.sidebar-collapse .modern-sidebar-filter,
        body.sidebar-collapse .sidebar-filter {
          display: none !important;
        }
        body.sidebar-collapse .sidebar-menu > li > a {
          padding: 13px 0 !important;
          text-align: center !important;
        }
        body.sidebar-collapse .sidebar-menu > li > a > .fa {
          width: 54px !important;
          margin-right: 0 !important;
          text-align: center !important;
          font-size: 15px;
        }
        body.sidebar-collapse .sidebar-menu > li > a > span,
        body.sidebar-collapse .sidebar-menu > li > a > .pull-right-container,
        body.sidebar-collapse .sidebar-menu > li > a > .pull-right {
          display: none !important;
        }
        .sidebar-mini.sidebar-collapse .sidebar-menu > li:hover > a > span:not(.pull-right),
        .sidebar-mini.sidebar-collapse .sidebar-menu > li:hover > .treeview-menu {
          display: none !important;
        }
      }
      @media (max-width: 991px) {
        .content-wrapper, .right-side, .main-footer { margin-left: 0 !important; }
        .main-header .navbar { margin-left: 0 !important; }
        .quick-grid, .quick-grid.two, .quick-grid.three, .quick-grid.four { grid-template-columns: 1fr; }
      }



      /* FINAL FIX 2026-06-30: logo header stabil saat collapse */
      .main-header {
        position: relative !important;
        z-index: 1030 !important;
      }
      .main-header .logo,
      .skin-blue .main-header .logo {
        width: 270px !important;
        min-width: 270px !important;
        max-width: 270px !important;
        height: 50px !important;
        padding: 0 14px !important;
        display: flex !important;
        align-items: center !important;
        justify-content: flex-start !important;
        overflow: hidden !important;
        position: relative !important;
        z-index: 1100 !important;
        background: linear-gradient(90deg, #102B42 0%, #17324D 60%, #214A66 100%) !important;
      }
      .main-header .logo .logo-lg,
      .main-header .logo .pdrb-logo-full {
        display: flex !important;
        align-items: center !important;
        gap: 10px !important;
        width: 100% !important;
        height: 50px !important;
        line-height: 1 !important;
        overflow: hidden !important;
      }
      .main-header .logo .logo-mini,
      .main-header .logo .pdrb-logo-mini {
        display: none !important;
      }
      .main-header .logo .app-logo-img {
        width: 34px !important;
        height: 34px !important;
        min-width: 34px !important;
        max-width: 34px !important;
        object-fit: contain !important;
        object-position: center !important;
        display: block !important;
        background: transparent !important;
        border: 0 !important;
        box-shadow: none !important;
      }
      .main-header .logo .app-logo-text {
        display: inline-block !important;
        color: #FFFFFF !important;
        font-size: 16px !important;
        font-weight: 800 !important;
        letter-spacing: .15px !important;
        white-space: nowrap !important;
        line-height: 1 !important;
      }

      @media (min-width: 768px) {
        body.sidebar-mini.sidebar-collapse .main-header .logo,
        body.sidebar-collapse .main-header .logo,
        body.sidebar-mini.sidebar-collapse .skin-blue .main-header .logo,
        body.sidebar-collapse .skin-blue .main-header .logo {
          width: 64px !important;
          min-width: 64px !important;
          max-width: 64px !important;
          height: 50px !important;
          padding: 0 !important;
          display: flex !important;
          align-items: center !important;
          justify-content: center !important;
          overflow: hidden !important;
          background: #102B42 !important;
          z-index: 1200 !important;
        }
        body.sidebar-mini.sidebar-collapse .main-header .logo .logo-lg,
        body.sidebar-collapse .main-header .logo .logo-lg,
        body.sidebar-mini.sidebar-collapse .main-header .logo .pdrb-logo-full,
        body.sidebar-collapse .main-header .logo .pdrb-logo-full,
        body.sidebar-mini.sidebar-collapse .main-header .logo .app-logo-text,
        body.sidebar-collapse .main-header .logo .app-logo-text {
          display: none !important;
          width: 0 !important;
          max-width: 0 !important;
          opacity: 0 !important;
          visibility: hidden !important;
        }
        body.sidebar-mini.sidebar-collapse .main-header .logo .logo-mini,
        body.sidebar-collapse .main-header .logo .logo-mini,
        body.sidebar-mini.sidebar-collapse .main-header .logo .pdrb-logo-mini,
        body.sidebar-collapse .main-header .logo .pdrb-logo-mini {
          display: flex !important;
          align-items: center !important;
          justify-content: center !important;
          width: 64px !important;
          height: 50px !important;
          min-width: 64px !important;
          margin: 0 !important;
          padding: 0 !important;
          opacity: 1 !important;
          visibility: visible !important;
        }
        body.sidebar-mini.sidebar-collapse .main-header .logo .app-logo-img,
        body.sidebar-collapse .main-header .logo .app-logo-img {
          width: 36px !important;
          height: 36px !important;
          min-width: 36px !important;
          max-width: 36px !important;
          margin: 0 auto !important;
        }
        body.sidebar-mini.sidebar-collapse .main-header .navbar,
        body.sidebar-collapse .main-header .navbar {
          margin-left: 64px !important;
        }
        body.sidebar-mini.sidebar-collapse .main-sidebar,
        body.sidebar-collapse .main-sidebar,
        body.sidebar-mini.sidebar-collapse .left-side,
        body.sidebar-collapse .left-side,
        body.sidebar-mini.sidebar-collapse .main-sidebar:hover,
        body.sidebar-collapse .main-sidebar:hover {
          width: 64px !important;
        }
        body.sidebar-mini.sidebar-collapse .content-wrapper,
        body.sidebar-collapse .content-wrapper,
        body.sidebar-mini.sidebar-collapse .right-side,
        body.sidebar-collapse .right-side,
        body.sidebar-mini.sidebar-collapse .main-footer,
        body.sidebar-collapse .main-footer {
          margin-left: 64px !important;
        }
      }



      /* OVERRIDE TERAKHIR: logo header pakai pseudo-element agar tetap muncul saat collapse */
      .skin-blue .main-header .logo,
      .main-header .logo {
        position: relative !important;
        width: 270px !important;
        min-width: 270px !important;
        max-width: 270px !important;
        height: 50px !important;
        line-height: 50px !important;
        padding: 0 14px 0 58px !important;
        overflow: hidden !important;
        background: var(--pdrb-navy) !important;
        color: #FFFFFF !important;
        text-align: left !important;
        white-space: nowrap !important;
        z-index: 3000 !important;
      }
      .main-header .logo:before {
        content: '' !important;
        position: absolute !important;
        left: 16px !important;
        top: 8px !important;
        width: 34px !important;
        height: 34px !important;
        display: block !important;
        background-image: url('logo_eksplorasi_pdrb.png') !important;
        background-repeat: no-repeat !important;
        background-position: center center !important;
        background-size: contain !important;
        z-index: 3002 !important;
        pointer-events: none !important;
      }
      .main-header .logo .logo-mini,
      .main-header .logo .pdrb-logo-mini,
      .main-header .logo .app-logo-img {
        display: none !important;
      }
      .main-header .logo .logo-lg,
      .main-header .logo .pdrb-logo-full,
      .main-header .logo .app-logo-title {
        display: inline-block !important;
        visibility: visible !important;
        opacity: 1 !important;
        color: #FFFFFF !important;
        font-size: 16px !important;
        font-weight: 800 !important;
        letter-spacing: .2px !important;
        line-height: 50px !important;
        white-space: nowrap !important;
        text-align: left !important;
      }

      @media (min-width: 768px) {
        body.sidebar-mini.sidebar-collapse .main-header .logo,
        body.sidebar-collapse .main-header .logo {
          position: fixed !important;
          left: 0 !important;
          top: 0 !important;
          width: 64px !important;
          min-width: 64px !important;
          max-width: 64px !important;
          height: 50px !important;
          padding: 0 !important;
          margin: 0 !important;
          overflow: visible !important;
          background: var(--pdrb-navy) !important;
          z-index: 5000 !important;
          text-align: center !important;
        }
        body.sidebar-mini.sidebar-collapse .main-header .logo:before,
        body.sidebar-collapse .main-header .logo:before {
          content: '' !important;
          position: absolute !important;
          left: 15px !important;
          top: 8px !important;
          width: 34px !important;
          height: 34px !important;
          display: block !important;
          visibility: visible !important;
          opacity: 1 !important;
          background-image: url('logo_eksplorasi_pdrb.png') !important;
          background-repeat: no-repeat !important;
          background-position: center center !important;
          background-size: contain !important;
          z-index: 5002 !important;
        }
        body.sidebar-mini.sidebar-collapse .main-header .logo *,
        body.sidebar-collapse .main-header .logo * {
          display: none !important;
          visibility: hidden !important;
          opacity: 0 !important;
        }
        body.sidebar-mini.sidebar-collapse .main-header .navbar,
        body.sidebar-collapse .main-header .navbar {
          margin-left: 64px !important;
        }
      }



      /* SOLUSI FINAL: logo collapse dibuat elemen overlay, tidak bergantung pada .main-header .logo */
      #collapse-logo-overlay {
        display: none !important;
        position: fixed !important;
        left: 0 !important;
        top: 0 !important;
        width: 70px !important;
        height: 50px !important;
        align-items: center !important;
        justify-content: center !important;
        background: #12283A !important;
        z-index: 999999 !important;
        pointer-events: none !important;
      }
      #collapse-logo-overlay img {
        width: 36px !important;
        height: 36px !important;
        object-fit: contain !important;
        display: block !important;
      }
      body.sidebar-mini.sidebar-collapse #collapse-logo-overlay,
      body.sidebar-collapse #collapse-logo-overlay {
        display: flex !important;
      }
      @media (max-width: 767px) {
        #collapse-logo-overlay { display: none !important; }
      }



      /* FIX FINAL: selectize/dropdown agar tidak terlihat dobel dan tidak kalah z-index */
      .content .form-group {
        margin-bottom: 16px !important;
      }

      .content .selectize-control {
        width: 100% !important;
        margin: 0 !important;
        padding: 0 !important;
        line-height: normal !important;
        box-shadow: none !important;
        background: transparent !important;
      }

      .content .selectize-control.single .selectize-input,
      .content .selectize-control.multi .selectize-input,
      .content .selectize-input {
        min-height: 42px !important;
        height: 42px !important;
        padding: 10px 38px 10px 14px !important;
        border: 1px solid #DCE6E2 !important;
        border-radius: 10px !important;
        background: #FFFFFF !important;
        background-image: none !important;
        box-shadow: none !important;
        -webkit-box-shadow: none !important;
        outline: 0 !important;
        display: flex !important;
        align-items: center !important;
        overflow: hidden !important;
      }

      .content .selectize-input:before {
        display: none !important;
        content: none !important;
      }

      .content .selectize-input > input {
        height: 20px !important;
        line-height: 20px !important;
        margin: 0 !important;
        padding: 0 !important;
      }

      .content .selectize-input.items.has-options.full.has-items,
      .content .selectize-input.items.full.has-options.has-items {
        box-shadow: none !important;
        -webkit-box-shadow: none !important;
      }

      .content .selectize-input.focus,
      .content .selectize-input.input-active,
      .content .selectize-control.single .selectize-input.input-active {
        border-color: #2D7F78 !important;
        box-shadow: 0 0 0 3px rgba(45, 127, 120, .10) !important;
        -webkit-box-shadow: 0 0 0 3px rgba(45, 127, 120, .10) !important;
      }

      .content .selectize-control.single .selectize-input:after {
        right: 14px !important;
        border-color: #333333 transparent transparent transparent !important;
      }

      .content .selectize-control.single .selectize-input.dropdown-active:after {
        border-color: transparent transparent #333333 transparent !important;
      }

      body > .selectize-dropdown,
      .content .selectize-dropdown,
      .selectize-dropdown {
        z-index: 999999 !important;
        border: 1px solid #DCE6E2 !important;
        border-radius: 10px !important;
        background: #FFFFFF !important;
        box-shadow: 0 12px 28px rgba(31, 41, 55, .16) !important;
        -webkit-box-shadow: 0 12px 28px rgba(31, 41, 55, .16) !important;
        overflow: hidden !important;
        max-height: 190px !important;
      }

      body > .selectize-dropdown .selectize-dropdown-content,
      .selectize-dropdown .selectize-dropdown-content {
        max-height: 190px !important;
        overflow-y: auto !important;
      }

      .selectize-dropdown .option,
      .selectize-dropdown .optgroup-header {
        padding: 9px 13px !important;
        line-height: 1.35 !important;
      }

      .selectize-dropdown .active,
      .selectize-dropdown .option.active {
        background: #2C5F8A !important;
        color: #FFFFFF !important;
      }

      /* Menambah jarak supaya dropdown tidak langsung menimpa card ringkasan di bawah filter */
      .filter-panel,
      .distribution-period-filter,
      .structure-period-filter,
      .comparison-period-filter {
        margin-bottom: 22px !important;
        overflow: visible !important;
      }

      .filter-panel .box,
      .filter-panel .box-body,
      .distribution-period-filter .box,
      .distribution-period-filter .box-body,
      .structure-period-filter .box,
      .structure-period-filter .box-body,
      .comparison-period-filter .box,
      .comparison-period-filter .box-body {
        overflow: visible !important;
      }



      /* FIX STABIL: dropdown tidak membuat card berkedip */
      /* Tidak ada JS margin dinamis. Ruang filter dibuat tetap agar layout tidak reflow bolak-balik. */
      @supports selector(:has(*)) {
        .content .box.box-solid.box-info:has(.selectize-control) > .box-body,
        .content .box.box-solid.box-info:has(.form-control) > .box-body {
          padding-bottom: 115px !important;
        }

        .content .box.box-solid.box-info:has(.selectize-control),
        .content .box.box-solid.box-info:has(.form-control) {
          margin-bottom: 22px !important;
        }
      }

      .content .selectize-control {
        width: 100% !important;
        margin: 0 !important;
        padding: 0 !important;
        line-height: normal !important;
        box-shadow: none !important;
        background: transparent !important;
      }

      .content .selectize-control.single .selectize-input,
      .content .selectize-control.multi .selectize-input,
      .content .selectize-input {
        min-height: 42px !important;
        height: 42px !important;
        padding: 10px 38px 10px 14px !important;
        border: 1px solid #DCE6E2 !important;
        border-radius: 10px !important;
        background: #FFFFFF !important;
        background-image: none !important;
        box-shadow: none !important;
        -webkit-box-shadow: none !important;
        outline: 0 !important;
        display: flex !important;
        align-items: center !important;
        overflow: hidden !important;
      }

      .content .selectize-input:before {
        display: none !important;
        content: none !important;
      }

      .content .selectize-input.focus,
      .content .selectize-input.input-active,
      .content .selectize-control.single .selectize-input.input-active {
        border-color: #2D7F78 !important;
        box-shadow: 0 0 0 3px rgba(45, 127, 120, .10) !important;
        -webkit-box-shadow: 0 0 0 3px rgba(45, 127, 120, .10) !important;
      }

      .content .selectize-control.single .selectize-input:after {
        right: 14px !important;
        border-color: #333333 transparent transparent transparent !important;
      }

      .content .selectize-control.single .selectize-input.dropdown-active:after {
        border-color: transparent transparent #333333 transparent !important;
      }

      body > .selectize-dropdown,
      .selectize-dropdown {
        z-index: 999999 !important;
        border: 1px solid #DCE6E2 !important;
        border-radius: 10px !important;
        background: #FFFFFF !important;
        box-shadow: 0 12px 28px rgba(31, 41, 55, .16) !important;
        -webkit-box-shadow: 0 12px 28px rgba(31, 41, 55, .16) !important;
        overflow: hidden !important;
        max-height: 145px !important;
      }

      body > .selectize-dropdown .selectize-dropdown-content,
      .selectize-dropdown .selectize-dropdown-content {
        max-height: 145px !important;
        overflow-y: auto !important;
      }

      .selectize-dropdown .option,
      .selectize-dropdown .optgroup-header {
        padding: 9px 13px !important;
        line-height: 1.35 !important;
      }

      .selectize-dropdown .active,
      .selectize-dropdown .option.active {
        background: #2C5F8A !important;
        color: #FFFFFF !important;
      }



      /* FIX COMPACT: filter tidak dipanjangkan permanen */
      @supports selector(:has(*)) {
        .content .box.box-solid.box-info:has(.selectize-control) > .box-body,
        .content .box.box-solid.box-info:has(.form-control) > .box-body {
          padding-bottom: 17px !important;
        }

        .content .box.box-solid.box-info:has(.selectize-control.dropdown-active) > .box-body {
          padding-bottom: 92px !important;
        }

        .content .box.box-solid.box-info:has(.selectize-control.dropdown-active) {
          margin-bottom: 22px !important;
        }
      }

      body > .selectize-dropdown,
      .selectize-dropdown {
        max-height: 115px !important;
      }

      body > .selectize-dropdown .selectize-dropdown-content,
      .selectize-dropdown .selectize-dropdown-content {
        max-height: 115px !important;
        overflow-y: auto !important;
      }

      /* V8.2: dropdown Potensi Wilayah tidak boleh memicu reflow/kedip.
         Dropdown sudah ditempel ke body, jadi card tidak perlu berubah tinggi
         ketika Selectize dibuka atau pilihan diperbarui. */
      .potential-method-stack,
      .potential-method-stack .form-group,
      .potential-method-stack .selectize-control,
      .potential-method-stack .selectize-input {
        transition: none !important;
        animation: none !important;
      }
      @supports selector(:has(*)) {
        .content .box.box-solid.box-info:has(.potential-method-stack .selectize-control.dropdown-active) > .box-body {
          padding-bottom: 17px !important;
        }
        .content .box.box-solid.box-info:has(.potential-method-stack .selectize-control.dropdown-active) {
          margin-bottom: 22px !important;
        }
      }

      /* FIX FINAL KPI: cegah valueBox overflow/kedip, terutama card distribusi paling kanan */
      .summary-valuebox-row .small-box,
      .distribution-valuebox-row .small-box,
      .trend-valuebox-row .small-box {
        position: relative !important;
        overflow: hidden !important;
        min-width: 0 !important;
        box-sizing: border-box !important;
        transform: none !important;
      }

      .summary-valuebox-row .small-box *,
      .distribution-valuebox-row .small-box *,
      .trend-valuebox-row .small-box * {
        box-sizing: border-box !important;
      }

      .summary-valuebox-row .small-box .inner,
      .distribution-valuebox-row .small-box .inner,
      .trend-valuebox-row .small-box .inner {
        position: relative !important;
        z-index: 2 !important;
        min-width: 0 !important;
        padding-right: 78px !important;
      }

      .summary-valuebox-row .small-box h3,
      .summary-valuebox-row .small-box p,
      .distribution-valuebox-row .small-box h3,
      .distribution-valuebox-row .small-box p,
      .trend-valuebox-row .small-box h3,
      .trend-valuebox-row .small-box p {
        max-width: 100% !important;
        white-space: nowrap !important;
        overflow: hidden !important;
        text-overflow: ellipsis !important;
      }

      .summary-valuebox-row .small-box .icon,
      .distribution-valuebox-row .small-box .icon,
      .trend-valuebox-row .small-box .icon {
        pointer-events: none !important;
        right: 14px !important;
        top: 50% !important;
        transform: translateY(-50%) !important;
        font-size: 52px !important;
        opacity: .13 !important;
        max-width: 64px !important;
        overflow: hidden !important;
      }

      .distribution-valuebox-row .small-box h3 {
        font-size: 20px !important;
      }

      /* NO HOVER EFFECT: matikan efek gerak/bayangan saat kursor diarahkan ke card/panel */
      .box,
      .page-intro,
      .quick-card,
      .mini-guide-panel,
      .step-card,
      .small-box,
      .hero-panel,
      .pill-badge,
      .info-note,
      .privacy-note,
      .soft-note,
      .filter-summary-note,
      .structure-filter-hint,
      .distribution-note,
      .advanced-filter-details,
      .nav-tabs-custom,
      .tab-pane .box {
        transition: none !important;
        animation: none !important;
      }

      .box:hover,
      .page-intro:hover,
      .quick-card:hover,
      .mini-guide-panel:hover,
      .step-card:hover,
      .small-box:hover,
      .hero-panel:hover,
      .pill-badge:hover,
      .info-note:hover,
      .privacy-note:hover,
      .soft-note:hover,
      .filter-summary-note:hover,
      .structure-filter-hint:hover,
      .distribution-note:hover,
      .advanced-filter-details:hover,
      .nav-tabs-custom:hover,
      .tab-pane .box:hover {
        transform: none !important;
        animation: none !important;
      }

      .box:hover { box-shadow: 0 10px 28px rgba(15, 36, 53, .07) !important; }
      .page-intro:hover { box-shadow: 0 10px 26px rgba(15, 36, 53, .07) !important; }
      .quick-card:hover { box-shadow: 0 9px 24px rgba(15,36,53,.06) !important; }
      .mini-guide-panel:hover { box-shadow: 0 10px 26px rgba(15,36,53,.06) !important; }
      .step-card:hover { box-shadow: 0 8px 20px rgba(15,36,53,.055) !important; }
      .small-box:hover { box-shadow: 0 12px 28px rgba(15,36,53,.12) !important; }




      /* Samakan background area logo header dengan navbar gradien */
      .skin-blue .main-header .logo,
      .main-header .logo {
        background: linear-gradient(90deg, #102B42 0%, #17324D 55%, #214A66 100%) !important;
      }


      /* Samakan background header agar logo dan navbar menyatu */
      .skin-blue .main-header,
      .main-header {
        background: linear-gradient(90deg, #102B42 0%, #17324D 55%, #214A66 100%) !important;
      }
      .skin-blue .main-header .logo,
      .main-header .logo,
      .skin-blue .main-header .navbar,
      .main-header .navbar {
        background: transparent !important;
      }
      .skin-blue .main-header .logo:hover,
      .main-header .logo:hover,
      .skin-blue .main-header .navbar:hover,
      .main-header .navbar:hover,
      .skin-blue .main-header .navbar .sidebar-toggle:hover {
        background: transparent !important;
      }


      /* Header: logo dan navbar tidak disatukan, hanya disamakan warnanya */
      .skin-blue .main-header,
      .main-header {
        background: transparent !important;
      }
      .skin-blue .main-header .logo,
      .main-header .logo,
      .skin-blue .main-header .navbar,
      .main-header .navbar {
        background: #14324A !important;
      }
      .skin-blue .main-header .logo:hover,
      .main-header .logo:hover,
      .skin-blue .main-header .navbar:hover,
      .main-header .navbar:hover {
        background: #14324A !important;
      }
      .skin-blue .main-header .navbar .sidebar-toggle:hover {
        background: rgba(255,255,255,.06) !important;
      }


      /* Final: samakan warna header sebelum dan sesudah collapse */
      .skin-blue .main-header,
      .main-header,
      .skin-blue .main-header .logo,
      .main-header .logo,
      .skin-blue .main-header .navbar,
      .main-header .navbar,
      #collapse-logo-overlay {
        background: #14324A !important;
        border-bottom: 0 !important;
        box-shadow: none !important;
      }
      body.sidebar-mini.sidebar-collapse .main-header,
      body.sidebar-collapse .main-header,
      body.sidebar-mini.sidebar-collapse .main-header .logo,
      body.sidebar-collapse .main-header .logo,
      body.sidebar-mini.sidebar-collapse .main-header .navbar,
      body.sidebar-collapse .main-header .navbar,
      body.sidebar-mini.sidebar-collapse #collapse-logo-overlay,
      body.sidebar-collapse #collapse-logo-overlay {
        background: #14324A !important;
      }
      .skin-blue .main-header .logo:hover,
      .main-header .logo:hover,
      .skin-blue .main-header .navbar:hover,
      .main-header .navbar:hover,
      .skin-blue .main-header .navbar .sidebar-toggle:hover,
      .main-header .navbar .sidebar-toggle:hover {
        background: #14324A !important;
      }

      /* NO HOVER GLOBAL: hilangkan perubahan visual saat hover pada header, sidebar, tab, dan tombol umum */
      .skin-blue .main-header .logo:hover,
      .skin-blue .main-header .navbar:hover {
        background: linear-gradient(90deg, #102B42 0%, #17324D 55%, #214A66 100%) !important;
      }

      .skin-blue .main-header .navbar .sidebar-toggle:hover {
        background: transparent !important;
        color: inherit !important;
      }

      .skin-blue .sidebar-menu > li:hover > a {
        color: #D5E0E7 !important;
        background: transparent !important;
        border-left-color: transparent !important;
      }

      .skin-blue .sidebar-menu > li.active:hover > a {
        color: #FFFFFF !important;
        background: rgba(196,154,69,.13) !important;
        border-left-color: var(--pdrb-gold) !important;
      }

      .nav-tabs-custom > .nav-tabs > li > a:hover,
      .btn:hover,
      .dt-buttons .btn:hover {
        transform: none !important;
        transition: none !important;
      }


      /* Revisi Upload Data lama */
      .upload-revision-layout .upload-requirements-grid {
        display: grid;
        grid-template-columns: 1fr !important;
        gap: 10px !important;
        margin-top: 11px;
      }
      .upload-revision-layout .requirement-card {
        min-height: 78px;
        padding: 12px 14px;
      }
      .upload-revision-layout .requirement-card strong {
        font-size: 13px;
        margin-bottom: 4px;
      }
      .upload-revision-layout .requirement-card p {
        font-size: 11.5px;
        line-height: 1.38;
      }
      .compact-upload-card .upload-main-actions {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr)) !important;
        gap: 10px;
        margin: 0 0 14px 0;
      }
      .compact-upload-card .upload-main-actions .btn {
        width: 100%;
        min-height: 38px;
        white-space: nowrap !important;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      .compact-upload-card .form-group {
        margin-bottom: 10px;
      }
      @media (max-width: 767px) {
        .compact-upload-card .upload-main-actions {
          grid-template-columns: 1fr !important;
        }
      }


      /* Upload manual: pilih dua kelompok file, lalu proses satu kali */
      .upload-process-box .box-body {
        min-height: 0 !important;
      }
      .upload-process-card {
        border: 0 !important;
        padding: 6px 4px 2px !important;
      }
      .upload-dual-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 18px;
        align-items: stretch;
      }
      .upload-file-block {
        border: 1px solid #d8e4e9;
        border-radius: 14px;
        padding: 16px 16px 10px;
        background: #fbfdfd;
        min-width: 0;
      }
      .upload-file-heading {
        display: flex;
        align-items: flex-start;
        gap: 10px;
        margin-bottom: 10px;
        color: #123b5b;
      }
      .upload-file-heading > i {
        margin-top: 3px;
        font-size: 18px;
        color: #23857d;
      }
      .upload-file-heading > i.upload-location-icon {
        width: 18px;
        height: 18px;
        display: inline-block;
        font-size: 0 !important;
        line-height: 0 !important;
        background-color: #23857d;
        -webkit-mask: url('tanda_lokasi_sidebar_tebal.png') center / contain no-repeat;
        mask: url('tanda_lokasi_sidebar_tebal.png') center / contain no-repeat;
        margin-top: 2px;
      }
      .upload-file-heading strong,
      .upload-file-heading span {
        display: block;
      }
      .upload-file-heading strong {
        font-size: 14px;
        margin-bottom: 3px;
      }
      .upload-file-heading span {
        color: var(--pdrb-muted);
        font-size: 12px;
        line-height: 1.4;
      }
      .upload-process-footer {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 18px;
        margin-top: 16px;
        padding: 0;
        border: 0;
        border-radius: 0;
        background: transparent;
      }
      .upload-process-note {
        display: flex;
        align-items: flex-start;
        gap: 8px;
        color: #31565f;
        font-size: 12.5px;
        line-height: 1.45;
      }
      .upload-process-note i {
        margin-top: 2px;
        color: #23857d;
      }
      #process_pdrb {
        min-width: 190px;
        border-radius: 10px;
        font-weight: 700;
        white-space: nowrap;
      }
      #process_pdrb:disabled,
      #process_pdrb.disabled {
        opacity: .55;
        cursor: not-allowed;
      }
      .upload-template-button-group {
        display: flex;
        align-items: center;
        gap: 10px;
        flex-wrap: wrap;
      }
      .upload-template-button-group .btn {
        border-radius: 9px;
        min-height: 44px;
        display: inline-flex;
        align-items: center;
        justify-content: center;
      }
      .upload-file-block .form-group > label,
      .upload-file-block .control-label {
        display: none !important;
      }
      .upload-file-block .input-group {
        display: table !important;
        width: 100%;
        border-collapse: separate;
      }
      .upload-file-block .input-group-btn {
        display: table-cell !important;
        width: 1%;
        white-space: nowrap;
        vertical-align: middle;
      }
      .upload-file-block .input-group-btn .btn,
      .upload-file-block .btn-file {
        height: 44px;
        min-height: 44px;
        padding: 10px 16px;
        display: inline-block !important;
        white-space: nowrap;
        overflow: visible;
        border-radius: 12px 0 0 12px !important;
        line-height: 22px;
      }
      .upload-file-block .form-control {
        display: table-cell !important;
        width: 100%;
        height: 44px;
        min-height: 44px;
        padding: 10px 14px;
        border-radius: 0 12px 12px 0 !important;
        line-height: 22px;
      }
      @media (max-width: 900px) {
        .upload-dual-grid { grid-template-columns: 1fr; }
        .upload-process-footer { align-items: stretch; flex-direction: column; }
        #process_pdrb { width: 100%; }
        .upload-template-button-group { width: 100%; }
        .upload-template-button-group .btn { width: 100%; }
      }

      /* Pendekkan area garis putus-putus pada card Upload */
      .upload-card.compact-upload-card {
        padding-bottom: 8px !important;
      }
      .compact-upload-card .form-group,
      .compact-upload-card .shiny-input-container {
        margin-bottom: 0 !important;
      }
      .compact-upload-card .progress {
        margin-bottom: 0 !important;
      }
      .compact-upload-card .upload-input-separator {
        border-top: 1px solid var(--pdrb-border);
        margin: 14px 0 18px !important;
      }


      /* Satu tombol template di bawah input file */
      .compact-upload-card .single-template-button {
        display: flex !important;
        justify-content: center !important;
        margin: 10px 0 0 0 !important;
      }
      .compact-upload-card .single-template-button .btn {
        min-width: 230px;
        min-height: 38px;
        white-space: nowrap !important;
      }


      /* Ringkas tinggi card filter agar tidak terlalu panjang */
      .tabel-filter-panel {
        padding: 4px 2px 6px !important;
      }
      .tabel-filter-panel .row {
        margin-bottom: 8px !important;
      }
      .tabel-filter-panel [class*='col-sm-'] {
        margin-bottom: 6px !important;
      }
      .tabel-filter-panel .form-group {
        margin-bottom: 10px !important;
      }

      .distribution-period-filter,
      .structure-period-filter,
      .comparison-period-filter {
        margin-bottom: 8px !important;
      }
      .distribution-period-filter .form-group,
      .structure-period-filter .form-group,
      .comparison-period-filter .form-group,
      .analysis-filter-flex .form-group,
      .analysis-extra-flex .form-group {
        margin-bottom: 8px !important;
      }
      .analysis-filter-flex,
      .analysis-extra-flex {
        gap: 10px !important;
      }
      .analysis-extra-flex {
        margin-top: 2px !important;
      }
      .content .box.box-solid.box-info:has(.tabel-filter-panel) > .box-body,
      .content .box.box-solid.box-info:has(.distribution-period-filter) > .box-body,
      .content .box.box-solid.box-info:has(.structure-period-filter) > .box-body,
      .content .box.box-solid.box-info:has(.comparison-period-filter) > .box-body,
      .content .box.box-solid.box-info:has(.analysis-filter-flex) > .box-body {
        padding-bottom: 18px !important;
      }
      .content .box.box-solid.box-info:has(.analysis-extra-flex) > .box-body {
        padding-bottom: 18px !important;
      }

      /* Icon lokasi custom untuk map-marker */
      .custom-location-icon {
        display: block;
        width: 18px;
        height: 18px;
        background-color: currentColor;
        -webkit-mask: url('tanda_lokasi_sidebar_tebal.png') center / contain no-repeat;
        mask: url('tanda_lokasi_sidebar_tebal.png') center / contain no-repeat;
      }
      .page-intro-icon .custom-location-icon {
        width: 22px;
        height: 22px;
        color: var(--pdrb-navy);
      }
      .quick-card-icon .custom-location-icon {
        width: 18px;
        height: 18px;
        color: inherit;
      }

      /* Rapikan posisi, jarak, dan warna icon Potensi Wilayah di sidebar */
      .skin-blue .sidebar-menu > li > a > .sidebar-location-icon {
        width: 16px !important;
        min-width: 16px !important;
        height: 20px !important;
        margin-right: 2px !important;
        display: inline-flex !important;
        align-items: center !important;
        justify-content: flex-start !important;
        text-align: left !important;
        vertical-align: -3px !important;
        background: none !important;
        color: inherit !important;
        opacity: 1 !important;
      }
      .skin-blue .sidebar-menu > li > a > .sidebar-location-icon:before {
        content: '' !important;
        display: block !important;
        width: 16px !important;
        height: 16px !important;
        background-color: currentColor !important;
        -webkit-mask: url('tanda_lokasi_sidebar_tebal.png') center / contain no-repeat !important;
        mask: url('tanda_lokasi_sidebar_tebal.png') center / contain no-repeat !important;
      }
      .skin-blue .sidebar-menu > li.active > a > .sidebar-location-icon:before,
      .skin-blue .sidebar-menu > li:hover > a > .sidebar-location-icon:before {
        background-color: currentColor !important;
      }


      .structure-filter-grid {
        display: grid;
        gap: 16px;
        align-items: end;
        width: 100%;
      }
      .structure-filter-grid-row1 { grid-template-columns: repeat(5, minmax(0, 1fr)); }
      .structure-filter-grid-row2 { grid-template-columns: repeat(auto-fit, minmax(210px, 1fr)); margin-top: 12px; }
      .structure-filter-cell { min-width: 0; }
      .structure-filter-cell .form-group { margin-bottom: 0; }
      .structure-filter-grid > .shiny-panel-conditional { display: contents; }
      @media (max-width: 1200px) {
        .structure-filter-grid-row1 { grid-template-columns: repeat(3, minmax(0, 1fr)); }
        .structure-filter-grid-row2 { grid-template-columns: repeat(2, minmax(0, 1fr)); }
      }
      @media (max-width: 767px) {
        .structure-filter-grid-row1,
        .structure-filter-grid-row2 { grid-template-columns: 1fr; }
      }
    ")),
    tags$script(HTML("
      (function() {
        'use strict';

        var plotObservers = new WeakMap();
        var plotTimers = new WeakMap();
        var globalShowTimer = null;
        var globalFailsafeTimer = null;
        var uploadFailsafeTimer = null;
        var uploadLoaderLocked = false;
        var windowResizeTimer = null;

        function globalLoader() {
          return document.getElementById('pdrb_global_loader');
        }

        function normalizePercent(percent, detail) {
          var numeric = Number(percent);
          if (!Number.isFinite(numeric) && detail) {
            var matched = String(detail).match(/(^|\\s)(\\d{1,3})%/);
            if (matched) numeric = Number(matched[2]);
          }
          if (!Number.isFinite(numeric)) numeric = 0;
          return Math.max(0, Math.min(100, Math.round(numeric)));
        }

        function setGlobalLoaderText(message, detail, percent) {
          var messageNode = document.getElementById('pdrb_global_loader_message');
          var detailNode = document.getElementById('pdrb_global_loader_detail');
          var percentNode = document.getElementById('pdrb_global_loader_percent');
          var barNode = document.getElementById('pdrb_global_loader_bar');
          var normalizedPercent = normalizePercent(percent, detail);
          if (messageNode && message) messageNode.textContent = message;
          if (detailNode) detailNode.textContent = detail || 'Sedang memproses data.';
          if (percentNode) percentNode.textContent = normalizedPercent + '%';
          if (barNode) barNode.style.width = normalizedPercent + '%';
        }

        function showGlobalLoader(forceImmediate) {
          forceImmediate = forceImmediate === true;
          if (!forceImmediate && window.pdrbSuppressGlobalLoaderUntil && Date.now() < window.pdrbSuppressGlobalLoaderUntil) return;
          if (globalShowTimer) window.clearTimeout(globalShowTimer);

          var reveal = function() {
            var loader = globalLoader();
            if (loader) loader.classList.add('is-visible');
            if (document.body) document.body.classList.add('pdrb-global-loading');
          };

          if (forceImmediate) {
            reveal();
          } else {
            globalShowTimer = window.setTimeout(reveal, 110);
          }

          if (!uploadLoaderLocked) {
            if (globalFailsafeTimer) window.clearTimeout(globalFailsafeTimer);
            globalFailsafeTimer = window.setTimeout(function() { hideGlobalLoader(true); }, 15000);
          }
        }

        function hideGlobalLoader(force) {
          if (uploadLoaderLocked && !force) return;
          if (globalShowTimer) window.clearTimeout(globalShowTimer);
          if (globalFailsafeTimer) window.clearTimeout(globalFailsafeTimer);
          var loader = globalLoader();
          if (loader) loader.classList.remove('is-visible');
          if (document.body) document.body.classList.remove('pdrb-global-loading');
        }

        function lockUploadLoader(message, detail, percent) {
          uploadLoaderLocked = true;
          if (globalFailsafeTimer) window.clearTimeout(globalFailsafeTimer);
          if (uploadFailsafeTimer) window.clearTimeout(uploadFailsafeTimer);
          setGlobalLoaderText(
            message || 'Membaca file PDRB...',
            detail || '0% • Menyiapkan proses pembacaan data.',
            typeof percent === 'number' ? percent : 0
          );
          showGlobalLoader(true);
          uploadFailsafeTimer = window.setTimeout(function() {
            uploadLoaderLocked = false;
            setGlobalLoaderText(
              'Proses membaca data terlalu lama.',
              'Periksa file Excel atau pesan pada console R.',
              100
            );
            window.setTimeout(function() { hideGlobalLoader(true); }, 3000);
          }, 600000);
        }

        function updateUploadLoader(message, detail, percent) {
          if (!uploadLoaderLocked) uploadLoaderLocked = true;
          setGlobalLoaderText(message || 'Membaca file PDRB...', detail || '', percent);
          showGlobalLoader(true);
        }

        function releaseUploadLoader(success, message, detail, percent) {
          uploadLoaderLocked = false;
          if (uploadFailsafeTimer) window.clearTimeout(uploadFailsafeTimer);
          setGlobalLoaderText(
            message || (success === false ? 'Proses data belum berhasil.' : 'Data selesai diproses.'),
            detail || (success === false ? 'Periksa keterangan kesalahan yang ditampilkan.' : 'Seluruh file, sheet, validasi, dan indikator selesai diproses.'),
            typeof percent === 'number' ? percent : 100
          );
          window.setTimeout(function() {
            hideGlobalLoader(true);
          }, success === false ? 2200 : 650);
        }

        function shellIsVisible(shell) {
          if (!shell || !document.documentElement.contains(shell)) return false;
          var rect = shell.getBoundingClientRect();
          var style = window.getComputedStyle(shell);
          return shell.offsetParent !== null &&
            style.display !== 'none' &&
            style.visibility !== 'hidden' &&
            rect.width > 80 && rect.height > 80;
        }

        function holderFromShell(shell) {
          return shell ? shell.querySelector('.plotly.html-widget-output') : null;
        }

        function plotFromHolder(holder) {
          if (!holder) return null;
          if (holder.classList && holder.classList.contains('js-plotly-plot')) return holder;
          return holder.querySelector ? holder.querySelector('.js-plotly-plot') : null;
        }

        function shellFromNode(node) {
          if (!node || node.nodeType !== 1) return null;
          if (node.classList && node.classList.contains('pdrb-plot-shell')) return node;
          return node.closest ? node.closest('.pdrb-plot-shell') : null;
        }

        function resetShellText(shell) {
          if (!shell) return;
          var text = shell.querySelector('.pdrb-plot-loading-text');
          if (text) text.textContent = shell.getAttribute('data-loading-text') || 'Memuat grafik...';
        }

        function markShellLoading(shell) {
          if (!shell) return;
          resetShellText(shell);
          shell.classList.add('pdrb-plot-loading');
          shell.classList.remove('pdrb-plot-ready', 'pdrb-plot-unavailable');
        }

        function markShellReady(shell) {
          if (!shell) return;
          shell.classList.remove('pdrb-plot-loading', 'pdrb-plot-unavailable');
          shell.classList.add('pdrb-plot-ready');
        }

        function markShellUnavailable(shell) {
          if (!shell) return;
          var text = shell.querySelector('.pdrb-plot-loading-text');
          if (text) text.textContent = 'Grafik belum tersedia untuk filter yang dipilih.';
          shell.classList.remove('pdrb-plot-loading', 'pdrb-plot-ready');
          shell.classList.add('pdrb-plot-unavailable');
        }

        function plotHasStableSize(shell, plot) {
          if (!shell || !plot || !plot._fullLayout) return false;
          var shellRect = shell.getBoundingClientRect();
          var plotRect = plot.getBoundingClientRect();
          var fullWidth = Number(plot._fullLayout.width || 0);
          var fullHeight = Number(plot._fullLayout.height || 0);
          var minimumWidth = Math.max(80, shellRect.width * 0.72);
          return shellRect.width > 80 && shellRect.height > 80 &&
            plotRect.width >= minimumWidth && plotRect.height > 80 &&
            fullWidth >= minimumWidth && fullHeight > 80;
        }

        function stabilizeShell(shell, attempt) {
          if (!shell || !shellIsVisible(shell)) return;
          var holder = holderFromShell(shell);
          if (!holder) return;

          if (holder.querySelector('.shiny-output-error')) {
            markShellReady(shell);
            return;
          }

          var plot = plotFromHolder(holder);
          var plotReady = plot && plot._fullLayout && Array.isArray(plot.data);

          if (plotReady && window.Plotly) {
            try {
              Plotly.Plots.resize(plot);
            } catch (err) {
              console.warn('Plotly resize dilewati:', err);
            }

            window.requestAnimationFrame(function() {
              try { Plotly.Plots.resize(plot); } catch (err) {}
              window.requestAnimationFrame(function() {
                if (plotHasStableSize(shell, plot)) {
                  markShellReady(shell);
                } else if (attempt < 90) {
                  window.setTimeout(function() { stabilizeShell(shell, attempt + 1); }, 55);
                } else {
                  markShellUnavailable(shell);
                }
              });
            });
            return;
          }

          if (attempt < 90) {
            window.setTimeout(function() { stabilizeShell(shell, attempt + 1); }, 55);
          } else {
            markShellUnavailable(shell);
          }
        }

        function queueShell(shell, delay) {
          if (!shell) return;
          var previous = plotTimers.get(shell);
          if (previous) window.clearTimeout(previous);
          var timer = window.setTimeout(function() {
            stabilizeShell(shell, 0);
          }, typeof delay === 'number' ? delay : 20);
          plotTimers.set(shell, timer);
        }

        function visibleShells(root) {
          var scope = root || document;
          var shells = [];
          if (scope.classList && scope.classList.contains('pdrb-plot-shell')) shells.push(scope);
          if (scope.querySelectorAll) {
            scope.querySelectorAll('.pdrb-plot-shell').forEach(function(shell) { shells.push(shell); });
          }
          return shells.filter(function(shell, index, all) {
            return all.indexOf(shell) === index && shellIsVisible(shell);
          });
        }

        function prepareVisiblePlots(root, forceLoading) {
          visibleShells(root).forEach(function(shell) {
            if (forceLoading) markShellLoading(shell);
            queueShell(shell, 15);
          });
        }

        function observePlot(plot) {
          if (!plot || plotObservers.has(plot)) return;

          if (typeof plot.on === 'function') {
            plot.on('plotly_afterplot', function() {
              var shell = shellFromNode(plot);
              queueShell(shell, 0);
            });
          }

          if (window.ResizeObserver) {
            var observer = new ResizeObserver(function(entries) {
              entries.forEach(function(entry) {
                var shell = shellFromNode(plot);
                if (entry.contentRect.width > 80 && entry.contentRect.height > 80 && shellIsVisible(shell)) {
                  queueShell(shell, 25);
                }
              });
            });
            observer.observe(plot);
            plotObservers.set(plot, observer);
          } else {
            plotObservers.set(plot, true);
          }
        }

        function scanPlots(root) {
          var scope = root || document;
          if (scope.classList && scope.classList.contains('js-plotly-plot')) observePlot(scope);
          if (scope.querySelectorAll) scope.querySelectorAll('.js-plotly-plot').forEach(observePlot);
        }

        function activeContentRoot() {
          return document.querySelector('.tab-content > .tab-pane.active') ||
            document.querySelector('.tab-pane.active') ||
            document.querySelector('.content-wrapper') || document;
        }

        function waitUntilActivePlotsReady(attempt) {
          var root = activeContentRoot();
          var pending = visibleShells(root).some(function(shell) {
            return shell.classList.contains('pdrb-plot-loading');
          });
          if (!pending || attempt >= 110) {
            hideGlobalLoader();
            return;
          }
          window.setTimeout(function() { waitUntilActivePlotsReady(attempt + 1); }, 55);
        }

        function initializeManager() {
          var initialLoader = globalLoader();
          if (initialLoader && initialLoader.classList.contains('is-visible') && document.body) {
            document.body.classList.add('pdrb-global-loading');
          }

          $('.sidebar-menu li a').each(function() {
            var label = $(this).find('span').first().text().trim();
            if (label.length > 0) $(this).attr('title', label);
          });

          scanPlots(document);
          prepareVisiblePlots(activeContentRoot(), true);

          if (window.MutationObserver && document.body) {
            var mutationObserver = new MutationObserver(function(mutations) {
              mutations.forEach(function(mutation) {
                mutation.addedNodes.forEach(function(node) {
                  if (node && node.nodeType === 1) {
                    scanPlots(node);
                    var shell = shellFromNode(node);
                    if (shell && shellIsVisible(shell)) queueShell(shell, 15);
                  }
                });
              });
            });
            mutationObserver.observe(document.body, { childList: true, subtree: true });
          }
        }

        $(document).ready(initializeManager);


        /* V8.6: tampilkan loading lokal segera ketika filter Potensi Wilayah
           berubah. Sebelumnya loader baru muncul setelah Shiny mulai mengirim
           nilai output, sehingga grafik lama masih terlihat selama perhitungan. */
        var potentialPlotLoadingTimer = null;
        var potentialPlotFallbackTimer = null;

        function activePotentialPlotId() {
          var method = String($('#jenis_analisis_wilayah').val() || 'lq');
          if (method === 'dlq') return 'plot_dlq_v5';
          if (method === 'shift_share') return 'plot_shiftshare_v5';
          return 'plot_lq_v5';
        }

        function showPotentialPlotLoading(delay) {
          if (potentialPlotLoadingTimer) window.clearTimeout(potentialPlotLoadingTimer);
          if (potentialPlotFallbackTimer) window.clearTimeout(potentialPlotFallbackTimer);

          potentialPlotLoadingTimer = window.setTimeout(function() {
            var holder = document.getElementById(activePotentialPlotId());
            var shell = shellFromNode(holder);
            if (!shell || !shellIsVisible(shell)) return;

            markShellLoading(shell);

            /* Fallback hanya untuk kondisi output tidak mengirim event baru.
               Normalnya shiny:value/plotly_afterplot akan membuka grafik lebih cepat. */
            potentialPlotFallbackTimer = window.setTimeout(function() {
              if (shell.classList.contains('pdrb-plot-loading')) {
                queueShell(shell, 0);
              }
            }, 12000);
          }, typeof delay === 'number' ? delay : 40);
        }

        $(document).on(
          'change',
          '#jenis_analisis_wilayah, #dasar_harga_lq_tren, #potensi_level_kategori, #kategori_lq_tren, #tahun_lq, #periode_lq_peringkat, #tampilan_lq, #kategori_dlq, #tahun_awal_analisis, #tahun_akhir_analisis, #tahun_awal_shift_analisis, #tahun_akhir_shift_analisis, #komponen_shift_plot, #provinsi, #wilayah',
          function() {
            showPotentialPlotLoading(this && this.id === 'jenis_analisis_wilayah' ? 110 : 35);
          }
        );

        $(document).on('shiny:idle', function() {
          var root = activeContentRoot();
          prepareVisiblePlots(root, false);
        });

        $(document).on('shown.bs.tab', 'a[data-toggle=tab]', function() {
          var root = activeContentRoot();
          prepareVisiblePlots(root, true);
        });

        $(document).on('click', '.sidebar-toggle', function() {
          window.setTimeout(function() {
            prepareVisiblePlots(activeContentRoot(), true);
          }, 260);
        });

        $(document).on('shiny:recalculating', function(event) {
          var shell = shellFromNode(event && event.target);
          if (shell) markShellLoading(shell);
        });

        $(document).on('shiny:value', function(event) {
          if (!event || !event.name) return;
          var holder = document.getElementById(event.name);
          if (!holder) return;
          var shell = shellFromNode(holder);
          if (!shell) return;
          markShellLoading(shell);
          window.setTimeout(function() {
            scanPlots(holder);
            queueShell(shell, 10);
          }, 0);
        });

        $(document).on('shiny:error', function(event) {
          var shell = shellFromNode(event && event.target);
          if (shell) markShellReady(shell);
        });

        $(window).on('resize', function() {
          if (windowResizeTimer) window.clearTimeout(windowResizeTimer);
          windowResizeTimer = window.setTimeout(function() {
            prepareVisiblePlots(activeContentRoot(), true);
          }, 140);
        });

        if (window.Shiny) {
          /* V8.2: perbarui isi Selectize di tempat tanpa menghancurkan
             dan membuat ulang elemen. Ini mencegah filter Jenis Sektor
             Potensi Wilayah terlihat berkedip. */
          Shiny.addCustomMessageHandler('pdrbStableSelectizeUpdate', function(payload) {
            payload = payload || {};
            var id = payload.id;
            var rows = Array.isArray(payload.options) ? payload.options : [];
            var selected = payload.selected == null ? '' : String(payload.selected);

            function applyUpdate(attempt) {
              var node = id ? document.getElementById(id) : null;
              var control = node && node.selectize ? node.selectize : null;
              if (!control) {
                if (attempt < 40) {
                  window.setTimeout(function() { applyUpdate(attempt + 1); }, 50);
                }
                return;
              }

              var oldValue = String(control.getValue() == null ? '' : control.getValue());
              var wasOpen = !!control.isOpen;

              control.clearOptions();
              rows.forEach(function(row) {
                if (!row || row.value == null) return;
                control.addOption({
                  value: String(row.value),
                  text: row.text == null ? String(row.value) : String(row.text)
                });
              });
              control.refreshOptions(false);

              if (selected && Object.prototype.hasOwnProperty.call(control.options, selected)) {
                control.setValue(selected, oldValue === selected);
              } else {
                control.clear(oldValue === '');
              }

              if (wasOpen) control.open();
            }

            applyUpdate(0);
          });

          Shiny.addCustomMessageHandler('resizePlotly', function(id) {
            var holder = document.getElementById(id);
            var shell = shellFromNode(holder);
            if (shell) {
              markShellLoading(shell);
              queueShell(shell, 30);
            }
          });

          Shiny.addCustomMessageHandler('pdrbUploadLoading', function(payload) {
            payload = payload || {};
            var action = payload.action || 'progress';
            if (action === 'start') {
              lockUploadLoader(payload.message, payload.detail, payload.percent);
            } else if (action === 'progress') {
              updateUploadLoader(payload.message, payload.detail, payload.percent);
            } else if (action === 'finish') {
              releaseUploadLoader(payload.success !== false, payload.message, payload.detail, payload.percent);
            } else if (action === 'hide') {
              uploadLoaderLocked = false;
              hideGlobalLoader(true);
            }
          });
        }

        window.PDRBGlobalLoader = {
          lockUpload: lockUploadLoader,
          updateUpload: updateUploadLoader,
          releaseUpload: releaseUploadLoader,
          show: function(message, detail, percent) {
            setGlobalLoaderText(message || 'Membaca file PDRB...', detail || '', percent);
            showGlobalLoader(true);
          },
          hide: function() { hideGlobalLoader(true); }
        };

        window.PDRBPlotlyManager = {
          prepareVisible: prepareVisiblePlots,
          setLoading: markShellLoading,
          setReady: markShellReady,
          scan: scanPlots
        };
      })();
    "))
  ),
  
  tags$style(HTML("
    .analytics-filter-stack { width: 100%; }
    .analytics-filter-row {
      display: grid;
      gap: 14px 24px;
      align-items: end;
      width: 100%;
    }
    .analytics-filter-row-4 { grid-template-columns: repeat(4, minmax(0, 1fr)); }
    .analytics-filter-row-5 { grid-template-columns: repeat(5, minmax(0, 1fr)); }
    .analytics-filter-row-auto { grid-template-columns: repeat(auto-fit, minmax(210px, 1fr)); }
    .analytics-filter-row-second { margin-top: 14px; }
    .analytics-filter-cell, .analytics-filter-swap { min-width: 0; }
    .analytics-filter-row .form-group,
    .analytics-filter-swap .form-group { margin-bottom: 0; }
    .analytics-filter-row > .shiny-panel-conditional { display: contents; }
    .analytics-filter-swap > .shiny-panel-conditional { display: block; }
    /* Filter Analytics: pola adaptif seperti card Metode Analisis */
    .analytics-analysis-flex {
      display: flex;
      flex-wrap: wrap;
      gap: 12px;
      align-items: flex-end;
      width: 100%;
    }
    .analytics-analysis-flex .analytics-filter-cell,
    .analytics-analysis-flex .analytics-filter-swap {
      flex: 1 1 calc(25% - 12px);
      min-width: 210px;
    }
    .analytics-analysis-flex .form-group { margin-bottom: 0; }
    .analytics-analysis-flex > .shiny-panel-conditional { display: contents; }
    .analytics-analysis-flex .analytics-filter-swap > .shiny-panel-conditional { display: block; }
    @media (max-width: 1200px) {
      .analytics-analysis-flex .analytics-filter-cell,
      .analytics-analysis-flex .analytics-filter-swap {
        flex-basis: calc(50% - 12px);
      }
    }
    @media (max-width: 767px) {
      .analytics-analysis-flex .analytics-filter-cell,
      .analytics-analysis-flex .analytics-filter-swap {
        flex-basis: 100%;
        min-width: 100%;
      }
    }

    /* Ringkasan Status Data */
    .status-data-summary-grid {
      display: grid;
      grid-template-columns: repeat(4, minmax(0, 1fr));
      gap: 12px;
      margin: 4px 0 14px;
    }
    .status-data-metric {
      padding: 13px 14px;
      border: 1px solid var(--pdrb-border);
      border-radius: 11px;
      background: #FBFCFC;
      min-height: 78px;
    }
    .status-data-metric span {
      display: block;
      color: var(--pdrb-muted);
      font-size: 11.5px;
      line-height: 1.35;
      margin-bottom: 4px;
    }
    .status-data-metric strong {
      display: block;
      color: var(--pdrb-navy);
      font-size: 15px;
      line-height: 1.35;
      word-break: break-word;
    }
    .status-data-note {
      display: flex;
      gap: 8px;
      align-items: flex-start;
      margin: 10px 0 4px;
      padding: 11px 12px;
      border-radius: 9px;
      background: var(--pdrb-soft-blue);
      color: var(--pdrb-navy);
      font-size: 12px;
      line-height: 1.45;
    }
    .status-data-note.warning {
      background: var(--pdrb-soft-gold);
      color: #75581D;
    }
    .validation-unread-guide {
      display: flex;
      align-items: flex-start;
      gap: 11px;
      margin: 2px 0 14px;
      padding: 13px 14px;
      border: 1px solid #D8E6E3;
      border-radius: 10px;
      background: #F4F9F8;
      color: var(--pdrb-navy);
      font-size: 12px;
      line-height: 1.5;
    }
    .validation-unread-guide > i {
      margin-top: 2px;
      color: var(--pdrb-teal);
      flex: 0 0 auto;
    }
    .validation-unread-guide strong {
      display: block;
      margin-bottom: 4px;
      font-size: 12.5px;
    }
    .validation-unread-guide p {
      margin: 0;
      color: #49636C;
    }
    @media (max-width: 1200px) {
      .status-data-summary-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
    }
    @media (max-width: 767px) {
      .status-data-summary-grid { grid-template-columns: 1fr; }
    }

    #shared-analytics-filter-pool { display: none !important; }
    @media (max-width: 1200px) {
      .analytics-filter-row-4, .analytics-filter-row-5 { grid-template-columns: repeat(2, minmax(0, 1fr)); }
    }
    @media (max-width: 767px) {
      .analytics-filter-row, .analytics-filter-row-4, .analytics-filter-row-5, .analytics-filter-row-auto {
        grid-template-columns: 1fr;
      }
    }
  ")),
  div(
    id = "pdrb_global_loader",
    class = "pdrb-global-loader",
    div(
      class = "pdrb-global-loader-content",
      div(class = "pdrb-global-spinner", `aria-hidden` = "true"),
      div(
        class = "pdrb-global-loader-copy",
        span(id = "pdrb_global_loader_message", "Membaca file PDRB..."),
        div(
          class = "pdrb-global-progress-row",
          strong(id = "pdrb_global_loader_percent", "0%"),
          div(
            class = "pdrb-global-progress-track",
            span(id = "pdrb_global_loader_bar")
          )
        ),
        shiny::tags$small(
          id = "pdrb_global_loader_detail",
          "Menunggu proses dimulai."
        )
      )
    )
  ),
  div(
    id = "shared-analytics-filter-pool",
    pdrb_selectize(
      "kelompok_indikator", "Jenis Nilai",
      choices = c(
        "PDRB" = "PDRB",
        "Distribusi" = "Distribusi",
        "Pertumbuhan" = "Pertumbuhan",
        "Indeks Implisit" = "Indeks Implisit",
        "Sumber Pertumbuhan" = "Sumber Pertumbuhan"
      ),
      selected = "PDRB"
    ),
    uiOutput("indikator_filter_ui")
  ),

  tabItems(
    tabItem(
      tabName = "input_data",
      page_intro(
        "Workspace",
        "Upload Data",
        NULL,
        "cloud-upload"
      ),
      fluidRow(
        box(
          title = tagList(icon("upload"), " Unggah Data PDRB"),
          width = 12, status = "primary", solidHeader = TRUE,
          class = "upload-main-box upload-process-box",
          div(
            class = "upload-card compact-upload-card upload-process-card",
            div(
              class = "upload-dual-grid",
              div(
                class = "upload-file-block",
                div(
                  class = "upload-file-heading",
                  icon("building"),
                  div(
                    tags$strong("Data Provinsi/Agregat")
                  )
                ),
                fileInput(
                  "file_pdrb_provinsi", NULL,
                  accept = c(".xlsx", ".xls"), multiple = TRUE,
                  buttonLabel = "Pilih File",
                  placeholder = "Belum ada file"
                )
              ),
              div(
                class = "upload-file-block",
                div(
                  class = "upload-file-heading",
                  icon("map-marker", class = "upload-location-icon"),
                  div(
                    tags$strong("Data Kabupaten/Kota")
                  )
                ),
                fileInput(
                  "file_pdrb_kabkota", NULL,
                  accept = c(".xlsx", ".xls"), multiple = TRUE,
                  buttonLabel = "Pilih File",
                  placeholder = "Belum ada file"
                )
              )
            ),
            div(
              class = "upload-process-footer upload-action-footer",
              div(
                class = "upload-template-button-group",
                downloadButton(
                  "download_example_template_xlsx",
                  "Unduh Template 17 Lapangan Usaha (.xlsx)",
                  class = "btn-success"
                ),
                downloadButton(
                  "download_example_template_xlsx_alt",
                  "Unduh Template Lengkap (.xlsx)",
                  class = "btn-success"
                )
              ),
              div(
                class = "upload-process-button-wrap",
                uiOutput("process_upload_button_ui")
              )
            )
          ),
          tags$script(HTML("
            (function() {
              $(document).on('click', '#process_pdrb:not([disabled])', function() {
                if (window.PDRBGlobalLoader && window.PDRBGlobalLoader.lockUpload) {
                  window.PDRBGlobalLoader.lockUpload(
                    'Memproses data PDRB...',
                    '0% • Menyiapkan file yang dipilih untuk diproses.',
                    0
                  );
                }
              });
            })();
          "))
        )
      ),
      fluidRow(
        box(
          title = tagList(icon("database"), " Status Data"),
          width = 12, status = "info", solidHeader = TRUE,
          uiOutput("validation_status"),
          conditionalPanel(
            condition = "output.upload_attempted === true",
            tabsetPanel(
              id = "status_data_tabs",
              tabPanel(
                title = "Tabel Dibaca",
                br(),
                DT::DTOutput("validation_read_table")
              ),
              tabPanel(
                title = "Tabel Tidak Dibaca",
                br(),
                div(
                  class = "validation-unread-guide",
                  icon("info-circle"),
                  div(
                    strong("Tabel yang tidak dijadikan input dashboard"),
                    p(
                      "Dashboard hanya membaca data mentah PDRB ADHB dan ADHK. Tabel turunan seperti Distribusi, Pertumbuhan, Indeks Implisit, Sumber Pertumbuhan, LQ, DLQ, dan Extended Shift Share sengaja diabaikan karena dihitung ulang oleh dashboard. Sheet petunjuk, metadata, dan sheet kosong juga tidak dijadikan data analisis."
                    )
                  )
                ),
                DT::DTOutput("validation_unread_table")
              )
            )
          )
        )
      ),
      fluidRow(
        box(
          title = tagList(icon("table"), " Pratinjau Data"),
          width = 12, status = "primary", solidHeader = TRUE,
          br(),
          conditionalPanel(
            condition = "output.data_ready === true",
            div(
              class = "preview-control-panel",
              selectizeInput(
                "preview_indikator", "Jenis Data",
                choices = NULL,
                options = list(
                  dropdownParent = "body",
                  placeholder = "Pilih data",
                  allowEmptyOption = FALSE
                )
              )
            ),
            DT::DTOutput("input_preview")
          ),
          conditionalPanel(
            condition = "output.data_ready !== true",
            div(
              class = "preview-empty-state",
              icon("cloud-upload"),
              strong("Preview kosong"),
              p("Pilih minimal satu file, lalu klik Proses Data.")
            )
          )
        )
      )
    ),
    tabItem(
      tabName = "ringkasan",
      uiOutput("hero_panel"),
      div(
        class = "summary-valuebox-row",
        fluidRow(
          valueBoxOutput("vb_adhb", width = 3),
          valueBoxOutput("vb_adhk", width = 3),
          valueBoxOutput("vb_yoy_adhk", width = 3),
          valueBoxOutput("vb_top_sector", width = 3)
        )
      ),
      fluidRow(
        box(
          title = tagList(icon("line-chart"), " Tren PDRB ADHB dan ADHK"),
          width = 12, status = "primary", solidHeader = TRUE,
          pdrb_plotly_output("plot_trend_pdrb_overview", height = 390, loading_text = "Memuat tren PDRB...")
        )
      ),
      fluidRow(
        box(
          title = tagList(icon("trophy"), " Top Lapangan Usaha ADHB Terbesar"),
          width = 12, status = "primary", solidHeader = TRUE,
          pdrb_plotly_output("plot_top_adhb", height = 430, loading_text = "Memuat sektor utama...")
        )
      )
    ),
    
    tabItem(
      tabName = "tabel",
      page_intro(
        "Data",
        "Tabel Data",
        NULL,
        "table"
      ),
      fluidRow(
        box(
          title = tagList(icon("filter"), " Filter Tabel"),
          width = 12, status = "info", solidHeader = TRUE,
          div(
            class = "tabel-filter-panel",
            uiOutput("tabel_pilihan_utama_ui"),
            uiOutput("tabel_filter_dinamis_ui")
          )
        )
      ),
      fluidRow(
        box(
          title = tagList(icon("table"), " Hasil"),
          width = 12, status = "primary", solidHeader = TRUE,
          div(
            class = "table-download-actions",
            style = "display:flex; gap:10px; flex-wrap:wrap; align-items:center;",
            downloadButton("download_data_excel", "Unduh Excel (.xlsx)", class = "btn-primary"),
            downloadButton("download_data_csv", "Unduh CSV", class = "btn-success")
          ),
          tags$div(style = "height: 10px;"),
          DT::DTOutput("data_table")
        )
      )
    ),
    
    tabItem(
      tabName = "tren",
      page_intro(
        "Time series",
        "Tren PDRB",
        NULL,
        "line-chart"
      ),
      fluidRow(
        box(
          title = tagList(icon("filter"), " Filter"),
          width = 12, status = "info", solidHeader = TRUE,
          div(
            class = "analytics-analysis-flex",
            div(class = "analytics-filter-cell", pdrb_selectize(
              "tren_jenis_nilai", "Jenis Nilai",
              choices = c(
                "PDRB" = "PDRB",
                "Distribusi" = "Distribusi",
                "Pertumbuhan" = "Pertumbuhan",
                "Indeks Implisit" = "Indeks Implisit",
                "Sumber Pertumbuhan" = "Sumber Pertumbuhan"
              ),
              selected = "PDRB"
            )),
            div(
              class = "analytics-filter-cell analytics-filter-swap",
              conditionalPanel(
                condition = "input.tren_jenis_nilai != 'Indeks Implisit'",
                pdrb_selectize(
                  "tren_dasar_harga", "Dasar Harga",
                  choices = c("ADHB" = "ADHB", "ADHK" = "ADHK"), selected = "ADHB"
                )
              ),
              conditionalPanel(
                condition = "input.tren_jenis_nilai == 'Indeks Implisit'",
                pdrb_selectize(
                  "tren_jenis_indeks", "Jenis Indeks",
                  choices = c(
                    "Indeks Implisit" = "Indeks Implisit",
                    "Laju Indeks Implisit Q-to-Q" = "Laju Indeks Implisit Q-to-Q",
                    "Laju Indeks Implisit Y-on-Y" = "Laju Indeks Implisit Y-on-Y",
                    "Laju Indeks Implisit C-to-C" = "Laju Indeks Implisit C-to-C"
                  ),
                  selected = "Indeks Implisit"
                )
              )
            ),
            conditionalPanel(
              condition = "['Pertumbuhan','Sumber Pertumbuhan'].indexOf(input.tren_jenis_nilai) >= 0",
              div(class = "analytics-filter-cell", pdrb_selectize(
                "tren_jenis_pertumbuhan", "Jenis Pertumbuhan",
                choices = c("Q-to-Q" = "Q-to-Q", "Y-on-Y" = "Y-on-Y", "C-to-C" = "C-to-C"),
                selected = "Q-to-Q"
              ))
            ),
            div(class = "analytics-filter-cell", pdrb_selectize(
              "tren_tingkat_analisis", "Tingkat Analisis",
              choices = c(
                "Semua" = "Semua",
                "Kategori Utama" = "Kategori Utama",
                "Subkategori" = "Subkategori",
                "Rincian Subkategori" = "Rincian"
              ),
              selected = "Semua"
            )),
            div(class = "analytics-filter-cell", pdrb_selectize(
              "tren_jenis_sektor", "Jenis Sektor", choices = NULL,
              placeholder = "Pilih jenis sektor", max_options = 500
            )),
            div(class = "analytics-filter-cell", selectizeInput(
              "tahun_awal_tren", "Tahun Awal", choices = NULL,
              options = list(dropdownParent = "body")
            )),
            div(class = "analytics-filter-cell", selectizeInput(
              "tahun_akhir_tren", "Tahun Akhir", choices = NULL,
              options = list(dropdownParent = "body")
            )),
            div(class = "analytics-filter-cell", selectInput(
              "mode_periode_tren", "Periode",
              choices = c(
                "Semua Triwulan" = "quarterly",
                "Triwulan I" = "I", "Triwulan II" = "II",
                "Triwulan III" = "III", "Triwulan IV" = "IV",
                "Tahun" = "Total"
              ),
              selected = "quarterly"
            ))
          ),
          uiOutput("tren_filter_notice")
        )
      ),
      div(
        class = "summary-valuebox-row trend-valuebox-row",
        fluidRow(
          valueBoxOutput("vb_tren_awal", width = 4),
          valueBoxOutput("vb_tren_akhir", width = 4),
          valueBoxOutput("vb_tren_perubahan", width = 4)
        )
      ),
      fluidRow(
        box(
          title = tagList(icon("line-chart"), " Perkembangan Indikator"),
          width = 12, status = "primary", solidHeader = TRUE,
          pdrb_plotly_output("plot_trend", height = 520, loading_text = "Memuat grafik tren...")
        )
      )
    ),
    
    tabItem(
      tabName = "kernel",
      page_intro(
        "Analisis sebaran",
        "Distribusi Data",
        NULL,
        "bar-chart"
      ),
      fluidRow(
        box(
          title = tagList(icon("sliders"), " Pengaturan Distribusi Data"),
          width = 12, status = "info", solidHeader = TRUE,
          div(
            class = "distribution-period-filter",
            div(
              class = "analytics-analysis-flex",
              div(class = "analytics-filter-cell", pdrb_selectize(
                "distribusi_jenis_nilai", "Jenis Nilai",
                choices = c(
                  "PDRB" = "PDRB",
                  "Distribusi" = "Distribusi",
                  "Pertumbuhan" = "Pertumbuhan",
                  "Indeks Implisit" = "Indeks Implisit",
                  "Sumber Pertumbuhan" = "Sumber Pertumbuhan"
                ),
                selected = "PDRB"
              )),
              div(
                class = "analytics-filter-cell analytics-filter-swap",
                conditionalPanel(
                  condition = "input.distribusi_jenis_nilai != 'Indeks Implisit'",
                  pdrb_selectize(
                    "distribusi_dasar_harga", "Dasar Harga",
                    choices = c("ADHB" = "ADHB", "ADHK" = "ADHK"), selected = "ADHK"
                  )
                ),
                conditionalPanel(
                  condition = "input.distribusi_jenis_nilai == 'Indeks Implisit'",
                  pdrb_selectize(
                    "distribusi_jenis_indeks", "Jenis Indeks",
                    choices = c(
                      "Indeks Implisit" = "Indeks Implisit",
                      "Laju Indeks Implisit Q-to-Q" = "Laju Indeks Implisit Q-to-Q",
                      "Laju Indeks Implisit Y-on-Y" = "Laju Indeks Implisit Y-on-Y",
                      "Laju Indeks Implisit C-to-C" = "Laju Indeks Implisit C-to-C"
                    ),
                    selected = "Indeks Implisit"
                  )
                )
              ),
              conditionalPanel(
                condition = "['Pertumbuhan','Sumber Pertumbuhan'].indexOf(input.distribusi_jenis_nilai) >= 0",
                div(class = "analytics-filter-cell", pdrb_selectize(
                  "distribusi_jenis_pertumbuhan", "Jenis Pertumbuhan",
                  choices = c("Q-to-Q" = "Q-to-Q", "Y-on-Y" = "Y-on-Y", "C-to-C" = "C-to-C"),
                  selected = "Q-to-Q"
                ))
              ),
              div(class = "analytics-filter-cell", pdrb_selectize(
                "distribusi_tingkat_analisis", "Tingkat Analisis",
                choices = c(
                  "Semua" = "Semua",
                  "Kategori Utama" = "Kategori Utama",
                  "Subkategori" = "Subkategori",
                  "Rincian Subkategori" = "Rincian"
                ),
                selected = "Semua"
              )),
              div(class = "analytics-filter-cell", pdrb_selectize(
                "distribusi_jenis_sektor", "Jenis Sektor", choices = NULL,
                placeholder = "Pilih jenis sektor", max_options = 500
              )),
              div(class = "analytics-filter-cell", pdrb_selectize(
                "tahun_distribusi", "Tahun",
                choices = c("Semua Tahun" = "__ALL__"),
                selected = "__ALL__", placeholder = "Pilih tahun"
              )),
              div(class = "analytics-filter-cell", pdrb_selectize(
                "periode_distribusi", "Periode",
                choices = c(
                  "Semua Triwulan" = "__QUARTERS__",
                  "Triwulan I" = "I", "Triwulan II" = "II",
                  "Triwulan III" = "III", "Triwulan IV" = "IV",
                  "Tahun" = "Total"
                ),
                selected = "__QUARTERS__", placeholder = "Pilih periode"
              )),
              div(class = "analytics-filter-cell", pdrb_selectize(
                "tampilkan_kurva_kernel", "Kurva Kernel",
                choices = c("Tampilkan" = "show", "Sembunyikan" = "hide"),
                selected = "show", placeholder = "Pilih tampilan kurva"
              ))
            ),
            uiOutput("distribusi_filter_notice")
          )
        )
      ),
      div(
        class = "summary-valuebox-row distribution-valuebox-row",
        fluidRow(
          valueBoxOutput("vb_distribusi_median", width = 4),
          valueBoxOutput("vb_distribusi_cv", width = 4),
          valueBoxOutput("vb_distribusi_rentang", width = 4)
        )
      ),
      fluidRow(
        box(
          title = tagList(icon("bar-chart"), " Histogram Distribusi"),
          width = 8, status = "primary", solidHeader = TRUE,
          pdrb_plotly_output(
            "plot_kernel_histogram",
            height = 455,
            loading_text = "Memuat histogram dan kurva kernel..."
          )
        ),
        box(
          title = tagList(icon("calculator"), " Statistika Deskriptif"),
          width = 4, status = "info", solidHeader = TRUE,
          uiOutput("descriptive_context"),
          DT::DTOutput("descriptive_stats")
        )
      )
    ),
    
    tabItem(
      tabName = "struktur",
      page_intro(
        "Komposisi ekonomi",
        "Struktur Ekonomi",
        NULL,
        "pie-chart"
      ),
      fluidRow(
        box(
          title = tagList(icon("sliders"), " Pengaturan Struktur Ekonomi"),
          width = 12, status = "info", solidHeader = TRUE,
          div(
            class = "structure-period-filter",
            div(
              class = "analytics-analysis-flex",
              div(class = "analytics-filter-cell", pdrb_selectize(
                "struktur_jenis_nilai", "Jenis Nilai",
                choices = c(
                  "PDRB" = "PDRB",
                  "Distribusi" = "Distribusi",
                  "Pertumbuhan" = "Pertumbuhan",
                  "Indeks Implisit" = "Indeks Implisit",
                  "Sumber Pertumbuhan" = "Sumber Pertumbuhan"
                ),
                selected = "PDRB"
              )),
              div(
                class = "analytics-filter-cell analytics-filter-swap",
                conditionalPanel(
                  condition = "input.struktur_jenis_nilai != 'Indeks Implisit'",
                  pdrb_selectize(
                    "struktur_dasar_harga", "Dasar Harga",
                    choices = c("ADHB" = "ADHB", "ADHK" = "ADHK"), selected = "ADHK"
                  )
                ),
                conditionalPanel(
                  condition = "input.struktur_jenis_nilai == 'Indeks Implisit'",
                  pdrb_selectize(
                    "struktur_jenis_indeks", "Jenis Indeks",
                    choices = c(
                      "Indeks Implisit" = "Indeks Implisit",
                      "Laju Indeks Implisit Q-to-Q" = "Laju Indeks Implisit Q-to-Q",
                      "Laju Indeks Implisit Y-on-Y" = "Laju Indeks Implisit Y-on-Y",
                      "Laju Indeks Implisit C-to-C" = "Laju Indeks Implisit C-to-C"
                    ),
                    selected = "Indeks Implisit"
                  )
                )
              ),
              conditionalPanel(
                condition = "['Pertumbuhan','Sumber Pertumbuhan'].indexOf(input.struktur_jenis_nilai) >= 0",
                div(class = "analytics-filter-cell", pdrb_selectize(
                  "struktur_jenis_pertumbuhan", "Jenis Pertumbuhan",
                  choices = c("Q-to-Q" = "Q-to-Q", "Y-on-Y" = "Y-on-Y", "C-to-C" = "C-to-C"),
                  selected = "Q-to-Q"
                ))
              ),
              div(class = "analytics-filter-cell", pdrb_selectize(
                "struktur_tingkat_analisis", "Tingkat Analisis",
                choices = c(
                  "Semua" = "Semua",
                  "Kategori Utama" = "Kategori Utama",
                  "Subkategori" = "Subkategori",
                  "Rincian Subkategori" = "Rincian"
                ),
                selected = "Semua"
              )),
              div(class = "analytics-filter-cell", pdrb_selectize(
                "tahun_struktur", "Tahun", choices = NULL, placeholder = "Pilih tahun"
              )),
              div(class = "analytics-filter-cell", pdrb_selectize(
                "triwulan_struktur", "Periode", choices = NULL, placeholder = "Pilih periode"
              )),
              div(class = "analytics-filter-cell", pdrb_selectize(
                "tampilan_komposisi_struktur", "Tampilan Komposisi",
                choices = c(
                  "Kategori Utama" = "kategori_utama",
                  "Subkategori" = "subkategori",
                  "Rincian Subkategori" = "rincian"
                ),
                selected = "kategori_utama", placeholder = "Pilih level komposisi"
              )),
              conditionalPanel(
                condition = "['subkategori','rincian'].indexOf(input.tampilan_komposisi_struktur) >= 0",
                div(class = "analytics-filter-cell", selectizeInput(
                  "kategori_utama_struktur", "Kategori", choices = NULL,
                  options = list(placeholder = "Pilih kategori", maxOptions = 100, dropdownParent = "body")
                ))
              ),
              conditionalPanel(
                condition = "input.tampilan_komposisi_struktur == 'rincian'",
                div(class = "analytics-filter-cell", selectizeInput(
                  "subkategori_struktur", "Subkategori", choices = NULL,
                  options = list(placeholder = "Pilih subkategori", maxOptions = 200, dropdownParent = "body")
                ))
              )
            )
          )
        )
      ),
      fluidRow(
        valueBoxOutput("vb_structure_total", width = 4),
        valueBoxOutput("vb_structure_dominant", width = 4),
        valueBoxOutput("vb_structure_top3", width = 4)
      ),
      fluidRow(
        box(
          title = tagList(icon("pie-chart"), " Komposisi Struktur Ekonomi"),
          width = 12, status = "primary", solidHeader = TRUE,
          pdrb_plotly_output("plot_structure", height = 560, loading_text = "Memuat komposisi ekonomi...")
        )
      ),
      fluidRow(
        box(
          title = tagList(icon("list-ol"), " Tabel Ringkas Struktur Ekonomi"),
          width = 12, status = "info", solidHeader = TRUE,
          div(
            class = "table-download-actions",
            style = "display:flex; gap:10px; flex-wrap:wrap; align-items:center;",
            downloadButton("download_structure_excel", "Unduh Excel (.xlsx)", class = "btn-primary"),
            downloadButton("download_structure_csv", "Unduh CSV", class = "btn-success")
          ),
          tags$div(style = "height: 10px;"),
          DT::DTOutput("structure_table")
        )
      )
    ),
    
    tabItem(
      tabName = "perbandingan",
      page_intro(
        "Antarwilayah",
        "Komparasi Wilayah",
        NULL,
        "balance-scale"
      ),
      fluidRow(
        box(
          title = tagList(icon("sliders"), " Pengaturan Komparasi Wilayah"),
          width = 12, status = "info", solidHeader = TRUE,
          div(
            class = "analytics-analysis-flex",
            div(class = "analytics-filter-cell", pdrb_selectize(
              "komparasi_jenis_nilai", "Jenis Nilai",
              choices = c(
                "PDRB" = "PDRB",
                "Distribusi" = "Distribusi",
                "Pertumbuhan" = "Pertumbuhan",
                "Indeks Implisit" = "Indeks Implisit",
                "Sumber Pertumbuhan" = "Sumber Pertumbuhan"
              ),
              selected = "PDRB"
            )),
            div(
              class = "analytics-filter-cell analytics-filter-swap",
              conditionalPanel(
                condition = "input.komparasi_jenis_nilai != 'Indeks Implisit'",
                pdrb_selectize(
                  "komparasi_dasar_harga", "Dasar Harga",
                  choices = c("ADHB" = "ADHB", "ADHK" = "ADHK"), selected = "ADHK"
                )
              ),
              conditionalPanel(
                condition = "input.komparasi_jenis_nilai == 'Indeks Implisit'",
                pdrb_selectize(
                  "komparasi_jenis_indeks", "Jenis Indeks",
                  choices = c(
                    "Indeks Implisit" = "Indeks Implisit",
                    "Laju Indeks Implisit Q-to-Q" = "Laju Indeks Implisit Q-to-Q",
                    "Laju Indeks Implisit Y-on-Y" = "Laju Indeks Implisit Y-on-Y",
                    "Laju Indeks Implisit C-to-C" = "Laju Indeks Implisit C-to-C"
                  ),
                  selected = "Indeks Implisit"
                )
              )
            ),
            conditionalPanel(
              condition = "['Pertumbuhan','Sumber Pertumbuhan'].indexOf(input.komparasi_jenis_nilai) >= 0",
              div(class = "analytics-filter-cell", pdrb_selectize(
                "komparasi_jenis_pertumbuhan", "Jenis Pertumbuhan",
                choices = c("Q-to-Q" = "Q-to-Q", "Y-on-Y" = "Y-on-Y", "C-to-C" = "C-to-C"),
                selected = "Q-to-Q"
              ))
            ),
            div(class = "analytics-filter-cell", pdrb_selectize(
              "komparasi_tingkat_analisis", "Tingkat Analisis",
              choices = c(
                "Semua" = "Semua",
                "Kategori Utama" = "Kategori Utama",
                "Subkategori" = "Subkategori",
                "Rincian Subkategori" = "Rincian"
              ),
              selected = "Semua"
            )),
            div(class = "analytics-filter-cell", pdrb_selectize(
              "komparasi_jenis_sektor", "Jenis Sektor", choices = NULL,
              placeholder = "Pilih jenis sektor", max_options = 500
            )),
            div(class = "analytics-filter-cell", selectizeInput(
              "wilayah_banding", "Wilayah Dibandingkan", choices = NULL, multiple = TRUE,
              options = list(
                dropdownParent = "body",
                placeholder = "Kosongkan untuk semua kabupaten/kota", maxOptions = 500
              )
            )),
            div(class = "analytics-filter-cell", selectizeInput(
              "tahun_perbandingan", "Tahun", choices = NULL,
              options = list(dropdownParent = "body", placeholder = "Pilih tahun")
            )),
            div(class = "analytics-filter-cell", selectizeInput(
              "periode_perbandingan", "Periode", choices = NULL,
              options = list(dropdownParent = "body", placeholder = "Pilih periode")
            ))
          ),
            fluidRow(
              column(
                4,
                checkboxInput(
                  "include_agregat_perbandingan",
                  "Tampilkan provinsi/agregat sebagai pembanding",
                  value = TRUE
                )
              )
            ),
          div(
            class = "distribution-note",
            icon("info-circle"),
            span("Jika wilayah pembanding dikosongkan, grafik otomatis menampilkan semua kabupaten/kota. Centang opsi agregat jika provinsi ingin ikut dibandingkan.")
          ),
          conditionalPanel(
            condition = paste0(
              "input.komparasi_jenis_nilai == 'Distribusi' && ",
              "input.komparasi_jenis_sektor == 'Total PDRB__PDRB__PRODUK DOMESTIK REGIONAL BRUTO'"
            ),
            div(
              class = "distribution-note",
              icon("info-circle"),
              span("Distribusi Produk Domestik Regional Bruto selalu bernilai 100% karena total dibandingkan dengan dirinya sendiri. Pilih kategori atau sektor untuk melihat perbedaan kontribusi antarwilayah.")
            )
          )
        )
      ),
      fluidRow(
        valueBoxOutput("vb_compare_high", width = 4),
        valueBoxOutput("vb_compare_low", width = 4),
        valueBoxOutput("vb_compare_ratio", width = 4)
      ),
      fluidRow(
        box(
          title = tagList(icon("balance-scale"), " Komparasi Wilayah"),
          width = 12, status = "primary", solidHeader = TRUE,
          pdrb_plotly_output("plot_compare", height = 520, loading_text = "Memuat komparasi wilayah...")
        )
      ),
      fluidRow(
        box(
          title = tagList(icon("list-ol"), " Peringkat Wilayah"),
          width = 12, status = "info", solidHeader = TRUE,
          div(
            class = "table-download-actions",
            style = "display:flex; gap:10px; flex-wrap:wrap; align-items:center;",
            downloadButton("download_compare_excel", "Unduh Excel (.xlsx)", class = "btn-primary"),
            downloadButton("download_compare_csv", "Unduh CSV", class = "btn-success")
          ),
          tags$div(style = "height: 10px;"),
          DT::DTOutput("table_compare_rank")
        )
      )
    ),
    
    
    tabItem(
      tabName = "analisis_wilayah",
      page_intro(
        "Wilayah",
        "Potensi Wilayah",
        NULL,
        "custom-location"
      ),
      fluidRow(
        box(
          title = tagList(icon("sliders"), " Metode Analisis"),
          width = 12, status = "info", solidHeader = TRUE,
          div(
            class = "potential-method-stack",

            # Baris utama: susunan berubah mengikuti metode analisis.
            div(
              class = "potential-method-primary",
              div(
                class = "potential-method-item potential-method-type",
                pdrb_selectize(
                  "jenis_analisis_wilayah",
                  "Metode Analisis",
                  choices = c(
                    "Location Quotient (LQ)" = "lq",
                    "Dynamic Location Quotient (DLQ)" = "dlq",
                    "Extended Shift Share" = "shift_share"
                  ),
                  selected = "lq"
                )
              ),

              # LQ: Metode | Dasar Harga | Tingkat Analisis | Jenis Sektor
              conditionalPanel(
                condition = "input.jenis_analisis_wilayah == 'lq'",
                div(
                  class = "potential-method-item",
                  pdrb_selectize(
                    "dasar_harga_lq_tren", "Dasar Harga",
                    choices = c("ADHK" = "ADHK", "ADHB" = "ADHB"),
                    selected = "ADHK"
                  )
                )
              ),

              # Tingkat Analisis dipakai oleh LQ dan DLQ, tetapi tidak Extended Extended Shift Share.
              conditionalPanel(
                condition = "input.jenis_analisis_wilayah != 'shift_share'",
                div(
                  class = "potential-method-item",
                  pdrb_selectize(
                    "potensi_level_kategori", "Tingkat Analisis",
                    choices = c(
                      "Semua" = "Semua",
                      "Kategori Utama" = "Kategori Utama",
                      "Subkategori" = "Subkategori",
                      "Rincian Subkategori" = "Rincian"
                    ),
                    selected = "Semua"
                  )
                )
              ),

              conditionalPanel(
                condition = "input.jenis_analisis_wilayah == 'lq'",
                div(
                  class = "potential-method-item",
                  pdrb_selectize(
                    "kategori_lq_tren", "Jenis Sektor",
                    choices = NULL,
                    placeholder = "Pilih jenis sektor"
                  )
                )
              ),

              # DLQ: Metode | Tingkat Analisis | Jenis Sektor
              conditionalPanel(
                condition = "input.jenis_analisis_wilayah == 'dlq'",
                div(
                  class = "potential-method-item",
                  pdrb_selectize(
                    "kategori_dlq", "Jenis Sektor",
                    choices = c("Semua Sektor" = "__ALL__"),
                    selected = "__ALL__"
                  )
                )
              ),

              # Extended Shift Share: Metode | Tahun Awal | Tahun Akhir | Komponen Grafik
              conditionalPanel(
                condition = "input.jenis_analisis_wilayah == 'shift_share'",
                div(
                  class = "potential-method-item",
                  pdrb_selectize(
                    "tahun_awal_shift_analisis", "Tahun Awal",
                    choices = NULL,
                    placeholder = "Pilih tahun awal"
                  )
                )
              ),
              conditionalPanel(
                condition = "input.jenis_analisis_wilayah == 'shift_share'",
                div(
                  class = "potential-method-item",
                  pdrb_selectize(
                    "tahun_akhir_shift_analisis", "Tahun Akhir",
                    choices = NULL,
                    placeholder = "Pilih tahun akhir"
                  )
                )
              ),
              conditionalPanel(
                condition = "input.jenis_analisis_wilayah == 'shift_share'",
                div(
                  class = "potential-method-item",
                  pdrb_selectize(
                    "komponen_shift_plot", "Komponen Grafik",
                    choices = c("CE", "RIE", "RSE", "NE", "IM", "RCCE"),
                    selected = "CE"
                  )
                )
              )
            ),

            # LQ baris kedua: Tahun | Periode | Tampilan Grafik
            conditionalPanel(
              condition = "input.jenis_analisis_wilayah == 'lq'",
              div(
                class = "potential-method-secondary potential-method-secondary-3",
                div(
                  class = "potential-method-item",
                  pdrb_selectize(
                    "tahun_lq", "Tahun",
                    choices = c("Semua Tahun" = "__ALL__"),
                    selected = "__ALL__"
                  )
                ),
                div(
                  class = "potential-method-item",
                  pdrb_selectize(
                    "periode_lq_peringkat", "Periode",
                    choices = c(
                      "Semua Periode" = "__ALL__",
                      "Semua Triwulan" = "__QUARTERS__",
                      "Triwulan I" = "I",
                      "Triwulan II" = "II",
                      "Triwulan III" = "III",
                      "Triwulan IV" = "IV",
                      "Tahun" = "Total"
                    ),
                    selected = "__QUARTERS__"
                  )
                ),
                div(
                  class = "potential-method-item",
                  pdrb_selectize(
                    "tampilan_lq", "Tampilan Grafik",
                    choices = c(
                      "Tren Sektor Terpilih" = "tren_sektor",
                      "Tren Semua Sektor" = "tren_semua",
                      "Ranking Location Quotient (LQ)" = "peringkat"
                    ),
                    selected = "tren_sektor"
                  )
                )
              )
            ),

            # DLQ baris kedua: Tahun Awal | Tahun Akhir.
            # Dasar harga otomatis ADHK dan periode otomatis Tahun.
            conditionalPanel(
              condition = "input.jenis_analisis_wilayah == 'dlq'",
              div(
                class = "potential-method-secondary potential-method-secondary-2",
                div(
                  class = "potential-method-item",
                  pdrb_selectize(
                    "tahun_awal_analisis", "Tahun Awal",
                    choices = c("Semua Tahun" = "__ALL__"),
                    selected = "__ALL__",
                    placeholder = "Pilih tahun awal"
                  )
                ),
                div(
                  class = "potential-method-item",
                  pdrb_selectize(
                    "tahun_akhir_analisis", "Tahun Akhir",
                    choices = c("Semua Tahun" = "__ALL__"),
                    selected = "__ALL__",
                    placeholder = "Pilih tahun akhir"
                  )
                )
              )
            )
          )
        )
      ),
      conditionalPanel(
        condition = "output.reference_region_missing === true",
        fluidRow(
          box(
            title = tagList(icon("warning"), " Status Analisis Potensi Wilayah"),
            width = 12, status = "warning", solidHeader = TRUE,
            uiOutput("reference_region_status"),
            uiOutput("analysis_same_region_warning")
          )
        )
      ),
      fluidRow(
        valueBoxOutput("vb_analysis_1", width = 4),
        valueBoxOutput("vb_analysis_2", width = 4),
        valueBoxOutput("vb_analysis_3", width = 4)
      ),
      fluidRow(
        box(
          title = tagList(icon("commenting"), " Interpretasi Singkat"),
          width = 12, status = "warning", solidHeader = TRUE,
          uiOutput("analysis_interpretation")
        )
      ),
      conditionalPanel(
        condition = "input.jenis_analisis_wilayah == 'lq'",
        fluidRow(
          box(
            title = tagList(icon("bar-chart"), " Grafik Location Quotient (LQ)"),
            width = 12, status = "primary", solidHeader = TRUE,
            pdrb_plotly_output("plot_lq_v5", height = 500, loading_text = "Memuat hasil LQ...")
          )
        ),
        fluidRow(
          box(
            title = tagList(icon("table"), " Tabel Location Quotient (LQ)"),
            width = 12, status = "primary", solidHeader = TRUE,
            div(
              class = "table-download-actions",
              style = "display:flex; gap:10px; flex-wrap:wrap; align-items:center;",
              downloadButton("download_lq_excel", "Unduh Excel (.xlsx)", class = "btn-primary"),
              downloadButton("download_lq_csv", "Unduh CSV", class = "btn-success")
            ),
            tags$div(style = "height: 10px;"),
            DT::DTOutput("lq_table_v5")
          )
        )
      ),
      conditionalPanel(
        condition = "input.jenis_analisis_wilayah == 'dlq'",
        fluidRow(
          box(
            title = tagList(icon("th"), " Kuadran Dynamic Location Quotient (DLQ)"),
            width = 12, status = "success", solidHeader = TRUE,
            pdrb_plotly_output("plot_dlq_v5", height = 420, loading_text = "Memuat hasil DLQ...")
          )
        ),
        fluidRow(
          box(
            title = tagList(icon("table"), " Tabel Dynamic Location Quotient (DLQ)"),
            width = 12, status = "success", solidHeader = TRUE,
            div(
              class = "table-download-actions",
              style = "display:flex; gap:10px; flex-wrap:wrap; align-items:center;",
              downloadButton("download_dlq_excel", "Unduh Excel (.xlsx)", class = "btn-primary"),
              downloadButton("download_dlq_csv", "Unduh CSV", class = "btn-success")
            ),
            tags$div(style = "height: 10px;"),
            DT::DTOutput("dlq_table_v5")
          )
        )
      ),
      conditionalPanel(
        condition = "input.jenis_analisis_wilayah == 'shift_share'",
        fluidRow(
          box(
            title = tagList(icon("area-chart"), " Komponen Extended Shift Share"),
            width = 12, status = "info", solidHeader = TRUE,
            pdrb_plotly_output("plot_shiftshare_v5", height = 450, loading_text = "Memuat hasil Extended Shift Share...")
          )
        ),
        fluidRow(
          box(
            title = tagList(icon("table"), " Tabel Extended Shift Share"),
            width = 12, status = "info", solidHeader = TRUE,
            div(
              class = "table-download-actions",
              style = "display:flex; gap:10px; flex-wrap:wrap; align-items:center;",
              downloadButton("download_shiftshare_excel", "Unduh Excel (.xlsx)", class = "btn-primary"),
              downloadButton("download_shiftshare_csv", "Unduh CSV", class = "btn-success")
            ),
            tags$div(style = "height: 10px;"),
            DT::DTOutput("shiftshare_table_v5")
          )
        )
      )
    ),
    
    tabItem(
      tabName = "unduh_laporan",
      page_intro(
        "Output",
        "Laporan",
        NULL,
        "file-lines"
      ),
      fluidRow(
        box(
          title = tagList(icon("sliders"), " Pengaturan Laporan"),
          width = 12, status = "info", solidHeader = TRUE,
          fluidRow(
            column(
              4,
              pdrb_selectize("laporan_wilayah", "Wilayah", choices = NULL, placeholder = "Pilih wilayah")
            ),
            column(
              4,
              pdrb_selectize("laporan_tahun", "Tahun Fokus", choices = NULL, placeholder = "Pilih tahun fokus")
            ),
            column(
              4,
              pdrb_selectize(
                "laporan_periode", "Periode Fokus",
                choices = c(
                  "Periode Terbaru" = "__LATEST__",
                  "Triwulan I" = "I",
                  "Triwulan II" = "II",
                  "Triwulan III" = "III",
                  "Triwulan IV" = "IV",
                  "Tahun" = "Total"
                ),
                selected = "__LATEST__",
                placeholder = "Pilih periode fokus"
              )
            )
          ),
          fluidRow(
            column(
              3,
              pdrb_selectize("laporan_tahun_awal", "Tahun Awal Tren", choices = NULL, placeholder = "Pilih tahun awal")
            ),
            column(
              3,
              pdrb_selectize("laporan_tahun_akhir", "Tahun Akhir Tren", choices = NULL, placeholder = "Pilih tahun akhir")
            ),
            column(
              3,
              pdrb_selectize(
                "laporan_jenis", "Jenis Laporan",
                choices = c("Ringkas" = "ringkas", "Lengkap" = "lengkap"),
                selected = "ringkas"
              )
            ),
            column(
              3,
              pdrb_selectize(
                "laporan_format", "Format File",
                choices = c("Word (.docx)" = "docx", "PDF (.pdf)" = "pdf"),
                selected = "docx"
              )
            )
          ),
          br(),
          downloadButton("download_laporan_pdrb", "Unduh Laporan", class = "btn-primary")
        )
      ),
      fluidRow(
        box(
          title = tagList(icon("eye"), " Preview Output"),
          width = 12, status = "primary", solidHeader = TRUE,
          uiOutput("laporan_preview")
        )
      )
    ),
    
    tabItem(
      tabName = "penjelasan",
      page_intro(
        "Bantuan",
        "Panduan Penggunaan Dashboard",
        NULL,
        "question-circle"
      ),

      fluidRow(
        box(
          title = tagList(icon("list-ol"), " Alur Penggunaan"),
          width = 12, status = "primary", solidHeader = TRUE,
          class = "help-section-box",
          div(
            class = "help-step-grid",
            help_step_card(
              "1", "upload", "Unggah Data",
              "Pilih file Excel PDRB provinsi/agregat dan/atau kabupaten/kota. Salah satu jenis file dapat digunakan secara mandiri.",
              "Menu: Upload Data"
            ),
            help_step_card(
              "2", "database", "Proses dan Periksa Data",
              "Klik Proses Data, kemudian periksa file, sheet, wilayah, tahun, periode, dan jenis data yang berhasil dibaca.",
              "Pastikan tidak ada tabel penting yang gagal dibaca."
            ),
            help_step_card(
              "3", "line-chart", "Lakukan Analisis",
              "Pilih wilayah pada Filter Global, kemudian gunakan menu analisis sesuai kebutuhan pengguna.",
              "Overview, Tren, Distribusi, Struktur, Komparasi, dan Potensi Wilayah."
            ),
            help_step_card(
              "4", "download", "Unduh Hasil",
              "Unduh tabel dalam format Excel atau CSV, atau buat laporan analisis dalam format Word dan PDF.",
              "Menu: Tabel Data dan Laporan"
            )
          )
        )
      ),

      fluidRow(
        box(
          title = tagList(icon("file-excel-o"), " Format Data"),
          width = 12, status = "info", solidHeader = TRUE,
          class = "help-section-box",
          div(
            class = "help-two-column",
            div(
              class = "help-subpanel",
              h4(class = "help-subpanel-title", icon("database"), " Komponen Data"),
              div(
                class = "help-item-list",
                help_item_card("table", "Data Utama", "File Excel harus memuat nilai PDRB atas dasar harga berlaku dan/atau atas dasar harga konstan."),
                help_item_card("custom-location", "Wilayah", "Wilayah dikenali berdasarkan kode wilayah empat digit, nama wilayah, atau keterangan 'Wilayah: Nama Wilayah' pada sheet."),
                help_item_card("calendar", "Waktu", "Data dapat memuat Triwulan I, Triwulan II, Triwulan III, Triwulan IV, dan Tahun."),
                help_item_card("tags", "Sektor", "Gunakan Template 17 Lapangan Usaha untuk kategori utama A–U, atau Template Lengkap untuk kategori, subkategori, dan rincian kegiatan ekonomi.")
              )
            ),
            div(
              class = "help-subpanel",
              h4(class = "help-subpanel-title", icon("check-circle"), " Ketentuan Pembacaan"),
              div(
                class = "help-item-list",
                help_item_card("file-text", "Nama Sheet", "Nama sheet boleh berbeda selama isi sheet memiliki penanda wilayah dan struktur data PDRB yang dapat dikenali."),
                help_item_card("ban", "Nilai Kosong", "Sel kosong, NA, tanda hubung, nilai referensi yang rusak, dan periode tanpa nilai tidak digunakan dalam perhitungan."),
                help_item_card("calculator", "Data Turunan", "Distribusi, pertumbuhan, indeks implisit, LQ, DLQ, dan Extended Shift Share tidak perlu diunggah karena dihitung otomatis."),
                help_item_card("exchange", "Data Pembanding", "Analisis LQ, DLQ, dan Extended Shift Share memerlukan data kabupaten/kota serta data provinsi sebagai wilayah pembanding.")
              )
            )
          ),
          div(
            class = "help-template-actions",
            downloadButton(
              "help_download_template_provinsi",
              "Unduh Template 17 Lapangan Usaha (.xlsx)",
              class = "btn-success"
            ),
            downloadButton(
              "help_download_template_kabkota",
              "Unduh Template Lengkap (.xlsx)",
              class = "btn-success"
            )
          )
        )
      ),

      fluidRow(
        box(
          title = tagList(icon("th-large"), " Fungsi Menu"),
          width = 12, status = "primary", solidHeader = TRUE,
          class = "help-section-box",
          div(
            class = "help-menu-group",
            h4(class = "help-group-title", icon("briefcase"), " Workspace"),
            div(
              class = "help-menu-grid",
              help_menu_card("upload", "Upload Data", "Mengunggah, memproses, memvalidasi, dan menampilkan pratinjau data PDRB."),
              help_menu_card("dashboard", "Overview", "Menampilkan ringkasan PDRB ADHB, PDRB ADHK, pertumbuhan, sektor terbesar, tren, dan lapangan usaha utama.")
            )
          ),
          div(
            class = "help-menu-group",
            h4(class = "help-group-title", icon("bar-chart"), " Analytics"),
            div(
              class = "help-menu-grid",
              help_menu_card("line-chart", "Tren PDRB", "Menampilkan perkembangan indikator berdasarkan rentang tahun dan periode yang dipilih."),
              help_menu_card("bar-chart", "Distribusi Data", "Menampilkan histogram, kurva kernel, dan statistik deskriptif data."),
              help_menu_card("pie-chart", "Struktur Ekonomi", "Menampilkan komposisi serta kontribusi lapangan usaha terhadap perekonomian wilayah."),
              help_menu_card("balance-scale", "Komparasi Wilayah", "Membandingkan nilai indikator antarwilayah pada tahun dan periode yang sama."),
              help_menu_card("custom-location", "Potensi Wilayah", "Menilai sektor basis, prospek sektor, dan daya saing wilayah melalui LQ, DLQ, dan Extended Shift Share.")
            )
          ),
          div(
            class = "help-menu-group",
            h4(class = "help-group-title", icon("folder-open"), " Output"),
            div(
              class = "help-menu-grid",
              help_menu_card("table", "Tabel Data", "Menampilkan hasil data dan indikator dalam format Long atau Wide serta menyediakan unduhan Excel dan CSV."),
              help_menu_card("file-text", "Laporan", "Menghasilkan laporan analisis Ringkas atau Lengkap dalam format Word dan PDF.")
            )
          ),
          div(
            class = "help-menu-group",
            h4(class = "help-group-title", icon("life-ring"), " Support"),
            div(
              class = "help-menu-grid",
              help_menu_card("question-circle", "Bantuan", "Menyediakan panduan penggunaan, arti indikator, dan solusi masalah umum.")
            )
          )
        )
      ),

      fluidRow(
        box(
          title = tagList(icon("info-circle"), " Arti Indikator"),
          width = 12, status = "info", solidHeader = TRUE,
          class = "help-section-box",
          div(
            class = "help-indicator-groups",
            div(
              class = "help-indicator-group",
              h4(class = "help-group-title", icon("database"), " Indikator Dasar"),
              help_item_card("bar-chart", "PDRB ADHB", "Nilai PDRB berdasarkan harga yang berlaku pada periode berjalan. Digunakan untuk melihat nilai nominal dan struktur ekonomi."),
              help_item_card("database", "PDRB ADHK", "Nilai PDRB berdasarkan harga konstan. Digunakan untuk melihat perubahan volume produksi dan pertumbuhan ekonomi riil."),
              help_item_card("pie-chart", "Distribusi PDRB", "Persentase kontribusi suatu lapangan usaha terhadap total PDRB wilayah.")
            ),
            div(
              class = "help-indicator-group",
              h4(class = "help-group-title", icon("line-chart"), " Indikator Perkembangan"),
              help_item_card("refresh", "Pertumbuhan Q-to-Q", "Perubahan nilai dibandingkan dengan triwulan sebelumnya."),
              help_item_card("calendar", "Pertumbuhan Y-on-Y", "Perubahan nilai dibandingkan dengan triwulan yang sama pada tahun sebelumnya."),
              help_item_card("area-chart", "Pertumbuhan C-to-C", "Perubahan kumulatif dibandingkan dengan periode kumulatif yang sama pada tahun sebelumnya."),
              help_item_card("signal", "Indeks Implisit", "Perbandingan antara PDRB ADHB dan PDRB ADHK yang menggambarkan perubahan tingkat harga secara umum."),
              help_item_card("share-alt", "Sumber Pertumbuhan", "Besarnya kontribusi setiap lapangan usaha terhadap pertumbuhan ekonomi total.")
            ),
            div(
              class = "help-indicator-group",
              h4(class = "help-group-title", icon("map"), " Indikator Potensi Wilayah"),
              help_item_card("crosshairs", "Location Quotient (LQ)", "Membandingkan peranan sektor di wilayah analisis dengan sektor yang sama di wilayah pembanding. Nilai di atas 1 menunjukkan sektor basis."),
              help_item_card("custom-location", "Dynamic Location Quotient (DLQ)", "Menilai perubahan prospek sektor berdasarkan pertumbuhan sektor wilayah dan wilayah pembanding pada dua tahun."),
              help_item_card("exchange", "Extended Shift Share", "Menguraikan perubahan sektor berdasarkan pertumbuhan umum, struktur industri, daya saing, dan dinamika regional.")
            )
          )
        )
      ),

      fluidRow(
        box(
          title = tagList(icon("exclamation-circle"), " Masalah Umum dan Solusi"),
          width = 12, status = "warning", solidHeader = TRUE,
          class = "help-section-box",
          div(
            class = "help-accordion-grid",
            help_accordion_item("file", "File tidak dapat dibaca", "Pastikan file berformat Excel (.xlsx atau .xls) dan memuat data PDRB ADHB dan/atau ADHK. Periksa kembali wilayah, tahun, periode, kode kategori, dan nama lapangan usaha, lalu unggah ulang dan klik Proses Data."),
            help_accordion_item("custom-location", "Wilayah tidak dikenali", "Pastikan file memuat kode wilayah empat digit atau nama wilayah yang jelas. Keterangan seperti 'Wilayah: Kabupaten Pandeglang' atau 'Wilayah: Provinsi Banten' dapat membantu dashboard mengenali wilayah."),
            help_accordion_item("calendar", "Tahun atau periode tidak muncul", "Tahun dan periode hanya ditampilkan jika memiliki data yang berhasil dibaca. Periksa apakah nilainya kosong, menggunakan tanda hubung, atau belum lengkap, lalu proses kembali file yang telah diperbaiki."),
            help_accordion_item("bar-chart", "Grafik atau tabel kosong", "Periksa pilihan wilayah, tahun, periode, jenis nilai, tingkat analisis, dan sektor. Cobalah memilih cakupan filter yang lebih luas karena kombinasi filter yang terlalu khusus dapat tidak memiliki data."),
            help_accordion_item("line-chart", "Pertumbuhan tidak tersedia", "Pertumbuhan memerlukan periode pembanding. Q-to-Q membutuhkan triwulan sebelumnya, Y-on-Y membutuhkan periode yang sama pada tahun sebelumnya, dan C-to-C membutuhkan data kumulatif pembanding."),
            help_accordion_item("custom-location", "LQ, DLQ, atau Extended Shift Share tidak tersedia", "Pastikan data wilayah analisis dan provinsi pembanding tersedia. LQ memerlukan tahun dan periode yang sama, DLQ memerlukan dua tahun tahunan lengkap, sedangkan Extended Shift Share memerlukan tahun awal dan akhir yang valid."),
            help_accordion_item("refresh", "Data baru belum tampil", "Setelah memilih file baru, klik Proses Data dan tunggu hingga status pembacaan selesai. Memilih file saja belum mengganti data pada menu analisis."),
            help_accordion_item("download", "File unduhan tidak muncul", "Pastikan browser mengizinkan unduhan dari dashboard dan periksa folder Downloads. Klik tombol unduh kembali setelah tabel atau laporan selesai ditampilkan."),
            help_accordion_item("file-text", "Laporan tidak dapat diunduh", "Pastikan data sudah selesai diproses, seluruh filter laporan telah terisi, dan rentang tahun yang dipilih valid. Tunggu hingga proses selesai dan hindari menekan tombol unduh berulang kali."),
            help_accordion_item("file-text", "File laporan terunduh sebagai HTML", "File HTML menunjukkan laporan Word atau PDF belum berhasil dibuat. Hapus file tersebut, kembali ke menu Laporan, periksa kembali filter, lalu coba unduh dalam format Word terlebih dahulu.")
          )
        )
      ),

      fluidRow(
        box(
          title = tagList(icon("info-circle"), " Catatan Penting"),
          width = 12, status = "primary", solidHeader = TRUE,
          class = "help-section-box",
          div(
            class = "help-important-note",
            icon("info-circle"),
            div(
              strong("Gunakan hasil analisis secara proporsional"),
              p("Dashboard hanya menghitung indikator berdasarkan data yang berhasil dibaca dan dinyatakan valid. Hasil analisis bersifat deskriptif dan perlu dikaji bersama konteks ekonomi, kebijakan, serta sumber informasi pendukung lainnya.")
            )
          ),
          div(
            class = "help-note-grid",
            help_item_card("table", "Verifikasi Hasil", "Periksa Tabel Data untuk memastikan hasil perhitungan sebelum membuat laporan.", "compact"),
            help_item_card("calendar", "Tahun Berjalan", "Tahun berjalan dapat memiliki data triwulanan meskipun nilai Tahun belum lengkap.", "compact"),
            help_item_card("shield", "Privasi File", "File digunakan untuk proses analisis dan bukan sebagai tempat penyimpanan dokumen permanen.", "compact")
          )
        )
      )
    )
    
  )
)

