#' Export a Markdown Document to an ABMI-Styled docx or PDF
#'
#' Convert a pandoc Markdown file to a Word document built on the ABMI
#' reference doc, or to a PDF typeset with LaTeX (xelatex) in the ABMI look
#' (Montserrat/Lora type, navy and sage palette, ABMI header and footer).
#'
#' On top of plain pandoc, the export adds:
#' * **Numbered captions and cross-references.** End a table or figure
#'   caption with a label, e.g. `: Caption. {#tbl-parity}` or
#'   `![Caption.](map.png){#fig-map}`; `@tbl-parity` or `[@tbl-parity]` in
#'   the text becomes a link reading "Table 1". Labels start `tbl-` or `fig-`.
#' * **Citations** with `--citeproc`, from `bibliography` (or the document's
#'   own `bibliography:` metadata) in the `csl` style.
#' * **Code blocks** in a monospaced font on a light grey background, with
#'   syntax highlighting (fenced blocks with a language, e.g. ```` ```r ````).
#' * **Centred figures and captions.**
#' * **Mermaid diagrams** rendered with mermaid-cli (`mmdc`) if installed
#'   (`npm install -g @mermaid-js/mermaid-cli`); otherwise they are exported
#'   as code with a warning. A `%% width: 60%` line in the block sets the
#'   diagram's width (also `4in`, `10cm`, `400px`).
#'
#' @param input Path to the Markdown file.
#' @param format `"docx"` or `"pdf"`.
#' @param output Output path. Defaults to `input` with the format's extension.
#' @param reference_doc ABMI reference docx. Gives the docx its styles, header
#'   and footer, and the PDF its header logo and footer art (the first image
#'   in the reference doc's header and footer). Set once per session with
#'   `options(utilR.abmi_reference_doc = "path/to/ref.docx")`.
#' @param bibliography Optional `.bib` (or CSL JSON) file for citations.
#' @param csl Optional CSL style file; pandoc's default is Chicago author-date.
#' @param toc Add a table of contents? In a docx it is a Word field that Word
#'   offers to fill in when the file is first opened.
#' @param toc_depth Heading levels in the table of contents.
#' @param number_sections Number the headings?
#' @param code_font Monospaced font for code in a docx (Consolas comes with
#'   Microsoft Office; the PDF uses Roboto Mono).
#' @param link_colour Hex colour for links, without `#`.
#' @param pandoc_args Extra arguments passed to pandoc as is.
#' @param open Open the result when done?
#' @param quiet Suppress pandoc's warnings and the "Wrote" message?
#'
#' @return Invisibly, the path to the written file.
#' @examples
#' \dontrun{
#' export_abmi_document("methods.md")
#' export_abmi_document("methods.md", "pdf", toc = TRUE,
#'                      bibliography = "~/Dropbox/3_resources/pandoc/library.bib",
#'                      csl = "~/Zotero/styles/ecology.csl")
#' }
#' @export
export_abmi_document <- function(input,
                                 format = c("docx", "pdf"),
                                 output = NULL,
                                 reference_doc = getOption(
                                     "utilR.abmi_reference_doc",
                                     "~/Dropbox/3_resources/pandoc/referenceDocs/abmi_ref_doc.docx"
                                 ),
                                 bibliography = NULL,
                                 csl = NULL,
                                 toc = FALSE,
                                 toc_depth = 3,
                                 number_sections = FALSE,
                                 code_font = "Consolas",
                                 link_colour = "4A90E2",
                                 pandoc_args = character(),
                                 open = FALSE,
                                 quiet = FALSE) {
    format <- match.arg(format)

    # 1. Check inputs and tools ----
    input <- normalizePath(input, mustWork = TRUE)
    if (is.null(output)) {
        output <- paste0(tools::file_path_sans_ext(input), ".", format)
    }
    output <- normalizePath(output, mustWork = FALSE)
    reference_doc <- if (!is.null(reference_doc)) path.expand(reference_doc)
    if (format == "docx" && (is.null(reference_doc) || !file.exists(reference_doc))) {
        stop("Reference doc not found: ", reference_doc,
             "\nSet `reference_doc` or options(utilR.abmi_reference_doc = ...).")
    }
    pandoc <- abmi_find_pandoc()
    if (format == "pdf" && !nzchar(Sys.which("xelatex"))) {
        stop("xelatex not found; a TeX distribution (e.g. TeX Live, or ",
             "tinytex::install_tinytex()) is needed for PDF output.")
    }
    resources <- abmi_resources()

    work <- tempfile("abmi_export_")
    dir.create(work)
    on.exit(unlink(work, recursive = TRUE), add = TRUE)

    # 2. Arguments shared by both formats ----
    mmdc <- Sys.which("mmdc")
    args <- c(
        input,
        "--from=markdown",
        "--standalone",
        paste0("--resource-path=", dirname(input)),
        "--metadata", paste0("mermaid-dir=", work),
        if (nzchar(mmdc)) c("--metadata", paste0("mermaid-cli=", mmdc)),
        paste0("--lua-filter=", file.path(resources, "abmi_mermaid.lua")),
        paste0("--lua-filter=", file.path(resources, "abmi_crossref.lua")),
        "--citeproc",
        if (!is.null(bibliography)) {
            paste0("--bibliography=", normalizePath(bibliography, mustWork = TRUE))
        },
        if (!is.null(csl)) paste0("--csl=", normalizePath(csl, mustWork = TRUE)),
        if (toc) c("--toc", paste0("--toc-depth=", toc_depth)),
        if (number_sections) "--number-sections"
    )

    # 3. Format-specific arguments ----
    if (format == "docx") {
        ref <- abmi_docx_reference(reference_doc, work, code_font, link_colour)
        args <- c(args, paste0("--reference-doc=", ref))
    } else {
        header <- abmi_latex_header(resources, reference_doc, work, link_colour)
        args <- c(
            args,
            "--pdf-engine=xelatex",
            paste0("--include-in-header=", header),
            "--variable=subparagraph",
            "--variable=papersize=letter",
            "--variable=geometry:margin=1in",
            "--variable=fontsize=11pt",
            "--variable=colorlinks=true",
            "--variable=linkcolor=abmilink",
            "--variable=urlcolor=abmilink",
            "--variable=citecolor=abmilink",
            "--variable=toccolor=abminavy"
        )
    }
    args <- c(args, pandoc_args, if (quiet) "--quiet", "-o", output)

    # 4. Run pandoc ----
    # pandoc (and LaTeX) write their scratch files to TMPDIR; point it at the
    # working folder so a TMPDIR that doesn't exist on this machine can't break it.
    old_tmp <- Sys.getenv("TMPDIR", unset = NA)
    Sys.setenv(TMPDIR = work)
    on.exit(if (is.na(old_tmp)) Sys.unsetenv("TMPDIR") else Sys.setenv(TMPDIR = old_tmp),
            add = TRUE)
    status <- system2(pandoc, shQuote(args))
    if (!identical(status, 0L) || !file.exists(output)) {
        stop("pandoc failed (exit status ", status, "); see the messages above.")
    }
    if (!quiet) message("Wrote ", output)
    if (open) browseURL(output)
    invisible(output)
}

# The package's pandoc filters and LaTeX header: inst/pandoc in the source (e.g.
# under devtools::load_all()), pandoc/ once installed, or the folder in
# options(utilR.abmi_resources = ...), e.g. when this file is sourced on its own.
abmi_resources <- function() {
    dir <- getOption("utilR.abmi_resources")
    ns <- topenv(environment(abmi_resources))
    if (is.null(dir) && isNamespace(ns)) {
        root <- getNamespaceInfo(ns, "path")
        dir <- c(file.path(root, "pandoc"), file.path(root, "inst", "pandoc"))
        dir <- dir[file.exists(file.path(dir, "abmi_crossref.lua"))][1]
    }
    if (is.null(dir) || is.na(dir) || !file.exists(file.path(dir, "abmi_crossref.lua"))) {
        stop("The package's pandoc resources (inst/pandoc) were not found; set ",
             "options(utilR.abmi_resources = \"path/to/inst/pandoc\").")
    }
    normalizePath(dir)
}

# Path to pandoc: on the PATH, else the copy bundled with RStudio (via rmarkdown).
abmi_find_pandoc <- function() {
    pandoc <- Sys.which("pandoc")
    if (!nzchar(pandoc) && requireNamespace("rmarkdown", quietly = TRUE) &&
        rmarkdown::pandoc_available()) {
        pandoc <- file.path(rmarkdown::find_pandoc()$dir, "pandoc")
    }
    if (!nzchar(pandoc)) stop("pandoc not found; install it from https://pandoc.org.")
    version <- system2(pandoc, "--version", stdout = TRUE)[1]
    if (numeric_version(sub("^pandoc\\S* ", "", version)) < "3.1") {
        stop("pandoc 3.1 or later is needed; found ", version, ".")
    }
    unname(pandoc)
}

# A copy of the reference doc with the styles the export relies on added, unless
# the reference doc already defines them (then its own look is kept). pandoc would
# otherwise create these with no formatting: captions and figures left-aligned, code
# in the body font, links indistinguishable from text.
abmi_docx_reference <- function(reference_doc, work, code_font, link_colour) {
    ref <- file.path(work, "reference.docx")
    file.copy(reference_doc, ref)
    unzip(ref, files = "word/styles.xml", exdir = work)
    styles_path <- file.path(work, "word", "styles.xml")
    styles <- paste(readLines(styles_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

    font <- sprintf('<w:rFonts w:ascii="%1$s" w:hAnsi="%1$s" w:cs="%1$s" w:eastAsia="%1$s"/>', code_font)
    shade <- '<w:shd w:val="clear" w:color="auto" w:fill="F3F4F6"/>'
    centre <- '<w:jc w:val="center"/>'
    para <- function(id, name, ppr, rpr = "") {
        sprintf(paste0('<w:style w:type="paragraph" w:customStyle="1" w:styleId="%s">',
                       '<w:name w:val="%s"/><w:basedOn w:val="Normal"/><w:pPr>%s</w:pPr>',
                       '<w:rPr>%s</w:rPr></w:style>'), id, name, ppr, rpr)
    }
    new_styles <- c(
        "Table Caption" = para("TableCaption", "Table Caption",
                               paste0('<w:keepNext/><w:spacing w:before="160" w:after="60"/>', centre),
                               '<w:i/><w:sz w:val="18"/><w:szCs w:val="18"/>'),
        "Image Caption" = para("ImageCaption", "Image Caption",
                               paste0('<w:spacing w:before="60" w:after="160"/>', centre),
                               '<w:i/><w:sz w:val="18"/><w:szCs w:val="18"/>'),
        "Captioned Figure" = para("CaptionedFigure", "Captioned Figure",
                                  paste0('<w:keepNext/><w:spacing w:before="160" w:after="0"/>', centre)),
        "Figure" = para("Figure", "Figure",
                        paste0('<w:keepNext/><w:spacing w:before="160" w:after="0"/>', centre)),
        "Centred Image" = para("CentredImage", "Centred Image",
                               paste0('<w:spacing w:before="160" w:after="160"/>', centre)),
        "Source Code" = sprintf(paste0(
            '<w:style w:type="paragraph" w:customStyle="1" w:styleId="SourceCode">',
            '<w:name w:val="Source Code"/><w:basedOn w:val="Normal"/><w:link w:val="VerbatimChar"/>',
            '<w:pPr>%s<w:spacing w:before="120" w:after="120" w:line="240" w:lineRule="auto"/></w:pPr>',
            '<w:rPr>%s<w:sz w:val="18"/><w:szCs w:val="18"/></w:rPr></w:style>'), shade, font),
        "Verbatim Char" = sprintf(paste0(
            '<w:style w:type="character" w:customStyle="1" w:styleId="VerbatimChar">',
            '<w:name w:val="Verbatim Char"/><w:link w:val="SourceCode"/>',
            '<w:rPr>%s<w:sz w:val="18"/><w:szCs w:val="18"/>%s</w:rPr></w:style>'), font, shade),
        "Hyperlink" = sprintf(paste0(
            '<w:style w:type="character" w:styleId="Hyperlink"><w:name w:val="Hyperlink"/>',
            '<w:uiPriority w:val="99"/><w:unhideWhenUsed/><w:rPr><w:color w:val="%s"/>',
            '<w:u w:val="single"/></w:rPr></w:style>'), sub("^#", "", link_colour))
    )
    # pandoc finds styles by name, so check names (case-insensitively) as well as ids.
    has_style <- vapply(names(new_styles), function(name) {
        id <- gsub(" ", "", name)
        grepl(sprintf('w:styleId="%s"', id), styles, fixed = TRUE) ||
            grepl(sprintf('<w:name w:val="%s"', name), styles, ignore.case = TRUE)
    }, logical(1))
    if (any(!has_style)) {
        styles <- sub("</w:styles>",
                      paste0(paste(new_styles[!has_style], collapse = ""), "</w:styles>"),
                      styles, fixed = TRUE)
        writeLines(styles, styles_path, useBytes = TRUE)
        old <- setwd(work)
        on.exit(setwd(old), add = TRUE)
        if (zip(ref, "word/styles.xml", flags = "-q") != 0) {
            stop("Could not update the reference doc's styles (is `zip` installed?).")
        }
    }
    ref
}

# The LaTeX header with the reference doc's header logo and footer art filled in
# (each left out if the reference doc has none, or none is given).
abmi_latex_header <- function(resources, reference_doc, work, link_colour) {
    logo <- footer <- ""
    if (!is.null(reference_doc) && file.exists(reference_doc)) {
        logo <- abmi_docx_part_image(reference_doc, "header", work)
        footer <- abmi_docx_part_image(reference_doc, "footer", work)
    } else {
        message("No reference doc; the PDF will have no logo or footer art.")
    }
    header <- readLines(file.path(resources, "abmi_header.tex"), warn = FALSE, encoding = "UTF-8")
    header <- gsub("@@LOGO@@", logo, header, fixed = TRUE)
    header <- gsub("@@FOOTER@@", footer, header, fixed = TRUE)
    header <- gsub("@@LINK@@", toupper(sub("^#", "", link_colour)), header, fixed = TRUE)
    header <- gsub("@@SERIF@@", abmi_serif_font(), header, fixed = TRUE)
    path <- file.path(work, "abmi_header.tex")
    writeLines(header, path, useBytes = TRUE)
    path
}

# fontspec command for the serif used in subheadings and the header tagline: Lora
# (the ABMI brand serif) loaded from its font files if they're installed, as xelatex
# doesn't always find user-installed fonts by name; otherwise Libertinus Serif,
# which ships with TeX Live.
abmi_serif_font <- function() {
    dirs <- path.expand(c("~/Library/Fonts", "/Library/Fonts", "~/.fonts",
                          "~/.local/share/fonts", "/usr/share/fonts/truetype/lora",
                          file.path(Sys.getenv("LOCALAPPDATA"), "Microsoft/Windows/Fonts"),
                          "C:/Windows/Fonts"))
    found <- dirs[file.exists(file.path(dirs, "Lora-Regular.ttf")) &
                      file.exists(file.path(dirs, "Lora-Italic.ttf"))]
    if (length(found)) {
        sprintf(paste0("\\newfontfamily\\abmiserif{Lora}[Path=%s/, Extension=.ttf, ",
                       "UprightFont=*-Regular, ItalicFont=*-Italic]"),
                gsub("\\\\", "/", normalizePath(found[1])))
    } else {
        paste0("\\newfontfamily\\abmiserif{LibertinusSerif}[Extension=.otf, ",
               "UprightFont=*-Regular, ItalicFont=*-Italic]")
    }
}

# The first PNG/JPEG used by a header (or footer) part of a docx, copied into `work`;
# "" if there is none. LaTeX paths need forward slashes and no spaces, which the
# temporary folder satisfies.
abmi_docx_part_image <- function(docx, part, work) {
    files <- unzip(docx, list = TRUE)$Name
    rels <- sort(grep(sprintf("^word/_rels/%s[0-9]*\\.xml\\.rels$", part), files, value = TRUE))
    for (rel in rels) {
        unzip(docx, files = rel, exdir = work)
        xml <- paste(readLines(file.path(work, rel), warn = FALSE), collapse = "")
        targets <- regmatches(xml, gregexpr('Target="media/[^"]+\\.(png|jpe?g)"', xml, ignore.case = TRUE))[[1]]
        if (length(targets)) {
            media <- paste0("word/", sub('^Target="([^"]+)"$', "\\1", targets[1]))
            unzip(docx, files = media, exdir = work)
            out <- file.path(work, paste0(part, ".", tools::file_ext(media)))
            file.rename(file.path(work, media), out)
            return(gsub("\\\\", "/", out))
        }
    }
    ""
}
