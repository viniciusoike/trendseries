# LinkedIn document carousel

Eight portrait pages: a cover, six feature examples, and an install card.
Upload `trendseries-carousel.pdf` as a LinkedIn document. `pages/page-01.png`
through `page-08.png` are automated 1080 × 1350 exports; `index.html` is a
self-contained preview of every page.

## Rebuild

Keep the trendseries, ekioplot, and ekiotools repositories next to each
other. ekioplot supplies the chart theme, and ekiotools supplies the carousel
API. From the trendseries root:

```sh
Rscript promo/linkedin/build.R
```

The script loads the three local packages with pkgload. It also needs here, dplyr,
ggplot2, scales, tsibble, chromote, jsonlite, and Chrome. Install Lato and
DejaVu Sans Mono locally for the intended fonts; the browser falls back to
sans-serif and monospace if unavailable. No font download is needed.

Each feature's displayed code is stored once and evaluated verbatim. Its result
supplies all chart data or the printed output. Plot styling is in `build.R`.
Examples assume trendseries is loaded, as in the build script. Data come from
the package snapshot documented in `R/data.R`, not a live data request.

## Reuse for another package

Keep release content and computations in that package's `promo/` directory.
Create a brand and cards, then pass them to the ekiotools renderer:

```r
brand <- ekiotools::promo_brand(
  "mypackage", "1.0.0", logo = "logo.png",
  docs_url = "https://example.org/mypackage/"
)
cards <- list(
  ekiotools::promo_cover("Work with time series", "An example in R."),
  ekiotools::promo_feature("Calculate a summary", code = "mean(1:10)", output = "[1] 5.5"),
  ekiotools::promo_closing("Try it in R")
)
ekiotools::render_promo(cards, brand, "carousel.html", png_dir = "pages")
```

Feature cards accept a chart with alt text, code, output, and a short note.
Omitted sections create no placeholders. The renderer embeds images, waits for
fonts and images, and rejects overflowing cards. Cut content if export fails.
The PDF uses CSS pages of 1080 × 1350 px; Chrome may round PDF dimensions by a
fraction of a point. PNGs retain the exact pixel dimensions.
