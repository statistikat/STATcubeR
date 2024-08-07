% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recoder.R
\name{sc_recoder}
\alias{sc_recoder}
\title{Recode sc_table objects}
\description{
A collection of methods that can be used to modify an object of class
sc_table by reference. Typical usage is to access the \code{recode} binding
of an \code{sc_table} object and then use method chaining to perform recode
operations.

\if{html}{\out{<div class="sourceCode">}}\preformatted{x <- od_table("OGD_krebs_ext_KREBS_1")
x$recode$
  label_field("C-BERJ-0", "de", "JAHR")$
  label_measure("F-KRE", "de", "Anzahl")
}\if{html}{\out{</div>}}

See the example section for more details.
}
\examples{
x <- od_table("OGD_krebs_ext_KREBS_1")

x$recode$
  label_field("C-KRE_GESCHLECHT-0", "en", "SEX")$
  label_measure("F-KRE", "en", "NUMBER")$
  level("C-KRE_GESCHLECHT-0", "GESCHLECHT-1", "en", "MALE")

x$tabulate("C-KRE_GESCHLECHT-0", "F-KRE")

earnings <- od_table("OGD_veste309_Veste309_1")
earnings$recode$
  total_codes("C-A11-0", "A11-1")$
  total_codes("C-STAATS-0", "STAATS-9")$
  total_codes("C-VEBDL-0", "VEBDL-10")$
  total_codes("C-BESCHV-0", "BESCHV-1")

earnings$total_codes()

earnings$tabulate("C-STAATS-0")
earnings$recode$visible("C-STAATS-0", "STAATS-8", FALSE)
earnings$tabulate("C-STAATS-0")

earnings$recode$
  order("C-A11-0", c("A11-3", "A11-1", "A11-2"))
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-sc_recoder-new}{\code{sc_recoder$new()}}
\item \href{#method-sc_recoder-label_field}{\code{sc_recoder$label_field()}}
\item \href{#method-sc_recoder-label_measure}{\code{sc_recoder$label_measure()}}
\item \href{#method-sc_recoder-level}{\code{sc_recoder$level()}}
\item \href{#method-sc_recoder-total_codes}{\code{sc_recoder$total_codes()}}
\item \href{#method-sc_recoder-visible}{\code{sc_recoder$visible()}}
\item \href{#method-sc_recoder-order}{\code{sc_recoder$order()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-sc_recoder-new"></a>}}
\if{latex}{\out{\hypertarget{method-sc_recoder-new}{}}}
\subsection{Method \code{new()}}{
Create a new recoder instance. This will automatically
be performed during the setup of \code{sc_data} objects
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{sc_recoder$new(x)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{the private environment of an \code{sc_data} object}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-sc_recoder-label_field"></a>}}
\if{latex}{\out{\hypertarget{method-sc_recoder-label_field}{}}}
\subsection{Method \code{label_field()}}{
Change the label of a field variable
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{sc_recoder$label_field(field, language, new)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{field}}{a field code}

\item{\code{language}}{a language, "de" or "en"}

\item{\code{new}}{the new label}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-sc_recoder-label_measure"></a>}}
\if{latex}{\out{\hypertarget{method-sc_recoder-label_measure}{}}}
\subsection{Method \code{label_measure()}}{
Change the label of a measure variable
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{sc_recoder$label_measure(measure, language, new)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{measure}}{a measure code}

\item{\code{language}}{a language "de" or "en"}

\item{\code{new}}{the new label}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-sc_recoder-level"></a>}}
\if{latex}{\out{\hypertarget{method-sc_recoder-level}{}}}
\subsection{Method \code{level()}}{
Change the labels of a level
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{sc_recoder$level(field, level, language, new)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{field}}{a field code}

\item{\code{level}}{a level code for the field}

\item{\code{language}}{a language "de" or "en"}

\item{\code{new}}{the new label for the level}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-sc_recoder-total_codes"></a>}}
\if{latex}{\out{\hypertarget{method-sc_recoder-total_codes}{}}}
\subsection{Method \code{total_codes()}}{
Change the total code for a field
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{sc_recoder$total_codes(field, new)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{field}}{a field code}

\item{\code{new}}{a level code for the field or \code{NA}. Will be used as the
new total code. In case of \code{NA}, the total code will be unset.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-sc_recoder-visible"></a>}}
\if{latex}{\out{\hypertarget{method-sc_recoder-visible}{}}}
\subsection{Method \code{visible()}}{
set the visibility of a level. Invisible levels are
omitted in the output of \verb{$tabulate()} but don't affect aggregation
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{sc_recoder$visible(field, level, new)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{field}}{a field code}

\item{\code{level}}{a level code for the field}

\item{\code{new}}{visibility. \code{TRUE} or \code{FALSE}}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-sc_recoder-order"></a>}}
\if{latex}{\out{\hypertarget{method-sc_recoder-order}{}}}
\subsection{Method \code{order()}}{
set the order of levels.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{sc_recoder$order(field, new)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{field}}{a field code}

\item{\code{new}}{the new order. A permutation of all level codes for the field.
alternatively, an integer vector that defines the permutation.}
}
\if{html}{\out{</div>}}
}
}
}
