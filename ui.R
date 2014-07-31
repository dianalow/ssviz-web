library(shiny)

shinyUI(navbarPage("ssviz",        
tabPanel("Welcome",
HTML(
"
<h1>ssviz</h1>
<p>Small RNA sequencing enables the discovery and profiling of
microRNAs, piRNAs and other non-coding RNA for any organism, even without prior genome annotation. 
This site runs the R Shiny implementation of the <a href=\"http://master.bioconductor.org/packages/devel/bioc/html/ssviz.html\" target=\"_blank\">ssviz R package</a>, which is intended primarily as a 
visual aid, and in the future provide more specialized analysis of small RNAs.
ssviz is also available for installation via <a href=\"http://master.bioconductor.org/packages/devel/bioc/html/ssviz.html\" target=\"_blank\">Bioconductor</a>.
</p>
<br>
The ssviz package together with this website's Shiny implementation is written by <a href=\"http://dianalow.github.io\">Diana Low</a>.
<ul><li>Low D (2014). ssviz: A small RNA-seq visualizer and analysis toolkit. R package version 0.99.2</li></ul><br>
<p>
Running from your local R installation?
<pre>
require(shiny)
runGitHub(\"ssviz-web\",username=\"dianalow\")
</pre>
</p>
<hr>
References<br>
<ol>
<li>FASTX-Toolkit: FASTQ/A short-reads pre-processing tools</li>
<li>Ruvkun, G. Molecular Biology: Glimpses of a Tiny RNA World. Science 2001, 294, 797-799.</li>
<li>Brennecke J, Aravin AA, Stark A, Dus M, Kellis M, Sachidanandam R, Hannon GJ. Discrete small
RNA-generating loci as master regulators of transposon activity in Drosophila. Cell. 2007;128:1089-
1103.</li>
<li>Thomson T, Lin H. The biogenesis and function of PIWI proteins and piRNAs: progress and
prospect. Annu. Rev. Cell. Dev. Biol. 2009;25:355-376.</li>
<li>Shinpei Kawaoka, Yuji Arai, Koji Kadota, et al. Zygotic amplication of secondary piRNAs during
silkworm embryogenesis RNA (2011), 17:00-00</li>
</ol>
<br>
<h4>Contact</h4>
Diana Low
<a href=\"mailto:dlow@imcb.a-star.edu.sg\">dlow [at] imcb.a-star.edu.sg</a><br>
<a href=\"http://eglab.org\" target=\"_blank\">Guccione Lab</a><br>
Institute of Molecular and Cell Biology (IMCB), Singapore
<hr>

Built using <a href=\"http://shiny.rstudio.com\" target=\"_blank\">Shiny</a><br>
")
),

tabPanel("Start",
HTML("<h2>Upload and view bam files</h2>
<p>To start analyzing your mapped sequences:
<ol>
<li>Upload your bam files (or use the samples)</li>
<li>Load the bam file</li>
</ol></p>
")                                  
           ,
           sidebarLayout(
             sidebarPanel(
               HTML("<p align=\'justify\'>As small RNA reads are often repeated, it is thus favourable to
collapse identical sequences into a single read (but keeping note of the total number of reads) to prevent 
mapping the same read multiple times. <a href=\"http://hannonlab.cshl.edu/fastx_toolkit/\">Fastx-toolkit</a> is a software that can do this automatically, and collapsed 
read names are in the form of readname-readcount. After the bam file has been uploaded, this function will automatically retrieve the counts (if any) from the readname (effectively splitting them and create a 
new column \"count\". If it has not been collapsed, counts will be set to 1.</p>"),
               fileInput('file1', label='Bam file#1',accept=c('bam', '.bam')),
               fileInput('file2', label='Bam file#2',accept=c('bam', '.bam')),
               actionButton("goSampleFiles", "Use sample files")
               
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               HTML("<h4>Current bam files in system:</h4>"),
               htmlOutput("filenames"),
               HTML("<br><br>"),
               actionButton("goLoadBam1", "Load Bam file #1"),
               htmlOutput("bamloaded1"),
               actionButton("goLoadBam2", "Load Bam file #2"),
               htmlOutput("bamloaded2"),
               HTML("<br><br>"),
               tabsetPanel(
                 tabPanel("Bam #1", verbatimTextOutput("read1")), 
                 tabPanel("Bam #2", verbatimTextOutput("read2"))
               )
             )
           )
           ),
                  
tabPanel("Distribution",
HTML("<h2>Plot bam file properties (distribution)</h2>
<p>ssviz has a few plot functions - the most basic is to plot the general distribution in the bam le based
on their properties. For small RNA sequencing in particular, it is important to know the lengths of the
reads (representing the lengths of the small RNA), direction (strand in sequencing) and perhaps region
(eg. chromosome or RNA element). For example, microRNAs are typically 18-24nt in length, whereas
piRNAs are longer, around 24-32 nt in length (Ruvkun 2001, Brennecke 2007, Thomson 2009).</p>
<p>When comparing two or more datasets, it is crucial that the datasets are normalized properly, and
this includes having information on (i) total number of counts for the same reads, and (ii) the overall
number of reads that was mapped, to make sure than the comparisons are on the same scale. Part
(i) can be obtained directly from the loaded bam DataFrame (if fastx-toolkit was used). Part (ii)
can be obtained as an output of the mapper, eg. bowtie. Typically this number would represent total
number of reads sequenced, or mappable to the broadest encompassing index (the genome).
In the bam file, read length is represented by qwidth, direction by strand and region by rname
and pos.</p>
<p>
<h3>Notes on normalization</h3>
<ol>
<li>If you have previously used fastx-toolkit or another tool that collapses reads to a form of 
\"readname-readcount\", please tick the \"Bam file contains read count information\" box and ssviz will extract this information.
If not, each read will be given a pseudo count of 1.</li>
<li>If total counts are not given, this function will normalize against the number of reads in the bam file (taking into
account point #1). This would make sense in most cases, unless your bam file contains a subset of reads (eg. only 1 type 
of small RNAs). In this case, you should normalize over the total mappable reads of your dataset.
</li>
</ol>
</p><br><br>"
),
sidebarLayout(
  
  sidebarPanel(uiOutput("filedropdown_distro"),
               checkboxInput("plotdistro_readcount",label="Bam file contains read count information?",value=TRUE),
               #checkboxInput("plotdistro_unique",label="unique counts only",value=FALSE),
               HTML("<br>"),
               selectInput("plotdistro_type", label = "Plot type", 
                           choices = list("qwidth (read length)" = "qwidth", "strand (read direction)" = "strand",
                                          "rname (mapped entity)" = "rname"), selected = "qwidth"),
               textInput("plotdistro_ncount", label = "Total counts (comma separated)"),
               textInput("plotdistro_samplenames",label="Sample names (comma separated)"),
               HTML("<br>"),
               actionButton("goPlotDistro", "Plot distribution")),
  mainPanel(plotOutput("plotdistro")))

),
                  
tabPanel("Region Density",
HTML("<h2>Plot region density</h2>
     <p>
<h3>Notes on normalization</h3>
<ol>
<li>If you have previously used fastx-toolkit or another tool that collapses reads to a form of 
\"readname-readcount\", please tick the \"Bam file contains read count information\" box and ssviz will extract this information.
If not, each read will be given a pseudo count of 1.</li>
<li>If total counts are not given, this function will normalize against the number of reads in the bam file (taking into
account point #1). This would make sense in most cases, unless your bam file contains a subset of reads (eg. only 1 type 
of small RNAs). In this case, you should normalize over the total mappable reads of your dataset.
</li>
</ol>
</p><br><br>"),
sidebarLayout(
  
  sidebarPanel(uiOutput("filedropdown_region"),
               checkboxInput("plotregion_readcount",label="Bam file contains read count information?",value=TRUE),
               HTML("<br>"),
               textInput("plotregion_ncount", label = "Total counts (comma separated if 2 datasets)"),
               textInput("plotregion_samplenames",label="Sample names (comma separated if 2 datasets)"),
               textInput("plotregion_region",label="Region (chr:start-stop)",value='chr1:3015526-3080526'),
               #checkboxInput("plotregion_unique",label="unique counts only",value=FALSE),
               HTML("<br>"),
               actionButton("goPlotRegion", "Plot region density")),
  mainPanel(plotOutput("plotregion")
            )),div(class = "busy",
                   p("Calculation in progress.."),
                   img(src="ajaxloaderq.gif")
            )
),
                 
tabPanel("Nucleotide Frequency",

HTML("<h2>Nucleotide Frequency</h2>
A known property of primary piRNA is a marked 5-prime uridine bias (Brennecke 2007). The ntfreq
function allows the user to compute the frequency of nucleotides over a chosen length.<br><br>"),
sidebarLayout(
  sidebarPanel(
    uiOutput("filedropdown_ntfreq"),
    checkboxInput("ntfreq_readcount",label="Bam file contains read count information?",value=TRUE),
    HTML("<br>"),
               sliderInput("ntfreq_length", label = "Nucleotide length from 5' end",
                           min = 5, max = 10, value = 5),
               HTML("<br>"),
               actionButton("gontfreq","Compute and plot!")),
  mainPanel(verbatimTextOutput("textntfreq"),
            plotOutput("plotntfreq"))),
tagList(
  tags$head(
    tags$link(rel="stylesheet", type="text/css",href="style.css"),
    tags$script(type="text/javascript", src = "busy.js")
  )
),
div(class = "busy",
    p("Calculation in progress.."),
    img(src="ajaxloaderq.gif")
)
),
                  
tabPanel("Ping pong analysis",
HTML("<h2>Ping pong analysis</h2>
<p>PIWI-interacting RNAs (piRNAs) are 23-30-nucleotide-long small RNAs that act as sequence-specic
silencers of transposable elements in animal gonads (Kawaoka 2011). The ping-pong mechanism is
a proposed method for the amplication of primary Piwi-associated RNAs (piRNAs), which leads to
the production of new primary piRNAs from their precursor transcripts, which eventually amplies
the pool of both primary and secondary piRNAs (Brennecke 2007). This positive feedback loop is
a secondary biogenesis mechanism that requires complementary transcripts to a pre-existing pool of
piRNAs.</p>
<p>
Piwi proteins retain the endoribonuclease or Slicer activity that allows them to cleave targets
between position 10 and position 11 of their bound piRNA. This cleavage denes the 5' end of a
secondary piRNA that is generated from the transposon transcript. Because a very high proportion
of piRNAs have a uridine (U) at the rst position and because the complementarity between piRNAs
and targets is expected to be nearly perfect, secondary piRNAs typically have adenosines at position
10, which base-pairs with the U at the rst position of the piRNA. This is reected as a sharp peak at
10nts when frequency is plotted against overlap length, and also can be seen in the nucleotide frequency
plot in the next section.</p>
<p>
To compute the overlaps between the sense and anti-sense (amplified) piRNAs, we leverage on the
positional information contained in the bam file of \"+\" strand reads and \"-\" strand reads, calculates
and plots the frequency of overlap (up to the length of the read).</p>
"),
sidebarLayout(
  sidebarPanel(uiOutput("filedropdown_pingpong"),
               checkboxInput("plotregion_readcount",label="Bam file contains read count information?",value=TRUE),
               HTML("<br>"),
               actionButton("gopingpong","Compute pingpong frequencies!"),
               HTML("<br><br><br>"),
               uiOutput("objectdropdown_pingpong"),
               actionButton("plotpingpong","Plot ping-pong!")),
  mainPanel(verbatimTextOutput("textpp"),
            plotOutput("plotpp"),
            tagList(
              tags$head(
                tags$link(rel="stylesheet", type="text/css",href="style.css"),
                tags$script(type="text/javascript", src = "busy.js")
              )
            ),
            div(class = "busy",
                p("Calculation in progress.."),
                img(src="ajaxloaderq.gif")
            ))
)),

navbarMenu("Help",
tabPanel("Example plots",
         HTML("<h3>Distribution plot with single dataset</h3><img src=\"plotdistro1.png\">
              <h3>Distribution plot with two datasets</h3><img src=\"plotdistro2.png\">
              <h3>Region density</h3><img src=\"plotregion.png\">
              <h3>Nucleotide frequency</h3><img src=\"plotfreq.png\">
              <h3>Ping-pong analysis</h3><img src=\"pingpong.png\">")
         ),
tabPanel("Troubleshooting input file")
)))
