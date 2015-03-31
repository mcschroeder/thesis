\usepackage[T1]{fontenc}
\usepackage[yyyymmdd,hhmmss]{datetime}
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{mathtools}
\usepackage{xcolor}
\usepackage{fancyvrb}
\usepackage{array}
\usepackage{adjustbox}
\usepackage[justification=centering]{caption}
\usepackage{siunitx}

\usepackage{fontspec}
\setmonofont
  [ BoldFont       = DejaVuSansMono-Bold.ttf,
    ItalicFont     = DejaVuSansMono-Oblique.ttf,
    BoldItalicFont = DejaVuSansMono-BoldOblique.ttf,
    Scale          = MatchLowercase,
  ]
  {DejaVuSansMono.ttf}

%\setmonofont
%  [ BoldFont       = UbuntuMono-Bold,
%    ItalicFont     = UbuntuMono-Italic,
%    BoldItalicFont = UbuntuMono-BoldItalic,
%    Scale          = MatchLowercase,
%  ]
%  {UbuntuMono-Regular}

%---------------------------------------------------
% header & footer
%---------------------------------------------------
\usepackage{fancyhdr}
\pagestyle{fancy}

\setlength{\headheight}{15pt} 

\renewcommand{\chaptermark}[1]{
  \ifnum\value{chapter}>0
    \markboth{\thechapter.\ #1}{}
  \else
    \markboth{#1}{}
  \fi
}

\fancyhead{}
\fancyhead[ER]{\textsl{\nouppercase{\leftmark}}}
\fancyhead[OL]{\textsl{\nouppercase{\rightmark}}}

\fancyhead[LE,RO]{\thepage}

\fancyfoot{}

% TODO remove draft notice
\def\draftnotice{\tiny\emph{[draft~\today~\currenttime]}}
\fancyfoot[C]{\draftnotice}
\fancypagestyle{plain}{
\fancyhf{}
\fancyfoot[C]{\thepage\\\draftnotice}
}

\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{0pt}

\newcommand{\clearemptydoublepage}{\clearpage{\pagestyle{empty}\cleardoublepage}}

\usepackage{emptypage}

%---------------------------------------------------
% biblatex
%---------------------------------------------------
\usepackage[style=authoryear,backref=true,backend=biber]{biblatex}

% for american-style quotes
\usepackage[american]{babel}
\usepackage[english=american]{csquotes}
\renewcommand{\mktextquote}[6]{#1#2#4#5#3#6}
\renewcommand*{\mkcitation}{}

\addbibresource{thesis.bib}
%TODO correct proceedings cite

%---------------------------------------------------
% hyperref
%---------------------------------------------------
\usepackage{hyperref}
\usepackage[noabbrev]{cleveref}

\hypersetup{colorlinks=true}
\hypersetup{linktocpage=true}

%TODO \hypersetup{pdftitle={}}
\hypersetup{pdfauthor={Michael Schröder}}
\hypersetup{pdfkeywords={functional programming, transactional memory, haskell, stm}}
\hypersetup{pdfsubject={Software Engineering master's thesis}}

%---------------------------------------------------
% haskell
%---------------------------------------------------
%include thesis.fmt

%---------------------------------------------------
% Operational semantics of STM
%---------------------------------------------------
\usepackage[inference]{semantic}
\newcommand{\term}[1]{\texttt{#1}}
\newcommand{\svert}{\enskip\vert\enskip}
\newcommand{\ctxP}[1]{\mathbb{P}[#1]}
\newcommand{\ctxE}[1]{\mathbb{E}[#1]}
\newcommand{\ctxS}[1]{\mathbb{S}[#1]}
\newcommand{\rul}[1]{\textit{(#1)}}

%---------------------------------------------------
% GHC runtime system identifiers
%---------------------------------------------------
\newcommand{\rts}[1]{\texttt{#1}}

%---------------------------------------------------
% Colors
%---------------------------------------------------
%TODO: unified color scheme
\newcommand{\colorA}[1]{{\color{blue}#1}}
\newcommand{\colorB}[1]{{\color{magenta}#1}}

\newcommand{\red}[1]{{\color[HTML]{E41A1C}#1}}
\newcommand{\blue}[1]{{\color[HTML]{377EB8}#1}}
\newcommand{\green}[1]{{\color[HTML]{4DAF4A}#1}}

%---------------------------------------------------
% Convenience macro for framed minipages
%---------------------------------------------------
\newcommand{\ffbox}[1]{%
\noindent\fbox{\hspace{-1em}\begin{minipage}{\textwidth}#1\end{minipage}}
}

%---------------------------------------------------
% Hackage packages
%---------------------------------------------------
%TODO: optional version
\newcommand{\package}[1]{\href{http://hackage.haskell.org/package/#1}{\texttt{#1}}}
