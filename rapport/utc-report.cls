\ProvidesClass{utc-report}

\LoadClass[a4paper,12pt]{report}

\usepackage[T1]{fontenc} % French accents
\usepackage{graphicx} % Figures
\usepackage[headheight=62pt,bottom=1cm]{geometry} % Margins
\usepackage{float} % Layout stuff
\usepackage{xspace} % Spaces at the end of latex macros
\usepackage{enumitem} %For custom itemize
\geometry{vmargin=3cm, textwidth=16cm} % Margins
\usepackage{fancyhdr} % Header and footer customisation
\pagestyle{fancy} % Header and footer customisation
\usepackage[hidelinks]{hyperref}

\usepackage{ulem} % Color underlining
\usepackage{utc-common/utc_colors} % UTC colors from 'utc-common'

% NOUVEAUX PACKAGES AJOUTÉS
\usepackage{etoolbox} % Pour les conditionnels
\usepackage{regexpatch} % Pour patcher les commandes

\usepackage{csvsimple}
\usepackage{etoolbox}
\usepackage{xstring}

\ProcessOptions\relax

\newcommand{\UV}[1]{\def\theUV{#1}}
\renewcommand{\labelitemi}{\color{utcyellow}$\bullet$} % itemize level 1
\renewcommand{\labelitemii}{\color{utcgray}$\bullet$} % itemize level 2
\renewcommand{\title}[1]{\def\thetitle{#1}}
\newcommand{\subtitle}[1]{\def\thesubtitle{#1}} % NOUVELLE COMMANDE SUBTITLE
\renewcommand{\author}[1]{\def\theauthor{#1}}
\newcommand{\supervisor}[1]{\def\thesupervisor{#1}}
\newcommand{\displaysupervisor}{%
  \@ifundefined{thesupervisor}{}{%
    \large{\thesupervisor} \\[0.5cm]
  }%
}
\newcommand{\partnerlogofooter}[1]{\def\thepartnerlogofooter{#1}}
\newcommand{\partnerlogotitle}[1]{\def\thepartnerlogotitle{#1}}
\newcommand{\displaypartnerlogofooter}{%
  \@ifundefined{thepartnerlogofooter}{}{%
    \includegraphics[height=0.5cm]{\thepartnerlogofooter}%
  }%
}

\newcommand{\displaypartnerlogotitle}{%
  \@ifundefined{thepartnerlogotitle}{}{%
    \includegraphics[width=5cm]{\thepartnerlogotitle}%
  }%
}

% Header and footer
\renewcommand{\headrulewidth}{0.5pt} % Thickness of the header line 
\renewcommand{\footrulewidth}{1.5pt} % Thickness of the footer line 

% Set the footer size
\setlength{\footskip}{15mm} 

%Changing the color of the footer line
\renewcommand{\footrule}{%
  {\color{utcyellow} \hrule width\headwidth height\footrulewidth \vskip+2mm}
} 

% Header content
\lhead{\textsc{\includegraphics[height=1cm]{utc-common/utc_icon.png} \leftmark }} % Actual section
\rhead{\theUV} % Current UV

% Footer content
\cfoot{\thepage} % Page number
\lfoot{\theauthor} % Autors
\rfoot{%
  \includegraphics[height=0.5cm]{utc-common/utc_icon.png}%
  \hspace{0.5cm}%
  \displaypartnerlogofooter
}

\makeatletter

% Sections underlined in utcyellow
\newcommand\sectionuline{%
  \bgroup\markoverwith{\textcolor{utcyellow}{\rule[-0.5ex]{0.1mm}{0.5mm}}}%
  \ULon%
}

% MODIFICATIONS POUR LES SECTIONS - VERSIONS DYNAMIQUES

% Redéfinition de \section avec gestion des titres longs
\renewcommand\section{\@startsection {section}{1}{\z@}%
  {-3.5ex \@plus -1ex \@minus -.2ex}%
  {2.3ex \@plus .2ex}%
  {\normalfont\LARGE\bfseries\raggedright\sectionuline}% % \raggedright ajouté
}

% Redéfinition de \subsection avec gestion des titres longs
\renewcommand\subsection{\@startsection {subsection}{2}{0em}%
  {-3ex \@plus -0.8ex \@minus -.2ex}%
  {1.8ex \@plus .2ex}%
  {\normalfont\Large\bfseries\raggedright}% % \raggedright ajouté
}

% Redéfinition de \subsubsection avec gestion des titres longs
\renewcommand\subsubsection{\@startsection {subsubsection}{3}{0em}%
  {-2.5ex \@plus -0.6ex \@minus -.2ex}%
  {1.8ex \@plus .2ex}%
  {\normalfont\large\bfseries\color{utcgray}\raggedright}% % \raggedright ajouté
}

% Redéfinition de \paragraph avec gestion des titres longs
\renewcommand\paragraph{\@startsection {paragraph}{4}{3em}%
  {-2ex \@plus -0.4ex \@minus -.2ex}%
  {1.3ex \@plus .2ex}%
  {\normalfont\large\bfseries\color{utcgray_light}\raggedright}% % \raggedright ajouté
}

% Redéfinition de \subparagraph avec gestion des titres longs
\renewcommand\subparagraph{\@startsection {subparagraph}{5}{4em}%
  {-1.5ex \@plus -0.2ex \@minus -.2ex}%
  {.8ex \@plus .2ex}%
  {\normalfont\bfseries\color{utcgray_light}\raggedright}% % \raggedright ajouté
}

% COMMANDES PERSONNALISÉES POUR LES TITRES LONG AVEC SAUTS DE LIGNE MANUELS
\newcommand{\longtitle}[2][]{%
  \ifstrempty{#1}{%
    \textbf{\large #2}%
  }{%
    \textbf{\large #1}%
    \ifstrempty{#2}{}{\\\textbf{\large #2}}%
  }%
}

\newcommand{\verylongtitle}[3][]{%
  \ifstrempty{#1}{%
    \textbf{\large #2}%
  }{%
    \textbf{\large #1}%
  }%
  \ifstrempty{#2}{}{\\\textbf{\large #2}}%
  \ifstrempty{#3}{}{\\\textbf{\large #3}}%
}

% No space after section number
\def\@seccntformat#1{\csname the#1\endcsname.\hspace{0.5em}}

% Title page
\newcommand{\maketitlepage}{%
  \thispagestyle{empty}%
  \setcounter{page}{0}%
  
  \begin{figure}[H]%
    \centering%
    \includegraphics[width=7cm]{utc-common/utc_su_logo.png}%
    \vspace{1cm}%
    \displaypartnerlogotitle%
  \end{figure}%

  \vspace{3cm}%
  
  \begin{center}%
    {\color{utcyellow}\rule{\linewidth}{0.8mm}}%
    \vspace*{0mm}%
    
    \Huge{\textbf{\theUV \\ \thetitle}}%
    
    % AFFICHAGE DU SOUS-TITRE SI DÉFINI
    \ifdefined\thesubtitle%
      \vspace{0.5cm}%
      {\LARGE\textit{\thesubtitle}}%
    \fi%
    
    {\color{utcyellow}\rule{\linewidth}{0.8mm}}%
    \vspace{2cm}%

    \Large{\theauthor} \\[0.5cm]%
    \displaysupervisor%

    \vspace{3cm}%
    \Large{\@date}%
  \end{center}%
  
  \vspace{3cm}%
  \pagebreak%
}

% Custom itemize with UTC yellow
\newif\ifutc@customitemize
\utc@customitemizetrue % Enabled by default

% Option to disable it
\newcommand{\disableutcitemize}{\utc@customitemizefalse}

% Redefine the itemize if enabled
\AtBeginDocument{%
  \ifutc@customitemize%
    \setlist[itemize,1]{label=\raisebox{0.2ex}{\includegraphics[height=1ex]{utc-common/utc_dot.png}}, labelsep=0.6em, left=1.5em}%
    \renewcommand{\labelitemii}{\raisebox{0.15ex}{\includegraphics[height=0.7ex]{utc-common/utc_dot.png}}}%
  \fi%
}
\newcommand{\citeentretien}[1]{%
    \csvreader[
        head to column names,
        filter={\equal{\id}{#1}}
    ]{annexes/citations.csv}{}%
    {%
        \par\noindent
        \begingroup
        \emergencystretch=3em
        \tolerance=2500
        \hyphenpenalty=400
        \begin{minipage}{\linewidth}
            \raggedright
            \emph{«~\texte~»}
            \vspace{0.2em}
            \hfill\scriptsize(Entretien \entretien, \textsc{\participant})
        \end{minipage}
        \endgroup
        \vspace{0.5em}
    }%
    \csvreader[
        head to column names,
        filter={\equal{\id}{#1}}
    ]{annexes/citations.csv}{}%
    {}% Empty fallback
}

\makeatother

