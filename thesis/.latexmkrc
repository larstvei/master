# .latexmkrc

$jobname = 'tex-files/master';

$ENV{'TEXINPUTS'}='./tex-files/ifimaster//:' . $ENV{'TEXINPUTS'};

$pdf_previewer = "open -a /Applications/Skim.app";
