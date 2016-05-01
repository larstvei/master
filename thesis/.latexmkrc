# .latexmkrc

$ENV{'TEXINPUTS'}='./tex-files/ifimaster//:' . $ENV{'TEXINPUTS'};

$pdf_previewer="emacsclient -e '(find-file %S)'";
$pdflatex='pdflatex %O -interaction=nonstopmode %S';
$pdf_update_method = 4;
$pdf_update_command = "emacsclient -e '(with-current-buffer (find-buffer-visiting %S) (pdf-view-revert-buffer nil t))'";
