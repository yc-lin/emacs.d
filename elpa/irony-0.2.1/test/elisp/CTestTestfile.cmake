# CMake generated Testfile for 
# Source directory: /home/yclin/.emacs.d/elpa/irony-0.2.1/server/test/elisp
# Build directory: /home/yclin/.emacs.d/elpa/irony-0.2.1/test/elisp
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(check-irony-el "/usr/local/bin/emacs" "-batch" "-l" "package" "--eval" "(package-initialize)" "--eval" "(unless (require 'cl-lib nil t) (package-refresh-contents) (package-install 'cl-lib))" "-l" "/home/yclin/.emacs.d/elpa/irony-0.2.1/server/test/elisp/irony.el" "-f" "ert-run-tests-batch-and-exit")
add_test(check-irony-cdb-json-el "/usr/local/bin/emacs" "-batch" "-l" "package" "--eval" "(package-initialize)" "--eval" "(unless (require 'cl-lib nil t) (package-refresh-contents) (package-install 'cl-lib))" "-l" "/home/yclin/.emacs.d/elpa/irony-0.2.1/server/test/elisp/irony-cdb-json.el" "-f" "ert-run-tests-batch-and-exit")
