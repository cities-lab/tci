library('testthat')

test_dir(dirname(sys.frame(1)$ofile), reporter = 'Summary')