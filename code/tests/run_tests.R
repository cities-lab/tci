library('testthat')

#source('sample.R')

test_dir(dirname(sys.frame(1)$ofile), reporter = 'Summary')