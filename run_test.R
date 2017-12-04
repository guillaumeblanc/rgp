library('RUnit')

# Defines a test suite for all tests in test folder
test.suite <- defineTestSuite("all",
                              dirs = file.path("tests"),
                              testFileRegexp = '.+_tests.R')

# Run suite
test.result <- runTestSuite(test.suite)
printTextProtocol(test.result)

