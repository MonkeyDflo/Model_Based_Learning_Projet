# you can execute this file or execute this command in the R console
# testthat::test_dir("./tests")

source("./../organize_dataset.R", chdir = TRUE)
library(testthat)

# test convertCateVectToBinMatrix ####
testDatasetLen = sample(iris[,5], 10)
test_that("1)convertCateVectToBinMatrix test output length ? ", {
  expect_length(convertCateVectToBinMatrix(testDatasetLen), 30)
})
testVectLen = c('C','V','B')
test_that("2)convertCateVectToBinMatrix test output length ?", {
  expect_length(convertCateVectToBinMatrix(testVectLen), 9)
})
expectedOutputId = diag(3)
test_that("3)convertCateVectToBinMatrix test expected output ?", {
  expect_identical(convertCateVectToBinMatrix(testVectLen), expectedOutputId)
})


# test binarizeCateMatrix ####
testVect = c('C','V','B')
expectedOutputVect = diag(3)
test_that("4)binarizeCateMatrix test one col matrix ?  ", {
  expect_identical(binarizeCateMatrix(testVect), expectedOutputVect)
})

directions = c("North", "East", "South", "South", "East")
eyesColor= c("B", "B", "B", "G", "G")
testDataSetMat = cbind(directions, eyesColor)
expectedOutputMat = cbind(c(1,0,0,0,0),c(0,1,0,0,1),c(0,0,1,1,0),c(1,1,1,0,0),c(0,0,0,1,1))
test_that("5)binarizeCateMatrix test many cols matrix ?  ", {
  expect_identical(binarizeCateMatrix(testDataSetMat), expectedOutputMat)
})

test_list = list('C','V','B')
test_that("6)binarizeCateMatrix test list input ?  ", {
  expect_error(binarizeCateMatrix(test_list), 'comparison of these types is not implemented')
})

test_array = array(data = c("North", "East", "South", "South", "East",
                            "B", "B", "B", "G", "G" ), dim=c(5,2))
expectedOutputArray = expectedOutputMat
test_that("7)binarizeCateMatrix test array input ?  ", {
  expect_identical(binarizeCateMatrix(test_array), expectedOutputArray)
})

test_df = data.frame(directions, eyesColor)
expectedOutputdf = expectedOutputMat
test_that("8)binarizeCateMatrix test df input ?  ", {
  expect_identical(binarizeCateMatrix(test_df), expectedOutputdf)
})

# test splitDatasetIntoCatAndConti ####
expectedOutputSplit = list(continuousMat = data.frame(iris[,1:4]), 
                           categoricalMat = data.frame(iris[,5]) )
test_that("9)splitDatasetIntoCatAndConti test continousMat and categoricalMat ?  ", {
  expect_equivalent(splitDatasetIntoCatAndConti(iris[,1:5]), expectedOutputSplit)
})

charEyesColor= c("B", "B", "B", "G", "G")
directions = c("North", "East", "South", "South", "East")
factVect = factor(directions, levels= c("North", "East", "South", "West"))
contiVect = sample(iris[,1], 5)
testDfWithChar = data.frame(charEyesColor,factVect,contiVect)
expectedOutputSplitChar = list(continuousMat = data.frame(contiVect), 
                           categoricalMat = data.frame(factVect, charEyesColor))
test_that("10)splitDatasetIntoCatAndConti test char input ?  ", {
  expect_identical(splitDatasetIntoCatAndConti(testDfWithChar), expectedOutputSplitChar)
})
