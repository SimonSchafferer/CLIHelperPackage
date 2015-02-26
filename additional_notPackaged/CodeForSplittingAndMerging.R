
pathToSplittedFiles = file.path(path.expand("~"),"Rworkspace/CLIHelperPackage/additional_notPackaged/splitted")
pathToSingleFile = file.path(path.expand("~"),"Rworkspace/CLIHelperPackage/R/")

test = list.files( )
names(test) = test
ORDER = c("CLIApplication.R", "OutResultReference.R", "FilesOutput.R", 
  "FoldersOutput.R", "CmdGenResult.R", "CmdGenResultExec.R", "Cutadapt_CLI.R", 
  "HTSeqCount_CLI.R", "IntersectBed_CLI.R","MergeBedFile_CLI.R", 
  "MultiBamCov_CLI.R", "Samtools_CLI.R","BamToBed_CLI.R","SortFiles_CLI.R", 
  "Bowtie2_CLI.R", "Bowtie2TophatIon_CLI.R", "Tophat2_CLI.R","Bowtie_CLI.R", "MultiIntersectBed_CLI.R", "MultiIntersectBed_perl_CLI.R", "RNAStar_CLI.R")

test[ORDER]

ClassSplitMergeHelper::mergeClassFiles(inPath = pathToSplittedFiles, filename=test[ORDER], outPath = pathToSingleFile, outFilename = "singleRCodeFile.R" )

