
test = list.files( "/home/simon/RWorkspace/CLIHelperPackage/demo/splitted")
names(test) = test
ORDER = c("CLIApplication.R", "OutResultReference.R", "FilesOutput.R", 
  "FoldersOutput.R", "CmdGenResult.R", "CmdGenResultExec.R", "Cutadapt_CLI.R", 
  "HTSeqCount_CLI.R", "IntersectBed_CLI.R","MergeBedFile_CLI.R", 
  "MultiBamCov_CLI.R", "Samtools_CLI.R","BamToBed_CLI.R","SortFiles_CLI.R", 
  "Bowtie2_CLI.R", "Bowtie2TophatIon_CLI.R", "Tophat2_CLI.R","Bowtie_CLI.R")

test[ORDER]

mergeClassFiles(inPath = "/home/simon/RWorkspace/CLIHelperPackage/demo/splitted", filename=test[ORDER], outPath = "/home/simon/RWorkspace/CLIHelperPackage/R/", outFilename = "singleRCodeFile.R" )

