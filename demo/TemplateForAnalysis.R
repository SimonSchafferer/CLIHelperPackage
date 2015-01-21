###################################
#   Template analysis
#
#   This script will produce all commands necessary to run a combined tophat/bowtie mapping and counting tables with ht-seq and multibamCov
###################################
#Optionally: extracting a sample fastq subset! sampleFastq.fastq (100.000 entries)
library(ShortRead)
sampleInfo = read.table("PATH_TO_SAMPLE_DESCRIPTION_TABLE", 
                        sep="\t", header=TRUE)
setwd("PATH_TO_FASTQ_FILES")
# lapply( sampleInfo$fastq, function(x){
#   sampler <- FastqSampler(x, 100000)
#   set.seed(123)
#   dat = yield(sampler)
#   writeFastq(dat, file.path("/home/simon/PHDStudies/RNA-Seq/IonProton/CavKO_Striessnig/IonProtonRun20140822_Cav13_striatum_striessnig/SampleAnalysis/rawData", x) )
#   return(TRUE)
# } )

############################
# Begin here
############################

rootDir = file.path( path.expand("~"), "PATH_TO_BASE_DIR") #Directory were the analysis starts
rawDataDir = file.path(rootDir,"rawData") #when unchanged -> fastq files should be placed in rawData folder of rootDir
gtfFileName = "Mus_musculus.GRCm38.78_mod.gtf" #GTF File name (modified, since the UCSC genome is employed: Chromosome names in ensembl are missing 'chr', therefore they were added)
gtfFilePath = "/home/simon/dbsOfflineUse/GTF_repos/Mus_musculus.GRCm38.78_repo/" #Path to GTF file
transcriptomeIndex = "/home/simon/dbsOfflineUse/GTF_repos/Mus_musculus.GRCm38.78_repo/Mus_musculus.GRCm38.78" #Path To bowtie Index. This has to be created with tophat command by using following command
# system( paste0("tophat2 --GTF ", file.path(gtfFilePath,gtfFileName), " --transcriptome-index=",file.path(gtfFilePath,"Mus_musculus.GRCm38.78"), bowtieIndexFilePath ) ) 
bowtieIndexFilePath = "/home/simon/dbsOfflineUse/MusMusculus/Mouse_mm10_fasta/bowtie2/mm10"

samplesInfo = read.table( file.path(rootDir, "samplesInfo.csv") ,sep="\t", header=TRUE) #The table containing sample information, no necessary to define at this step
commandLog = file.path(rootDir, "Commands") #log file where all commands are stored
file.create(commandLog)
executionLog = file.path(rootDir, "executionLog") #log file storing the output of the applications
file.create(executionLog)

setwd(rootDir)
library(CLIHelperPackage)
#######################
#   Cutadapt to trim the fastq files
#######################
cutAdaptCLI = Cutadapt_CLI(inFilePath=rawDataDir, cliParams =  c("--minimum-length 18"), outputFlag = "_trimmed", 
                           outFilePath = file.path(rootDir,"rawDataTrimmed") )
cutAdatptCLI_cmdRes = generateCommandResult(object = cutAdaptCLI )
#writing command log
cat( getCommandLog(cutAdatptCLI_cmdRes),  file = commandLog, append = FALSE )


topHat2CLI = Tophat2_CLI(inFilePath=getOutFilePath(getCLIApplication(cutAdatptCLI_cmdRes)),
                         inFileNames = getOutResultName(getOutResultReference(cutAdatptCLI_cmdRes)),
                         cliParams =  c("--num-threads 12","--keep-fasta-order",paste0("--transcriptome-index ",transcriptomeIndex) ), 
                         outputFlag = "_tophatOut", 
                         
                         outFilePath = file.path(rootDir,"tophat_mapping"), 
                         bowtieIndexFilePath = bowtieIndexFilePath)
topHat2_cmdRes = generateCommandResult( object = topHat2CLI )
#writing log
cat( paste0("\n",getCommandLog(topHat2_cmdRes)), file = commandLog, append = TRUE  )

allTopHatOutputs = file.path( getOutFilePath(getCLIApplication(topHat2_cmdRes)), getOutResultName(getOutResultReference( topHat2_cmdRes )) )

bowtie2TophatIonCLI_cmdResL = lapply( allTopHatOutputs, function(x){
  
  bowtie2TophatIonCLI = Bowtie2TophatIon_CLI(inFilePath = x,
                                             outputFlag = "bwt",
                                             bowtieIndexFilePath = bowtieIndexFilePath)
  
  bowtie2TophatIonCLI_cmdRes = generateCommandResult( object = bowtie2TophatIonCLI )
  return( bowtie2TophatIonCLI_cmdRes )
  
} )

for( i in 1:length(bowtie2TophatIonCLI_cmdResL)  ){
  cat( getCommandLog(bowtie2TophatIonCLI_cmdResL[[i]]), file = commandLog, append = TRUE  )
}

######################
#   Samtools commands
######################
samToolsHTSeqCmdL = lapply( bowtie2TophatIonCLI_cmdResL, function(x){
  
  inFN = getOutResultName(getOutResultReference(x))
  currPath = getOutFilePath(getCLIApplication(x))
  ######################
  # Sam Commands For htseq count
  ######################
  sortSam1 = Samtools_CLI(inFilePath=currPath, 
                          inFileNames = inFN,
                          cliParams =  c("-n"), 
                          outputFlag = "_sn", 
                          outFilePath = currPath, 
                          samtoolsApplication = "sort", 
                          outputFormat = "bam")
  sortBamCmdRes1 = generateCommandResult( object = sortSam1 )
  
  inFN = getOutResultName(getOutResultReference(sortBamCmdRes1))
  
  samView = Samtools_CLI(inFilePath=currPath, 
                         inFileNames = inFN,
                         cliParams =  c("-o"), 
                         outputFlag = "", 
                         outFilePath = currPath, 
                         samtoolsApplication = "view" ,
                         outputFormat = "sam")
  samViewCmdRes = generateCommandResult( object = samView )
  
  ######################
  # Sam Commands For multicov bedtools
  ######################
  
  inFN = getOutResultName(getOutResultReference(x))
  
  sortSam2 = Samtools_CLI(inFilePath=currPath, 
                          inFileNames = inFN,
                          cliParams =  c(""), 
                          outputFlag = "_s", 
                          outFilePath = currPath, 
                          samtoolsApplication = "sort", 
                          outputFormat = "bam")
  sortBamCmdRes2 = generateCommandResult( object = sortSam2 )
  
  
  inFN = getOutResultName(getOutResultReference(sortBamCmdRes2))
  
  samIndex = Samtools_CLI(inFilePath=currPath, 
                          inFileNames = inFN,
                          cliParams =  c(""), 
                          outputFlag = "", 
                          outFilePath = currPath, 
                          samtoolsApplication = "index", 
                          outputFormat = "bai")
  samIndexCmdRes = generateCommandResult( object = samIndex )
  
  return( list(sortBamCmdRes1, samViewCmdRes, sortBamCmdRes2, samIndexCmdRes) )
  
} )

for( i in 1:length(samToolsHTSeqCmdL)  ){
  for( j in 1:length(samToolsHTSeqCmdL[[i]])){
    cat(getCommandLog(samToolsHTSeqCmdL[[i]][[j]]), file = commandLog, append = TRUE  )
  }
}


######################
# HTSeq Count (in case of ncRNA analysis may be not required!)
######################
hTSeqCount_CLI_cmdResL = lapply( samToolsHTSeqCmdL, function(x){
  
  inFN = getOutResultName(getOutResultReference(x[[2]]))
  currPath = getOutFilePath(getCLIApplication(x[[2]]))
  
  hTSeqCount_CLI = HTSeqCount_CLI(inFilePath = currPath, 
                                  inFileNames = inFN,
                                  cliParams =  c("-s","no","-a 10"), 
                                  outputFlag = "_htscount", 
                                  outFilePath = currPath,
                                  gffFile = file.path( gtfFilePath, gtfFileName) )
  
  hTSeqCount_CLI_cmdRes = generateCommandResult( object = hTSeqCount_CLI )
  return( hTSeqCount_CLI_cmdRes )
} )

for( i in 1:length(hTSeqCount_CLI_cmdResL)  ){
  cat( getCommandLog(hTSeqCount_CLI_cmdResL[[i]]), file = commandLog, append = TRUE  )
}


######################
# MultiBamCoverageCount!
######################

#Definition of the output directories from tophat first, so they can be iterrated
outDir = file.path(rootDir,"coverageMatrix")
dir.create(outDir)

inFN = file.path( getOutFilePath(topHat2CLI), 
                  list.files( getOutFilePath(getCLIApplication(topHat2_cmdRes))),
                  getOutResultName(getOutResultReference(samToolsHTSeqCmdL[[1]][[3]])))


multiBamCov_CLI = MultiBamCov_CLI(inFilePath = "", 
                                  inFileNames = inFN,
                                  cliParams =  c("-s","-D"), 
                                  outputFlag = "_multibamcov", 
                                  outFilePath = outDir,
                                  annotationFileMB = file.path( gtfFilePath, gtfFileName), 
                                  annotationType = "bed")

multiBamCov_CLI_cmdRes = generateCommandResult( object = multiBamCov_CLI )
cat(getCommandLog(multiBamCov_CLI_cmdRes), file=commandLog, append=TRUE)

#######################################################
#       NOW RUN ALL COMMANDS!
#######################################################
# system(paste0("bash ",commandLog, " >& ", executionLog))


