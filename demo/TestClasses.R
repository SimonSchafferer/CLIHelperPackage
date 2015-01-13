cutAdaptCLI = Cutadapt_CLI(inFilePath="/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/rawData", 
             cliParams =  c("--minimum-length 20"), 
             outputFlag = "_trimmed", 
             outFilePath = file.path("/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/rawDataTrimmed")
             )
cutAdatptCLI_cmdRes = generateCommandResult( object = cutAdaptCLI )
# executeCommandResult(cutAdatptCLI_cmdRes)

topHat2CLI = Tophat2_CLI(inFilePath="/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/rawDataTrimmed", 
                           cliParams =  c("--num-threads 12","--keep-fasta-order"), 
                           outputFlag = "_tophatOut", 
                           outFilePath = file.path("/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/tophat-out_sampleAnalysis"), 
                         gtfFilePath = "/home/simon/dbsOfflineUse/GTF_repos/Mus_musculus.GRCm38.78_mod.gtf", 
                         bowtieIndexFilePath = "/home/simon/dbsOfflineUse/MusMusculus/Mouse_mm10_fasta/bowtie2/mm10")

topHat2_cmdRes = generateCommandResult( object = topHat2CLI )
# executeCommandResult(topHat2_cmdRes)

bowtie2CLI = Bowtie2_CLI(inFilePath="/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/rawDataTrimmed", 
                         cliParams =  c("--very-fast","-N 1"), 
                         outputFlag = "_bowtieOut", 
                         outFilePath = file.path("/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/bowtie-out_sampleAnalysis"), 
                         bowtieIndexFilePath = "/home/simon/dbsOfflineUse/MusMusculus/Mouse_mm10_fasta/bowtie2/mm10")

bowtie2CLI_mp = Bowtie2_CLI(inFilePath="/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/rawDataTrimmed", 
                         cliParams =  c("--very-fast","-N 1"), 
                         outputFlag = "_bowtieOut", 
                         outFilePath = file.path("/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/bowtie-out_sampleAnalysis"), 
                         bowtieIndexFilePath = "/home/simon/dbsOfflineUse/MusMusculus/Mouse_mm10_fasta/bowtie2/mm10", 
                         matepairFileNames = list.files("/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/rawDataTrimmed"))

bowtie2_cmdRes = generateCommandResult( object = bowtie2CLI )
bowtie2CLI_mp_cmdRes = generateCommandResult( object = bowtie2CLI_mp )


bowtie2TophatIonCLI = Bowtie2TophatIon_CLI(inFilePath = "/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/mapping/tophat_out_001_msawt_20141215", 
                     outputFlag = "bwt", 
                     bowtieIndexFilePath = "/home/simon/dbsOfflineUse/MusMusculus/Mouse_mm10_fasta/bowtie2/mm10")
bowtie2TophatIonCLI_cmdRes = generateCommandResult( object = bowtie2TophatIonCLI )

###################################
#   Samtools test classes (only templating ... maybe leave them out of package...)
###################################

#TESTING
testSort = Samtools_CLI(inFilePath="/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/rawData", 
                        inFileNames = c("aligned.bam"),
                        cliParams =  c("-n"), 
                        outputFlag = "_sn", 
                        outFilePath = file.path("/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/rawDataTrimmed"), 
                        samtoolsApplication = "sort", 
                        outputFormat = "bam")
generateCommandResult(testSort)@commands
testSort = Samtools_CLI(inFilePath="/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/rawData", 
                        inFileNames = c("aligned.bam"),
                        cliParams =  c(""), 
                        outputFlag = "_s", 
                        outFilePath = file.path("/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/rawDataTrimmed"), 
                        samtoolsApplication = "sort", 
                        outputFormat = "bam")
generateCommandResult(testSort)@commands


testView = Samtools_CLI(inFilePath="/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/rawData", 
                        inFileNames = c("aligned_sn.bam"),
                        cliParams =  c("-o"), 
                        outputFlag = "", 
                        outFilePath = file.path("/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/rawDataTrimmed"), 
                        samtoolsApplication = "view" ,
                        outputFormat = "sam")
generateCommandResult(testView)


testIndex = Samtools_CLI(inFilePath="/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/rawData", 
                         inFileNames = c("aligned_s.bam"),
                         cliParams =  c(""), 
                         outputFlag = "", 
                         outFilePath = file.path("/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/rawDataTrimmed"), 
                         samtoolsApplication = "index", 
                         outputFormat = "bai")
generateCommandResult(testIndex)


#######################################
#     Testing HTSeqCount
#######################################

testIndex = HTSeqCount_CLI(inFilePath="/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/rawData", 
                         inFileNames = c("aligned_sn.sam"),
                         cliParams =  c("-s","no","-a 10"), 
                         outputFlag = "_htscount", 
                         outFilePath = file.path("/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/rawDataTrimmed"), 
                         gffFile = "/home/simon/dbsOfflineUse/MusMusculus/Mus_musculus.GRCm38.78_mod.gtf")
generateCommandResult(testIndex)

#######################################
#     Testing MultiBamCoverage
#######################################

testIndex = MultiBamCov_CLI(inFilePath="/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/rawData", 
                         inFileNames = c("aligned_s.bam", "aligned_s.bam"),
                         cliParams =  c("-s","-D"), 
                         outputFlag = "_multibamcov", 
                         outFilePath = file.path("/home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/rawDataTrimmed"), 
                         annotationFileMB = "/home/simon/dbsOfflineUse/MusMusculus/Mus_musculus.GRCm38.78_mod.gtf", 
                         annotationType = "bed")
generateCommandResult(testIndex)



