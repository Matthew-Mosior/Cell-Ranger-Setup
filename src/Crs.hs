{-=Cell Ranger Setup (CRS): A Haskell-based solution to=-}
{-=setting up cellranger count or vdj runs based on a=-}
{-=configuration YAML.=-}
{-=Author: Matthew Mosior=-}
{-=Version: 1.0=-}
{-=Synopsis:  This Haskell Script will take in a=-}
{-=configuration YAML and will set up and launch=-}
{-=Cellranger count and/or vdj runs via LSF bsub=-}
{-=accordingly.=-}


{-Lanuguage Extension.-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}

{----------------------}


{-Import modules.-}

import YamlParser

{----------------}


{-Imports-}

import Control.Arrow as CA
import Control.Monad as CM
import Data.Aeson as DAeson
import Data.ByteString.Char8 as DBC
import Data.CSV as DCSV
import Data.Either as DE
import Data.List as DL
import Data.List.Split as DLS
import Data.Map as DMap
import Data.Maybe as DMaybe
import Data.Text as DText
import Data.Time as DTime
import Data.Time.Clock.POSIX as DTCP
import Data.Tuple as DTuple
import Data.Yaml as DYaml
import System.Console.GetOpt as SCG
import System.Directory as SD
import System.Environment as SE
import System.Exit as SX
import System.IO as SIO
import System.IO.Temp as SIOT
import System.Process as SP
import Text.ParserCombinators.Parsec as TPCP
import Text.Read as TRead
import Text.Regex as TR
import Text.Regex.TDFA as TRP

{---------}


{-Custom CML Option Datatype.-}

data Flag
    = Help -- --help
    deriving (Eq,Ord,Show)

{-----------------------------}


{-Option Description function relating to datatype above.-}

--options -> This function will
--describe flags.
options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"] (NoArg Help) "Print this help message.\n"
    ] 

{---------------------------------------------------------}


{-Function to correctly parse the flags.-}

--compilerOpts -> This function will
--parse incoming command line arguments.
compilerOpts :: [String] -> IO ([Flag],[String])
compilerOpts argv =
    case getOpt Permute options argv of
        (args,files,[]) ->
            if | DL.elem Help args ->
               do SIO.hPutStrLn stderr (greeting ++ SCG.usageInfo header options)
                  SX.exitWith SX.ExitSuccess
               | otherwise -> return (DL.nub args,files) 
        (_,_,errors) -> do
            SIO.hPutStrLn stderr (DL.concat errors ++ SCG.usageInfo header options)
            SX.exitWith (SX.ExitFailure 1)
        where 
            greeting         = "Cell Ranger Setup, Copyright (c) 2021 Matthew Mosior.\n"
            header           = "Usage: CRS [-h] [Configuration YAML]\n\
                               \Cell Ranger Setup (CRS), Version 1.0.\n\
                               \Please see https://github.com/Matthew-Mosior/Cell-Ranger-Setup/wiki for more information.\n"

{----------------------------------------}


{-Create run directory.-}

--createRunDirectory -> This function will
--create the requisite run directory.
createRunDirectory :: CRSConfig -> IO ()
createRunDirectory opts = do
    !currenttandd <- DTime.getZonedTime
    SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " ++ "Creating run directory ...")
    (_,_,_,ph) <- SP.createProcess (SP.proc "mkdir" [extractRunDirectory opts])
    ec <- SP.waitForProcess ph
    case ec of
        SX.ExitFailure _ -> do !currenttandd <- DTime.getZonedTime
                               error ("[" ++ (show currenttandd) ++ "] " ++ "Could not create " ++ (extractRunDirectory opts) ++ ".")
        SX.ExitSuccess   -> do --Print out created run directory.
                               !currenttandd <- DTime.getZonedTime
                               SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " ++ "Created " ++ (extractRunDirectory opts) ++ ".")

{-----------------------}


{-Create data sub-directory.-}

--createDataSubDirectory -> This function will
--create the data sub-directory within
--the run directory.
createDataSubDirectory :: CRSConfig -> IO ()
createDataSubDirectory opts = do
    !currenttandd <- DTime.getZonedTime
    SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                      ++ "Creating data sub-directory ...")
    (_,_,_,ph) <- SP.createProcess (SP.proc "mkdir" [(extractRunDirectory opts) ++ "data/"])
    ec <- SP.waitForProcess ph
    case ec of
        SX.ExitFailure _ -> do !currenttandd <- DTime.getZonedTime
                               error ("[" ++ (show currenttandd) ++ "] " 
                                          ++ "Could not create" ++ (extractRunDirectory opts) ++ "data/")
        SX.ExitSuccess   -> do --Print out created cellranger sub-directory.
                               !currenttandd <- DTime.getZonedTime
                               SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                                 ++ "Created " ++ (extractRunDirectory opts) ++ "data/.")

{---------------------------}


{-Create CellRanger sub-directory.-}

--createCellRangerSubDirectory -> This function will
--create the CellRanger sub-directory within
--the run directory.
createCellRangerSubDirectory :: CRSConfig -> IO ()
createCellRangerSubDirectory opts = do
    !currenttandd <- DTime.getZonedTime
    SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                       ++ "Creating cellranger sub-directory ...")
    (_,_,_,ph) <- SP.createProcess (SP.proc "mkdir" [(extractRunDirectory opts) ++ "cellranger/"])
    ec <- SP.waitForProcess ph
    case ec of
        SX.ExitFailure _ -> do !currenttandd <- DTime.getZonedTime
                               error ("[" ++ (show currenttandd) ++ "] " 
                                          ++ "Could not create " ++ (extractRunDirectory opts) ++ "cellranger/")
        SX.ExitSuccess   -> do --Print out created cellranger sub-directory.
                               !currenttandd <- DTime.getZonedTime
                               SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                                 ++ "Created " ++ (extractRunDirectory opts) ++ "cellranger/.")


{----------------------------------}

{-Create all sample directories.-}

--createAllSampleDirectories -> This function will
--create all applicable sample directories in the
--cellranger and data sub-directories.
createAllSampleDirectories :: [String] -> CRSConfig -> IO ()
createAllSampleDirectories []     _    = return ()
createAllSampleDirectories (x:xs) opts = do
    !currenttandd <- DTime.getZonedTime
    --Extract the rundirectory.
    let rundir = extractRunDirectory opts
    --Call createProcess to spawn new process to create current
    --cellranger sample directory.
    SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                      ++ "Creating " ++ x ++ " cellranger sample directory ...")
    (_,_,_,ph) <- SP.createProcess (SP.proc "mkdir" [rundir ++ "cellranger/" ++ x ++ "/"])
    ec <- SP.waitForProcess ph
    case ec of
        SX.ExitFailure _ -> do !currenttandd <- DTime.getZonedTime
                               error ("[" ++ (show currenttandd) ++ "] " 
                                          ++ "Could not create " ++ rundir ++ "cellranger/" ++ x ++ "/")
        SX.ExitSuccess   -> do --Print out created run directory.
                               !currenttandd <- DTime.getZonedTime
                               SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                                 ++ "Created " ++ rundir ++ "cellranger/" ++ x ++ "/.")
                               --Call createProcess to spawn new process to create current
                               --data sample directory.
                               SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                                 ++ "Creating " ++ x ++ " data sample directory ...")
                               (_,_,_,ph) <- SP.createProcess (SP.proc "mkdir" [rundir ++ "data/" ++ x ++ "/"])
                               ec <- SP.waitForProcess ph
                               case ec of
                                   SX.ExitFailure _ -> do !currenttandd <- DTime.getZonedTime
                                                          error ("[" ++ (show currenttandd) ++ "] " 
                                                                     ++ "Could not create " ++ rundir ++ "data/" ++ x ++ "/")
                                   SX.ExitSuccess   -> do --Print out created run directory.
                                                          !currenttandd <- DTime.getZonedTime
                                                          SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                                                            ++ "Created " ++ rundir ++ "data/" ++ x ++ "/.")  
                                                          --Recurse.
                                                          createAllSampleDirectories xs opts 

--createAllSubRunDirectories -> This function will
--create all applicable sub run directories,
--gene_expression feature_barcoding or
--vdj based on samplemap.
createAllSubRunDirectories :: [(String,String)] -> CRSConfig -> IO ()
createAllSubRunDirectories []     _    = return ()
createAllSubRunDirectories ((x,y):xs) opts = do
    !currenttandd <- DTime.getZonedTime
    --Extract the rundirectory.
    let rundir = extractRunDirectory opts
    --Call createProcess to spawn new process to create current
    --data sample sub-run directory.
    SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                       ++ "Creating " ++ x ++ "/" ++ y ++ "/" ++ " data sample directory ...")
    (_,_,_,ph) <- SP.createProcess (SP.proc "mkdir" [rundir ++ "data/" ++ x ++ "/" ++ y ++ "/"])
    ec <- SP.waitForProcess ph
    case ec of
        SX.ExitFailure _ -> do !currenttandd <- DTime.getZonedTime
                               error ("[" ++ (show currenttandd) ++ "] " 
                                          ++ "Could not create " ++ rundir ++ "data/" ++ x ++ "/" ++ y ++ "/.")
        SX.ExitSuccess   -> do --Print out created run directory.
                               !currenttandd <- DTime.getZonedTime
                               SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                                 ++ "Created " ++ rundir ++ "data/" ++ x ++ "/" ++ y ++ "/.")
                               --Recurse.
                               createAllSubRunDirectories xs opts 

{--------------------------------}


{-Copy fastq files to sample directories.-}

--copyFastqFilesToSampleDirectories -> This function will
--copy each fastq file listed within the Samplemap.csv
--to its respective sample directory.
copyFastqFilesToSampleDirectories :: [(String,String,String)] -> CRSConfig -> IO ()
copyFastqFilesToSampleDirectories []         _    = return ()
copyFastqFilesToSampleDirectories ((x,y,z):xs) opts = do
    !currenttandd <- DTime.getZonedTime
    --Extract the fastq directory.
    let fastqdir = extractFastqDirectory opts
    --Extract the run directory.
    let rundir = extractRunDirectory opts
    --Copy current fastq to correct sample
    --sun-run directory.
    SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                       ++ "Copying " ++ fastqdir ++ z ++ " to sample directory " ++ rundir ++ "data/" ++ x ++ "/" ++ y ++ "/...")
    (_,_,_,ph) <- SP.createProcess (SP.proc "cp" [fastqdir ++ z,rundir ++ "data/" ++ x ++ "/" ++ y ++ "/"])
    ec <- SP.waitForProcess ph
    case ec of
        SX.ExitFailure _ -> do !currenttandd <- DTime.getZonedTime
                               error ("[" ++ (show currenttandd) ++ "] " 
                                          ++ "Could not copy " ++ fastqdir ++ z ++ " to " 
                                          ++ rundir ++ "data/" ++ x ++ "/" ++ y ++ "/.")
        SX.ExitSuccess   -> do --Print out copied fastq.
                               !currenttandd <- DTime.getZonedTime
                               SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                                 ++ "Copied " ++ fastqdir ++ z ++ " to " 
                                                 ++ rundir ++ "data/" ++ x ++ "/" ++ y ++ "/.")
                               --Recurse.
                               copyFastqFilesToSampleDirectories xs opts 

{-----------------------------------------}


{-Create library.csv files for all sample directories.-}

--createLibrariesCsv -> This function will
--create the library.csv files for all sample
--directories.
createLibrariesCsv :: [(String,String,String)] -> CRSConfig -> IO ()
createLibrariesCsv []           _    = return ()
createLibrariesCsv ((x,y,z):xs) opts = do
    !currenttandd <- DTime.getZonedTime
    --Extract the run directory.
    let rundir = extractRunDirectory opts
    --Print out created libraries.csv
    --based on whether libraries.csv has
    --already been created.
    librariesfile <- SD.doesFileExist (rundir ++ "cellranger/" ++ x ++ "/libraries.csv")
    if | not librariesfile
       -> do --Create libraries.csv.
             SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                ++ "Creating " ++ rundir ++ "cellranger/" ++ x ++ "/libraries.csv ...") 
             let librariescsvdata = [["fastqs,sample,library_type\n"]]
             --Prepare librariescsvdata to print.
             let createlibrariescsvdata = DL.intercalate "\n" (DL.map (DL.intercalate "\t") librariescsvdata)
             --Write the output to libraries.csv.
             SIO.writeFile (rundir ++ "cellranger/" ++ x ++ "/libraries.csv") $ (createlibrariescsvdata)
             --Create sample for libraries.csv.
             let finalsample = TR.subRegex (TR.mkRegex "([^_]*_[^_]*)_.*") z "\\1"
             --Create library_type for libraries.csv.
             let librarytype = if | DL.isInfixOf "10x_5'_FeatureBarcoding" y
                                  -> "Antibody Capture"
                                  | otherwise
                                  -> "Gene Expression"
             SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                ++ "Adding data to " ++ rundir ++ "cellranger/" ++ x ++ "/libraries.csv ...")
             let librariescsvdata = [[rundir ++ "data/" ++ x ++ "/" ++ y ++ "/," ++ finalsample ++ "," ++ librarytype ++ "\n"]]
             --Prepare librariescsvdata to print.
             let finallibrariescsvdata = DL.intercalate "\n" (DL.map (DL.intercalate "\t") librariescsvdata)
             --Write the output to libraries.csv.
             SIO.appendFile (rundir ++ "cellranger/" ++ x ++ "/libraries.csv") $ (finallibrariescsvdata)
             SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                ++ "Added data to " ++ rundir ++ "cellranger/" ++ x ++ "/libraries.csv.")
             --Recurse.
             createLibrariesCsv xs opts
       | otherwise
       -> do --Create sample for libraries.csv.
             let finalsample = TR.subRegex (TR.mkRegex "([^_]*_[^_]*)_.*") z "\\1"
             --Create library_type for libraries.csv.
             let librarytype = if | DL.isInfixOf "10x_5'_FeatureBarcoding" y
                                  -> "Antibody Capture"
                                  | otherwise
                                  -> "Gene Expression"
             SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                ++ "Adding data to " ++ rundir ++ "cellranger/" ++ x ++ "/libraries.csv ...")
             let librariescsvdata = [[rundir ++ "data/" ++ x ++ "/" ++ y ++ "/," ++ finalsample ++ "," ++ librarytype ++ "\n"]]
             --Prepare librariescsvdata to print.
             let finallibrariescsvdata = DL.intercalate "\n" (DL.map (DL.intercalate "\t") librariescsvdata)
             --Write the output to libraries.csv.
             SIO.appendFile (rundir ++ "cellranger/" ++ x ++ "/libraries.csv") $ (finallibrariescsvdata)
             SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                ++ "Added data to " ++ rundir ++ "cellranger/" ++ x ++ "/libraries.csv.")
             --Recurse.
             createLibrariesCsv xs opts

{------------------------------------------------------}


{-Remove duplicate lines from libraries.csv.-}

--removeDupLibrariesCsv -> This function will
--remove duplicated lines in each libraries.csv file.
removeDupLibrariesCsv :: [(String,String,String)] -> CRSConfig -> IO ()
removeDupLibrariesCsv []           _    = return ()
removeDupLibrariesCsv ((x,y,z):xs) opts = do
    !currenttandd <- DTime.getZonedTime 
    --Extract the run directory.
    let rundir = extractRunDirectory opts
    --Remove duplicate lines.
    SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                       ++ "Removing duplicated lines from " ++ rundir ++ "cellranger/" ++ x ++ "/libraries.csv ...")
    (_,_,_,ph) <- SP.createProcess (SP.proc "gawk" ["-i","inplace","!seen[$0]++",rundir ++ "cellranger/" ++ x ++ "/libraries.csv"])
    ec <- SP.waitForProcess ph
    case ec of
        SX.ExitFailure _ -> do !currenttandd <- DTime.getZonedTime
                               error ("[" ++ (show currenttandd) ++ "] " 
                                          ++ "Could not remove duplicated lines from " ++ rundir 
                                          ++ "cellranger/" ++ x ++ "/libraries.csv.")
        SX.ExitSuccess   -> do --Print out created run directory.
                               !currenttandd <- DTime.getZonedTime
                               SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                                 ++ "Removed duplicated lines from " ++ rundir 
                                                 ++ "cellranger/" ++ x ++ "/libraries.csv.")
                               --Recurse.
                               removeDupLibrariesCsv xs opts

{--------------------------------------------}


{-Create and submit bsub commands via LSF.-}

--createAndSubmitBsubCommands -> This function will
--create and submit bsub commands via LSF
--based on configuration YAML.
createAndSubmitBsubCommands :: [(String,String,String)] -> CRSConfig -> [Int] -> IO ()
createAndSubmitBsubCommands []           _    _      = return ()
createAndSubmitBsubCommands _            _    []     = return ()
createAndSubmitBsubCommands ((x,y,z):xs) opts (a:as) = do
    !currenttandd <- DTime.getZonedTime
    if | DL.isInfixOf "10x_5'_FeatureBarcoding" y ||
         DL.isInfixOf "10x_SC_5'_V2_GEX" y &&
         (extractFeatureReferencePath opts) /= Nothing
       -> do --Create the bsub command.
             --Extract the run directory.
             let rundir = extractRunDirectory opts
             --Create finalid.
             let finalid = TR.subRegex (TR.mkRegex "([^_]*_[^_]*)_.*") z "\\1"
             SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                ++ "Constructing and submitting " ++ x ++ " bsub command via LSF ...")
             SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " ++ "bsub" ++ " " ++
                            "LSF_DOCKER_VOLUMES=$(echo \"" ++ (DL.intercalate " "
                                                              (DL.map
                                                              (DText.unpack)
                                                              (extractLsfDockerVolumes
                                                              (extractLsfVariables opts)))) ++ "\")" ++ " " ++
                            "-notify" ++ " " ++
                            "-J "  ++ (extractLsfJobName (extractLsfVariables opts)) ++ "[" ++ (show a) ++ "]" ++ " " ++
                            "-oo " ++ (extractBsubOoDirectory (extractLsfVariables opts)) 
                                   ++ (extractBsubOoPrefix (extractLsfVariables opts))  ++ "[" ++ (show a) ++ "]" ++ ".log" ++ " " ++
                            "-M "  ++ (extractMemoryLimit (extractBsubMemory (extractLsfVariables opts)))      ++  " " ++
                            "-q "  ++ (extractLsfQueue (extractLsfVariables opts))         ++  " " ++
                            "-G "  ++ (extractLsfComputeGroup (extractLsfVariables opts))  ++  " " ++
                            "-g "  ++ (extractLsfJobGroup (extractLsfVariables opts))      ++  " " ++
                            "-R \"select[mem>" ++ (extractResourceRequirementSelect (extractBsubMemory (extractLsfVariables opts))) ++ 
                            "] span[hosts=1] rusage[mem=" ++ (extractResourceRequirementUsage (extractBsubMemory (extractLsfVariables opts))) ++ "]\"" ++ " " ++
                            "-a 'docker0(" ++ (extractBsubDocker (extractLsfVariables opts)) ++ ")'" ++ " " ++   
                            "cellranger count" ++ " " ++
                            "--localmem=" ++ (extractLocalMemory (extractCellRangerOptions opts)) ++ " " ++
                            "--localcores=" ++ (extractLocalCores (extractCellRangerOptions opts)) ++ " " ++
                            "--id=" ++ finalid ++ " " ++ 
                            "--transcriptome=" ++ (DText.unpack (DMaybe.fromJust (extractTranscriptomeDirectoryPath opts))) ++ " " ++
                            "--featureref=" ++ (DText.unpack (DMaybe.fromJust (extractFeatureReferencePath opts))) ++ " " ++
                            "--libraries=" ++ rundir ++ "cellranger/" ++ x ++ "/libraries.csv")
             (_,_,_,ph) <- SP.createProcess (SP.proc "bsub" ["LSF_DOCKER_VOLUMES=$(echo \"" ++ (DL.intercalate " "
                                                             (DL.map
                                                             (DText.unpack)
                                                             (extractLsfDockerVolumes
                                                             (extractLsfVariables opts)))) ++ "\")"
                                                            ,"-notify"
                                                            ,"-J"
                                                            ,(extractLsfJobName 
                                                             (extractLsfVariables opts)) 
                                                             ++ "[" 
                                                             ++ (show a)
                                                             ++ "]" 
                                                            ,"-oo"
                                                            ,(extractBsubOoDirectory 
                                                             (extractLsfVariables opts))
                                                             ++ (extractBsubOoPrefix
                                                                (extractLsfVariables opts))
                                                             ++ "["
                                                             ++ (show a)
                                                             ++ "]"
                                                             ++ ".log"
                                                            ,"-M"
                                                            ,(extractMemoryLimit 
                                                             (extractBsubMemory 
                                                             (extractLsfVariables opts)))
                                                            ,"-q"
                                                            ,(extractLsfQueue 
                                                             (extractLsfVariables opts))
                                                            ,"-G"
                                                            ,(extractLsfComputeGroup 
                                                             (extractLsfVariables opts))
                                                            ,"-g"
                                                            ,(extractLsfJobGroup 
                                                             (extractLsfVariables opts))
                                                            ,"-R \"select[mem>" ++ (extractResourceRequirementSelect 
                                                                                   (extractBsubMemory 
                                                                                   (extractLsfVariables opts))) 
                                                                                ++ "]" 
                                                            ,"span[hosts=1]" 
                                                            ,"rusage[mem=" ++ (extractResourceRequirementUsage 
                                                                              (extractBsubMemory 
                                                                              (extractLsfVariables opts))) 
                                                                           ++ "]\""
                                                            ,"-a"
                                                            ,"'docker0(" ++ (extractBsubDocker (extractLsfVariables opts))
                                                                         ++ ")'"
                                                            ,"cellranger"
                                                            ,"count"
                                                            ,"--localmem=" ++ (extractLocalMemory 
                                                                              (extractCellRangerOptions opts))
                                                            ,"--localcores=" ++ (extractLocalCores 
                                                                                (extractCellRangerOptions opts))
                                                            ,"--id=" ++ finalid
                                                            ,"--transcriptome=" ++ (DText.unpack
                                                                                   (DMaybe.fromJust 
                                                                                   (extractTranscriptomeDirectoryPath opts)))
                                                            ,"--featureref=" ++ (DText.unpack
                                                                                (DMaybe.fromJust 
                                                                                (extractFeatureReferencePath opts)))
                                                            ,"--libraries=" ++ rundir 
                                                                            ++ "cellranger/" 
                                                                            ++ x 
                                                                            ++ "/libraries.csv"])
                                            {cwd = Just ((extractRunDirectory opts) ++ "cellranger/" ++ x)}
             ec <- SP.waitForProcess ph
             case ec of
                 SX.ExitFailure _ -> do !currenttandd <- DTime.getZonedTime
                                        error ("[" ++ (show currenttandd) ++ "] " 
                                                   ++ "Could not successfully construct and submit the above bsub command for " 
                                                   ++ x ++ " via LSF.")
                 SX.ExitSuccess   -> do --Print out created run directory.
                                        !currenttandd <- DTime.getZonedTime
                                        SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                                          ++ "Successfully constructed and submitted the above bsub command for " 
                                                          ++ x ++ " via LSF.")
       | DL.isInfixOf "10x_5'_FeatureBarcoding" y ||
         DL.isInfixOf "10x_SC_5'_V2_GEX" y &&
         (extractFeatureReferencePath opts) == Nothing
       -> do --Create the bsub command.
             --Extract the run directory.
             let rundir = extractRunDirectory opts
             --Create finalid.
             let finalid = TR.subRegex (TR.mkRegex "([^_]*_[^_]*)_.*") z "\\1"
             SIO.putStrLn ("[" ++ (show currenttandd) ++ "] "
                                ++ "Constructing and submitting " ++ x ++ " bsub command via LSF ...")
             SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " ++ "bsub" ++ " " ++
                            "LSF_DOCKER_VOLUMES=$(echo \"" ++ (DL.intercalate " "
                                                              (DL.map
                                                              (DText.unpack)
                                                              (extractLsfDockerVolumes
                                                              (extractLsfVariables opts)))) ++ "\")" ++ " " ++
                            "-notify" ++ " " ++
                            "-J "  ++ (extractLsfJobName (extractLsfVariables opts)) ++ "[" ++ (show a) ++ "]" ++ " " ++
                            "-oo " ++ (extractBsubOoDirectory (extractLsfVariables opts))
                                   ++ (extractBsubOoPrefix (extractLsfVariables opts))  ++ "[" ++ (show a) ++ "]" ++ ".log" ++ " " ++
                            "-M "  ++ (extractMemoryLimit (extractBsubMemory (extractLsfVariables opts)))      ++  " " ++
                            "-q "  ++ (extractLsfQueue (extractLsfVariables opts))         ++  " " ++
                            "-G "  ++ (extractLsfComputeGroup (extractLsfVariables opts))  ++  " " ++
                            "-g "  ++ (extractLsfJobGroup (extractLsfVariables opts))      ++  " " ++
                            "-R \"select[mem>" ++ (extractResourceRequirementSelect (extractBsubMemory (extractLsfVariables opts))) ++
                            "] span[hosts=1] rusage[mem=" ++ (extractResourceRequirementUsage (extractBsubMemory (extractLsfVariables opts))) ++ "]\"" ++ " " ++
                            "-a 'docker0(" ++ (extractBsubDocker (extractLsfVariables opts)) ++ ")'" ++ " " ++
                            "cellranger count" ++ " " ++
                            "--localmem=" ++ (extractLocalMemory (extractCellRangerOptions opts)) ++ " " ++
                            "--localcores=" ++ (extractLocalCores (extractCellRangerOptions opts)) ++ " " ++
                            "--id=" ++ finalid ++ " " ++
                            "--transcriptome=" ++ (DText.unpack (DMaybe.fromJust (extractTranscriptomeDirectoryPath opts))) ++ " " ++ 
                            "--libraries=" ++ rundir ++ "cellranger/" ++ x ++ "/libraries.csv")
             (_,_,_,ph) <- SP.createProcess (SP.proc "bsub" ["LSF_DOCKER_VOLUMES=$(echo \"" ++ (DL.intercalate " "
                                                             (DL.map
                                                             (DText.unpack)
                                                             (extractLsfDockerVolumes
                                                             (extractLsfVariables opts)))) ++ "\")"
                                                            ,"-notify"
                                                            ,"-J"
                                                            ,(extractLsfJobName
                                                             (extractLsfVariables opts))
                                                             ++ "["
                                                             ++ (show a)
                                                             ++ "]"
                                                            ,"-oo"
                                                            ,(extractBsubOoDirectory
                                                             (extractLsfVariables opts))
                                                             ++ (extractBsubOoPrefix
                                                                (extractLsfVariables opts))
                                                             ++ "["
                                                             ++ (show a)
                                                             ++ "]"
                                                             ++ ".log"
                                                            ,"-M"
                                                            ,(extractMemoryLimit
                                                             (extractBsubMemory
                                                             (extractLsfVariables opts)))
                                                            ,"-q"
                                                            ,(extractLsfQueue
                                                             (extractLsfVariables opts))
                                                            ,"-G"
                                                            ,(extractLsfComputeGroup
                                                             (extractLsfVariables opts))
                                                            ,"-g"
                                                            ,(extractLsfJobGroup
                                                             (extractLsfVariables opts))
                                                            ,"-R \"select[mem>" ++ (extractResourceRequirementSelect
                                                                                   (extractBsubMemory
                                                                                   (extractLsfVariables opts)))
                                                                                ++ "]"
                                                            ,"span[hosts=1]"
                                                            ,"rusage[mem=" ++ (extractResourceRequirementUsage
                                                                              (extractBsubMemory
                                                                              (extractLsfVariables opts)))
                                                                           ++ "]\""
                                                            ,"-a"
                                                            ,"'docker0(" ++ (extractBsubDocker (extractLsfVariables opts))
                                                                         ++ ")'"
                                                            ,"cellranger"
                                                            ,"count"
                                                            ,"--localmem=" ++ (extractLocalMemory
                                                                              (extractCellRangerOptions opts))
                                                            ,"--localcores=" ++ (extractLocalCores
                                                                                (extractCellRangerOptions opts))
                                                            ,"--id=" ++ finalid
                                                            ,"--transcriptome=" ++ (DText.unpack
                                                                                   (DMaybe.fromJust
                                                                                   (extractTranscriptomeDirectoryPath opts)))
                                                            ,"--libraries=" ++ rundir
                                                                            ++ "cellranger/"
                                                                            ++ x
                                                                            ++ "/libraries.csv"])
                                            {cwd = Just ((extractRunDirectory opts) ++ "cellranger/" ++ x)}
             ec <- SP.waitForProcess ph
             case ec of
                 SX.ExitFailure _ -> do !currenttandd <- DTime.getZonedTime
                                        error ("[" ++ (show currenttandd) ++ "] "
                                                   ++ "Could not successfully construct and submit the above bsub command for " 
                                                   ++ x ++ " via LSF.")
                 SX.ExitSuccess   -> do --Print out created run directory.
                                        !currenttandd <- DTime.getZonedTime
                                        SIO.putStrLn ("[" ++ (show currenttandd) ++ "] "
                                                          ++ "Successfully constructed and submitted the above bsub command for " 
                                                          ++ x ++ " via LSF.")
       | otherwise
       -> do --Create the bsub command.
             --Extract the run directory.
             let rundir = extractRunDirectory opts
             --Create finalid.
             let finalid = TR.subRegex (TR.mkRegex "([^_]*_[^_]*)_.*") z "\\1"
             SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                ++ "Constructing and submitting " ++ x ++ " bsub command via LSF ...")
             SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " ++ "bsub" ++ " " ++
                            "LSF_DOCKER_VOLUMES=$(echo \"" ++ (DL.intercalate " "
                                                              (DL.map
                                                              (DText.unpack)
                                                              (extractLsfDockerVolumes
                                                              (extractLsfVariables opts)))) ++ "\")" ++ " " ++
                            "-notify" ++ " " ++
                            "-J "  ++ (extractLsfJobName (extractLsfVariables opts)) ++ "[" ++ (show a) ++ "]" ++ " " ++
                            "-oo " ++ (extractBsubOoDirectory (extractLsfVariables opts))
                                   ++ (extractBsubOoPrefix (extractLsfVariables opts))  ++ "[" ++ (show a) ++ "]" ++ ".log" ++  " " ++
                            "-M "  ++ (extractMemoryLimit (extractBsubMemory (extractLsfVariables opts)))      ++  " " ++
                            "-q "  ++ (extractLsfQueue (extractLsfVariables opts))         ++  " " ++
                            "-G "  ++ (extractLsfComputeGroup (extractLsfVariables opts))  ++  " " ++
                            "-g "  ++ (extractLsfJobGroup (extractLsfVariables opts))      ++  " " ++
                            "-R \"select[mem>" ++ (extractResourceRequirementSelect (extractBsubMemory (extractLsfVariables opts))) ++
                            "] span[hosts=1] rusage[mem=" ++ (extractResourceRequirementUsage (extractBsubMemory (extractLsfVariables opts))) ++ "]\"" ++ " " ++
                            "-a 'docker0(" ++ (extractBsubDocker (extractLsfVariables opts)) ++ ")'" ++ " " ++
                            "cellranger count" ++ " " ++
                            "--localmem=" ++ (extractLocalMemory (extractCellRangerOptions opts)) ++ " " ++
                            "--localcores=" ++ (extractLocalCores (extractCellRangerOptions opts)) ++ " " ++
                            "--id=" ++ x ++ " " ++
                            "--sampleid=" ++ finalid ++ " " ++
                            "--reference=" ++ (DText.unpack (DMaybe.fromJust (extractVdjReferenceDirectoryPath opts))) ++ " " ++
                            "--fastqs=" ++ (rundir ++ "data/" ++ x ++ "/"))
             (_,_,_,ph) <- SP.createProcess (SP.proc "bsub" ["LSF_DOCKER_VOLUMES=$(echo \"" ++ (DL.intercalate " "
                                                             (DL.map
                                                             (DText.unpack)
                                                             (extractLsfDockerVolumes
                                                             (extractLsfVariables opts)))) ++ "\")"
                                                            ,"-notify"
                                                            ,"-J"
                                                            ,(extractLsfJobName
                                                             (extractLsfVariables opts))
                                                             ++ "["
                                                             ++ (show a)
                                                             ++ "]"
                                                            ,"-oo"
                                                            ,(extractBsubOoDirectory
                                                             (extractLsfVariables opts))
                                                             ++ (extractBsubOoPrefix
                                                                (extractLsfVariables opts))
                                                             ++ "["
                                                             ++ (show a)
                                                             ++ "]"
                                                             ++ ".log"
                                                            ,"-M"
                                                            ,(extractMemoryLimit
                                                             (extractBsubMemory
                                                             (extractLsfVariables opts)))
                                                            ,"-q"
                                                            ,(extractLsfQueue
                                                             (extractLsfVariables opts))
                                                            ,"-G"
                                                            ,(extractLsfComputeGroup
                                                             (extractLsfVariables opts))
                                                            ,"-g"
                                                            ,(extractLsfJobGroup
                                                             (extractLsfVariables opts))
                                                            ,"-R \"select[mem>" ++ (extractResourceRequirementSelect
                                                                                   (extractBsubMemory
                                                                                   (extractLsfVariables opts)))
                                                                                ++ "]"
                                                            ,"-a"
                                                            ,"'docker0(" ++ (extractBsubDocker (extractLsfVariables opts))
                                                                         ++ ")'"
                                                            ,"span[hosts=1]"
                                                            ,"rusage[mem=" ++ (extractResourceRequirementUsage
                                                                              (extractBsubMemory
                                                                              (extractLsfVariables opts)))
                                                                           ++ "]\""
                                                            ,"cellranger"
                                                            ,"count"
                                                            ,"--localmem=" ++ (extractLocalMemory
                                                                              (extractCellRangerOptions opts))
                                                            ,"--localcores=" ++ (extractLocalCores
                                                                                (extractCellRangerOptions opts))
                                                            ,"--id=" ++ x
                                                            ,"--sampleid=" ++ finalid
                                                            ,"--reference=" ++ (DText.unpack
                                                                               (DMaybe.fromJust 
                                                                               (extractVdjReferenceDirectoryPath opts)))
                                                            ,"--fastqs=" ++ rundir 
                                                                         ++ "data/" 
                                                                         ++ x 
                                                                         ++ "/"])
                                            {cwd = Just ((extractRunDirectory opts) ++ "cellranger/" ++ x)}
             ec <- SP.waitForProcess ph
             case ec of
                 SX.ExitFailure _ -> do !currenttandd <- DTime.getZonedTime
                                        error ("[" ++ (show currenttandd) ++ "] " 
                                                   ++ "Could not successfully construct and submit the above bsub command for " 
                                                   ++ x ++ " via LSF.")
                 SX.ExitSuccess   -> do --Print out created run directory.
                                        !currenttandd <- DTime.getZonedTime
                                        SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                                          ++ "Successfully constructed and submitted the above bsub command for " 
                                                          ++ x ++ " via LSF.")

{-----------------------------------------}


{-Wait on the jobs.-}

--waitOnJobsBwait -> This function will
--wait on the bjobs to finish.
waitOnJobsBwait :: CRSConfig -> IO ()
waitOnJobsBwait opts = do
    !currenttandd <- DTime.getZonedTime
    --Wait on jobs.
    SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                      ++ "Using bwait to wait for " ++ (extractLsfJobName (extractLsfVariables opts)) 
                      ++ " to finish.")
    (_,_,_,ph) <- SP.createProcess (SP.proc "bwait" ["-w","'ended(" ++ (extractLsfJobName (extractLsfVariables opts)) ++ ")'"])
    ec <- SP.waitForProcess ph
    case ec of
        SX.ExitFailure _ -> do !currenttandd <- DTime.getZonedTime
                               error ("[" ++ (show currenttandd) ++ "] " 
                                          ++ "Could not wait for " ++ (extractLsfJobName (extractLsfVariables opts)) 
                                          ++ " job to finish via bwait command.")
        SX.ExitSuccess   -> do --Print out created run directory.
                               !currenttandd <- DTime.getZonedTime
                               SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                                 ++ "Job " ++ (extractLsfJobName (extractLsfVariables opts)) 
                                                 ++ " finished via bwait command.")

{-------------------}


{-Create results directory.-}

--createResultsDirectory -> This function will
--create the requisite run directory.
createResultsDirectory :: CRSConfig -> IO ()
createResultsDirectory opts = do
    !currenttandd <- DTime.getZonedTime
    SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " ++ "Creating results directory ...")
    (_,_,_,ph) <- SP.createProcess (SP.proc "mkdir" [extractResultDirectory opts])
    ec <- SP.waitForProcess ph
    case ec of
        SX.ExitFailure _ -> do !currenttandd <- DTime.getZonedTime
                               error ("[" ++ (show currenttandd) ++ "] " ++ "Could not create " 
                                          ++ (extractResultDirectory opts) ++ ".")
        SX.ExitSuccess   -> do --Print out created run directory.
                               !currenttandd <- DTime.getZonedTime
                               SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " ++ "Created " 
                                                 ++ (extractResultDirectory opts) ++ ".")

{-----------------------}


{-Copy run directory to results directory and remove run directory.-}

--copyToResultsDirRemoveRunDir -> This function will
--recursively copy the run directory to the results directory
--location, and then remove the run directory.
copyToResultsDirRemoveRunDir :: CRSConfig -> IO ()
copyToResultsDirRemoveRunDir opts = do
    !currenttandd <- DTime.getZonedTime
    --Copy run directory to results directory.
    SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                       ++ "Copying (cp -r) " ++ (extractRunDirectory opts) ++ " to " 
                       ++ (extractResultDirectory opts) ++ ".")
    (_,_,_,ph) <- SP.createProcess (SP.proc "cp" ["-r",extractRunDirectory opts,extractResultDirectory opts])
    ec <- SP.waitForProcess ph
    case ec of
        SX.ExitFailure _ -> do !currenttandd <- DTime.getZonedTime
                               error ("[" ++ (show currenttandd) ++ "] " 
                                          ++ "Could not copy (cp -r) " ++ (extractRunDirectory opts) ++ " to " 
                                          ++ (extractResultDirectory opts) ++ ".")
        SX.ExitSuccess   -> do --Print out successful copy.
                               !currenttandd <- DTime.getZonedTime
                               SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                                 ++ "Successfully copied (cp -r) " ++ (extractRunDirectory opts) ++ " to " 
                                                 ++ (extractResultDirectory opts) ++ ".")
                               SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                                 ++ "Removing " ++ (extractRunDirectory opts) ++ ".")
                               (_,_,_,ph) <- SP.createProcess (SP.proc "rm" ["-r","-f",extractRunDirectory opts])
                               ec <- SP.waitForProcess ph
                               case ec of
                                   SX.ExitFailure _ -> do !currenttandd <- DTime.getZonedTime
                                                          error ("[" ++ (show currenttandd) ++ "] " 
                                                                     ++ "Could not remove (rm -rf) " 
                                                                     ++ (extractRunDirectory opts) ++ ".")
                                   SX.ExitSuccess   -> do --Print out successful copy.
                                                          !currenttandd <- DTime.getZonedTime
                                                          SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                                                            ++ "Successfully removed (rm -rf) " 
                                                                            ++ (extractRunDirectory opts) ++ ".")

{-------------------------------------------------------------------}

{-CRS Specific Function.-}

--processArgsAndFiles -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFiles :: ([Flag],[String]) -> IO ()
processArgsAndFiles ([],[]) = return () 
processArgsAndFiles (options,inputfiles) = do
    !currenttandd <- DTime.getZonedTime
    SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                       ++ "Starting Cell Ranger Setup ...")
    --Read in configuration YAML.
    readinputyaml <- DBC.readFile (inputfiles DL.!! 0) 
    --Decode readinputyaml.
    SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                       ++ "Decoding configuration YAML ...")
    decodedinputyaml <- 
        case decodeEither' readinputyaml of
            Left e -> error $ ("[" ++ (show currenttandd) ++ "] " 
                                   ++ "Could not parse configuration YAML file: \n" ++ show e)
            Right decodedinputyaml -> return decodedinputyaml
    --Read in the Samplemap.csv file.
    SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                       ++ "Parsing Samplemap.csv ...")
    decodedsamplemap <- parseFromFile csvFile (extractSamplemapCsv decodedinputyaml)
    case decodedsamplemap of
        Left  _ -> error ("[" ++ (show currenttandd) ++ "] " 
                              ++ "Could not parse" ++ (extractSamplemapCsv decodedinputyaml) ++ ".")
        Right r -> do --Create requisite directory structure for Cell Ranger runs.
                      createRunDirectory decodedinputyaml
                      createDataSubDirectory decodedinputyaml
                      createCellRangerSubDirectory decodedinputyaml
                      createAllSampleDirectories (DL.tail samples) decodedinputyaml
                      createAllSubRunDirectories (DL.tail uniqsamplesprotocols) decodedinputyaml
                      --Copy fastq files to appropriate sample directory
                      --with data sub-directory.
                      copyFastqFilesToSampleDirectories (DL.tail uniqsamplesprotocolsfilenames) decodedinputyaml
                      --Create libraries.csv files for all
                      --sample directories.
                      createLibrariesCsv (DL.tail uniqsamplesprotocolsfilenames) decodedinputyaml
                      --Remove duplicate lines from libraries.csv files.
                      removeDupLibrariesCsv (DL.tail nubuniqsamplesprotocolsfilenames) decodedinputyaml
                      --Submit bsub commands to LSF,
                      --and wait for the bsub commands to finish.
                      createAndSubmitBsubCommands (DL.tail nubuniqsamplesprotocolsfilenames) 
                                                  decodedinputyaml 
                                                  [1..(DL.length (DL.tail nubuniqsamplesprotocolsfilenames))]
                      waitOnJobsBwait decodedinputyaml
                      --Once bsub commands finish, copy run directory to results directory,
                      --and remove run directory once successfully copied.
                      createResultsDirectory decodedinputyaml
                      copyToResultsDirRemoveRunDir decodedinputyaml
                      --Cell Ranger Setup has finished successfully.
                      !currenttandd <- DTime.getZonedTime
                      SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                         ++ "Cell Ranger Setup has successfully finished.")
                      SIO.putStrLn ("[" ++ (show currenttandd) ++ "] " 
                                         ++ "Goodbye.")
            where
                --Local definitions.--
                samples = DL.nub
                          (DL.map
                          (DL.!!
                          (DMaybe.fromJust
                          (DL.elemIndex "Sample Name" (DL.head r)))) r)
                uniqsamplefilenames = DL.nub
                                      (DL.zip
                                      (DL.map (DL.!! (DMaybe.fromJust (DL.elemIndex "Sample Name" (DL.head r)))) r)
                                      (DL.map (DL.!! (DMaybe.fromJust (DL.elemIndex "File Name" (DL.head r)))) r))
                uniqsamplesprotocols = DL.nub
                                       (DL.zip
                                       (DL.map (DL.!! (DMaybe.fromJust (DL.elemIndex "Sample Name" (DL.head r)))) r)
                                       (DL.map (DL.!! (DMaybe.fromJust (DL.elemIndex "Protocol" (DL.head r)))) r))
                uniqsamplesprotocolsfilenames = DL.nub
                                               (DL.zip3 
                                               (DL.map (DL.!! (DMaybe.fromJust (DL.elemIndex "Sample Name" (DL.head r)))) r) 
                                               (DL.map (DL.!! (DMaybe.fromJust (DL.elemIndex "Protocol" (DL.head r)))) r) 
                                               (DL.map (DL.!! (DMaybe.fromJust (DL.elemIndex "File Name" (DL.head r)))) r))
                nubuniqsamplesprotocolsfilenames = nubBy (\(x,_,_) (y,_,_) -> x == y)
                                                         uniqsamplesprotocolsfilenames 
                ----------------------
{-------------------------}


{-Main function.-}

main :: IO ()
main = do
    --Set output buffering.
    hSetBuffering stdout NoBuffering
    --Get command line arguments.
    (args,files) <- SE.getArgs >>= compilerOpts
    --See if files is null
    if | (DL.length files) /= 1
       -> do --Print error statement and exit.
             SIO.putStrLn "CRS requires one argument:\n\
                          \Configuration YAML file\n"
             SX.exitWith (SX.ExitFailure 1)
       | otherwise 
       -> do --Run args and files through processArgsandFiles.
             processArgsAndFiles (args,files)
             
{----------------}
