{-=Cell Ranger Setup (CRS): A Haskell-based solution to=-}
{-=setting up cellranger count or vdj runs based on a=-}
{-=configuration YAML.=-}
{-=Author: Matthew Mosior=-}
{-=Version: 1.0=-}
{-=Synopsis:  This Haskell Script will take in a=-}
{-=configuration YAML and will set up and launch=-}
{-=Cell Ranger count and/or vdj runs via LSF bsub=-}
{-=commands accordingly.=-}


{-Lanuguage Extensions.-}

{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DeriveGeneric      #-}

{----------------------}


{-Module.-}

module YamlParser where

{--------}


{-Import modules.-}

import Control.Applicative as CA
import Control.Monad (mzero)
import Data.List as DL
import Data.Maybe as DMaybe
import Data.Yaml as DYaml
import Data.Aeson as DAeson
import Data.HashMap.Lazy as DHL
import Data.Map.Strict as DMap
import Data.Text as DText
import GHC.Generics

{----------------}


{-Custom YAML input file Datatype and related functions.-}

data CRSConfig = CRSConfig { samplemapcsv         :: Text
                           , fastqdirectory       :: Text
                           , rundirectory         :: Text
                           , resultdirectory      :: Text
                           , transcriptomedirpath :: Maybe Text
                           , featurerefpath       :: Maybe Text
                           , vdjreferencedirpath  :: Maybe Text
                           , lsfvariables         :: LSF
                           , cellrangeroptions    :: CellRangerOpts   
                           } deriving (Eq,Show,Read)

data LSF = LSF { lsfdockervolumes   :: [Text]
               , lsfjobgroup        :: Text
               , lsfcomputegroup    :: Text
               , lsfjobname         :: Text
               , lsfqueue           :: Text
               , bsuboodir          :: Text
               , bsubooprefix       :: Text
               , bsubdocker         :: Text
               , bsubcores          :: Text
               , bsubmemory         :: BsubMEM
               } deriving (Eq,Ord,Show,Read)

data BsubMEM = BsubMEM { memlimit     :: Text
                       , resreqselect :: Text
                       , resreqrusage :: Text
                       } deriving (Eq,Ord,Show,Read)

data CellRangerOpts = CellRangerOpts { localmem   :: Text
                                     , localcores :: Text
                                     } deriving (Eq,Ord,Show,Read)

instance FromJSON CRSConfig where
    parseJSON (Object v) = parseCRSConfig v
    parseJSON _          = CA.empty

parseCRSConfig v = CRSConfig
    <$> v .:  "samplemap_csv"
    <*> v .:  "fastq_directory"
    <*> v .:  "run_directory"
    <*> v .:  "result_directory"
    <*> v .:? "transcriptome_dir_path"
    <*> v .:? "feature_ref_path"
    <*> v .:? "vdj_reference_dir_path"
    <*> v .:  "lsf_variables"
    <*> v .:  "cellranger_options"

instance FromJSON LSF where
    parseJSON (Object v) = parseLSF v
    parseJSON _          = CA.empty

parseLSF v = LSF
    <$> v .: "lsf_docker_volumes"
    <*> v .: "lsf_job_group"
    <*> v .: "lsf_compute_group"
    <*> v .: "lsf_job_name"
    <*> v .: "lsf_queue"
    <*> v .: "bsub_oo_directory"
    <*> v .: "bsub_oo_prefix"
    <*> v .: "bsub_docker"
    <*> v .: "bsub_cores"
    <*> v .: "bsub_memory"

instance FromJSON BsubMEM where
    parseJSON (Object v) = parseBsubMEM v
    parseJSON _          = CA.empty

parseBsubMEM v = BsubMEM
    <$> v .: "memory_limit"
    <*> v .: "resource_requirement_select"
    <*> v .: "resource_requirement_rusage"

instance FromJSON CellRangerOpts where
    parseJSON (Object v) = parseCellRangerOpts v
    parseJSON _          = CA.empty

parseCellRangerOpts v = CellRangerOpts
    <$> v .: "local_memory"
    <*> v .: "local_cores"

{--------------------------------------------------------}


{-Custom extraction functions for CRSConfig Datatype.-}

--extractSamplemapCsv -> This function will
--extract the string associated with
--samplemapcsv.
extractSamplemapCsv :: CRSConfig -> String
extractSamplemapCsv (CRSConfig x _ _ _ _ _ _ _ _) = DText.unpack x

--extractFastqDirectory -> This function will
--extract the string associated with
--fastqdirectory.
extractFastqDirectory :: CRSConfig -> String
extractFastqDirectory (CRSConfig _ x _ _ _ _ _ _ _) = DText.unpack x

--extractRunDirectory -> This function will
--extract the string associated with
--rundirectory.
extractRunDirectory :: CRSConfig -> String
extractRunDirectory (CRSConfig _ _ x _ _ _ _ _ _) = DText.unpack x

--extractResultDirectory -> This function will
--extract the string associated with
--resultdirectory.
extractResultDirectory :: CRSConfig -> String
extractResultDirectory (CRSConfig _ _ _ x _ _ _ _ _) = DText.unpack x

--extractTranscriptomeDirectoryPath -> This function will
--extract the string associated with
--transcriptomedirpath.
extractTranscriptomeDirectoryPath :: CRSConfig -> Maybe Text
extractTranscriptomeDirectoryPath (CRSConfig _ _ _ _ x _ _ _ _) = x

--extractFeatureReferencePath -> This function will
--extract the string associated with
--featurerefpath.
extractFeatureReferencePath :: CRSConfig -> Maybe Text
extractFeatureReferencePath (CRSConfig _ _ _ _ _ x _ _ _) = x

--extractVdjReferenceDirectoryPath -> This function will
--extract the string associated with
--vdjreferencedirpath.
extractVdjReferenceDirectoryPath :: CRSConfig -> Maybe Text
extractVdjReferenceDirectoryPath (CRSConfig _ _ _ _ _ _ x _ _) = x

--extractLsfVariables -> This function will
--extract the string associated with
--lsfvariables.
extractLsfVariables :: CRSConfig -> LSF
extractLsfVariables (CRSConfig _ _ _ _ _ _ _ x _) = x

--extractCellRangerOptions -> This function will
--extract the string associated with
--cellrangeroptions.
extractCellRangerOptions :: CRSConfig -> CellRangerOpts
extractCellRangerOptions (CRSConfig _ _ _ _ _ _ _ _ x) = x

--extractLsfDockerVolumes -> This function will
--extract the string associated with
--lsfdockervolumes.
extractLsfDockerVolumes :: LSF -> [Text]
extractLsfDockerVolumes (LSF x _ _ _ _ _ _ _ _ _) = x

--extractLsfJobGroup -> This function will
--extract the string associated with
--lsfjobgroup.
extractLsfJobGroup :: LSF -> String
extractLsfJobGroup (LSF _ x _ _ _ _ _ _ _ _) = DText.unpack x

--extractLsfComputeGroup -> This function will
--extract the string associated with
--lsfcomputegroup.
extractLsfComputeGroup :: LSF -> String
extractLsfComputeGroup (LSF _ _ x _ _ _ _ _ _ _) = DText.unpack x

--extractLsfJobName -> This function will
--extract the string associated with
--lsfjobname.
extractLsfJobName :: LSF -> String
extractLsfJobName (LSF _ _ _ x _ _ _ _ _ _) = DText.unpack x

--extractLsfQueue -> This function will
--extract the string associated with
--lsfqueue.
extractLsfQueue :: LSF -> String
extractLsfQueue (LSF _ _ _ _ x _ _ _ _ _) = DText.unpack x

--extractBsubOoDirectory -> This function will
--extract the string associated with
--bsuboodir.
extractBsubOoDirectory :: LSF -> String
extractBsubOoDirectory (LSF _ _ _ _ _ x _ _ _ _) = DText.unpack x

--extractBsubOoPrefix -> This function will
--extract the string associated with
--bsubooprefix.
extractBsubOoPrefix :: LSF -> String
extractBsubOoPrefix (LSF _ _ _ _ _ _ x _ _ _) = DText.unpack x

--extractBsubDocker -> This function will
--extract the string associated with
--bsubdocker.
extractBsubDocker :: LSF -> String
extractBsubDocker (LSF _ _ _ _ _ _ _ x _ _) = DText.unpack x

--extractBsubCores -> This function will
--extract the string associated with
--bsubcores.
extractBsubCores :: LSF -> String
extractBsubCores (LSF _ _ _ _ _ _ _ _ x _) = DText.unpack x

--extractBsubMemory -> This function will
--extract the string associated with
--bsubmemory.
extractBsubMemory :: LSF -> BsubMEM
extractBsubMemory (LSF _ _ _ _ _ _ _ _ _ x) = x

--extractMemoryLimit -> This function will
--extract the string associated with
--memlimit.
extractMemoryLimit :: BsubMEM -> String
extractMemoryLimit (BsubMEM x _ _) = DText.unpack x

--extractResourceRequirementSelect -> This function will
--extract the string associated with
--resreqselect.
extractResourceRequirementSelect :: BsubMEM -> String
extractResourceRequirementSelect (BsubMEM _ x _) = DText.unpack x

--extractResourceRequirementUsage -> This function will
--extract the string associated with
--resrequsage.
extractResourceRequirementUsage :: BsubMEM -> String
extractResourceRequirementUsage (BsubMEM _ _ x) = DText.unpack x

--extractLocalMemory -> This function will
--extract the string associated with
--localmem.
extractLocalMemory :: CellRangerOpts -> String
extractLocalMemory (CellRangerOpts x _) = DText.unpack x

--extractLocalCores -> This function will
--extract the string associated with
--localcores.
extractLocalCores :: CellRangerOpts -> String
extractLocalCores (CellRangerOpts _ x) = DText.unpack x

{-----------------------------------------------------}
