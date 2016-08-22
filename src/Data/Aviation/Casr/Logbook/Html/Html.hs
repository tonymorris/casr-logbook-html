{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Casr.Logbook.Html.Html where

import Lucid
import Prelude

import Data.Aviation.Casr.Logbook
import Control.Lens
import Data.Char
import Data.Digit
import Data.Foldable
import Data.List
import Data.Time
import Data.String
import Control.Monad
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid hiding (Dual)
import qualified Data.Text as Text
import Text.Printf

data AircraftUsageExpense =
  AircraftUsageExpense {
    _aircraftusageexpenseperhour :: Int
  , _aircraftusageexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''AircraftUsageExpense

data AircraftLandingExpense =
  AircraftLandingExpense {
    _aircraftlandingexpenseamount :: Int
  , _aircraftlandingexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''AircraftLandingExpense

data AircraftFlightExpense =
  ExpenseAircraftUsage AircraftUsageExpense
  | ExpenseAircraftLanding AircraftLandingExpense
  deriving (Eq, Ord, Show)

makeClassyPrisms ''AircraftFlightExpense

data SimulatorFlightExpense =
  SimulatorFlightExpense {
    _simulatorflightexpenseperhour :: Int
  , _simulatorflightexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''SimulatorFlightExpense

data ExamExpense =
  ExamExpense {
    _examexpenseamount :: Int
  , _examexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''ExamExpense

data BriefingExpense =
  BriefingExpense {
    _briefingexpenseperhour :: Int
  , _briefingexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''BriefingExpense

data Visualisation =
  Doarama {
    _doaramaid :: String
  , _oembedid :: String
  , _doaramaname :: Maybe String
  } deriving (Eq, Ord, Show)

makeClassy ''Visualisation

data ImageType =
  Jpg
  | Png
  | Gif
  deriving (Eq, Ord, Show)

makeClassyPrisms ''ImageType

data Image =
  Image {
    _imageuri :: String
  , _imagetype :: ImageType
  , _imagesource :: Maybe String
  , _imagename :: Maybe String
  } deriving (Eq, Ord, Show)
  
makeClassy ''Image

data TrackLogType =
  Gpx
  | Kml
  | Kmz
  | ImageTrackLog ImageType
  deriving (Eq, Ord, Show)

makeClassyPrisms ''TrackLogType

data TrackLog =
  TrackLog {
    _trackloguri :: String
  , _tracklogtype :: TrackLogType
  , _tracklogsource :: Maybe String
  , _tracklogname :: Maybe String
  } deriving (Eq, Ord, Show)
  
makeClassy ''TrackLog

data VideoType =
  YouTube
  | Vimeo
  | Bambuser
  deriving (Eq, Ord, Show)
  
makeClassyPrisms ''VideoType

linkVideoType ::
  VideoType
  -> String
  -> String
linkVideoType YouTube u =
  "https://www.youtube.com/watch?v=" ++ u
linkVideoType Vimeo u =
  "https://bambuser.com/v/" ++ u 
linkVideoType Bambuser u =
  "https://vimeo.com/" ++ u

iframeVideoType ::
  VideoType
  -> String
  -> String
iframeVideoType YouTube u =
  "http://www.youtube.com/embed/" ++ u ++ "?autohide=1&amp;cc_load_policy=1&amp;color=white&amp;controls=1&amp;disablekb=0&amp;fs=1&amp;iv_load_policy=0&amp;loop=0&amp;modestbranding=1&amp;rel=0&amp;showinfo=0"
iframeVideoType Vimeo u =
  "https://player.vimeo.com/video/" ++ u
iframeVideoType Bambuser u =
  "https://embed.bambuser.com/broadcast/" ++ u ++ "?chat=1&amp;mute=0"

data Video =
  Video {
    _videouri :: String
  , _videotype :: VideoType
  , _videosource :: Maybe String
  , _videoname :: Maybe String
  } deriving (Eq, Ord, Show)

makeClassy ''Video

newtype TrackLogs =
  TrackLogs
    [TrackLog]
  deriving (Eq, Ord, Show)

makeClassy ''TrackLogs
makeWrapped ''TrackLogs

data AircraftFlightMeta =
  AircraftFlightMeta {
    _tracklogs :: [TrackLog]
  , _visualisations :: [Visualisation]
  , _images :: [Image]
  , _videos :: [Video]
  , _expenses :: [AircraftFlightExpense]
  } deriving (Eq, Ord, Show)

makeClassy '' AircraftFlightMeta

newtype SimulatorFlightMeta =
  SimulatorFlightMeta
    [SimulatorFlightExpense]
  deriving (Eq, Ord, Show)

makeClassy ''SimulatorFlightMeta
makeWrapped ''SimulatorFlightMeta

newtype ExamMeta =
  ExamMeta
    [ExamExpense]
  deriving (Eq, Ord, Show)

makeClassy ''ExamMeta
makeWrapped ''ExamMeta

newtype BriefingMeta =
  BriefingMeta
    [BriefingExpense]
  deriving (Eq, Ord, Show)

makeClassy ''BriefingMeta
makeWrapped ''BriefingMeta

---- My logbook

tonymorris ::
  Aviator
tonymorris =
  dobaviator
    "Morris"
    "Tony"
    [x1, x0, x0, x7, x0, x3, x6]
    (fromGregorian 1977 11 2)
    []

michaelward ::
  Aviator
michaelward =
  aviatorwithname
    "Ward"
    "Michael"
   
ryanlow ::
  Aviator
ryanlow =
  aviatorwithname
    "Low"
    "Ryan"

davidschofield ::
  Aviator
davidschofield =
  nodobaviator
    "Schofield"
    "David"
    [x5, x8, x9, x5, x2, x2]
    [Rating "FIR II" Nothing]

damienboyer ::
  Aviator
damienboyer =
  aviatorwithname
    "Boyer"
    "Damien"

clintdudman ::
  Aviator
clintdudman =
  aviatorwithname
    "Dudman"
    "Clint"

ryanmeyles ::
  Aviator
ryanmeyles =
  aviatorwithname
    "Meyles"
    "Ryan"

kenosbourne ::
  Aviator
kenosbourne =
  aviatorwithname
    "Osbourne"
    "Ken"

vhwkm ::
  Aircraft
vhwkm =
  singleaircraft
    "1980 American Champion Citabria 7GCBC"
    "VH-WKM"

vhldo ::
  Aircraft
vhldo =
  singleaircraft
    "2011 Cessna 162 Skycatcher"
    "VH-LDO"

vhafr ::
  Aircraft
vhafr =
  singleaircraft
    "2000 Cessna 172S Skyhawk"
    "VH-AFR"

vhvvo ::
  Aircraft
vhvvo =
  singleaircraft
    "2000 Cessna 172R Skyhawk"
    "VH-VVO"

vhzwy ::
  Aircraft
vhzwy =
  singleaircraft
    "2000 Cessna 172S Skyhawk"
    "VH-ZWY"

flightone ::
  Location
flightone =
  Location
    "Flight One"
    (-27.566768)
    153.014955

aslExamArcherfield ::
  Location
aslExamArcherfield =
  Location
    "Assessment Skills Limited (ASL) -- Archerfield"
    (-27.570284)
    153.014238

flightoneC162AircraftUsageExpense ::
  String
  -> AircraftFlightExpense
flightoneC162AircraftUsageExpense =
  ExpenseAircraftUsage . AircraftUsageExpense 34100

flightoneC172AircraftUsageExpense ::
  String
  -> AircraftFlightExpense
flightoneC172AircraftUsageExpense =
  ExpenseAircraftUsage . AircraftUsageExpense 41800

vhldoUnderInstruction ::
  AircraftFlightExpense
vhldoUnderInstruction =
  flightoneC162AircraftUsageExpense "VH-LDO Under Instruction"

vhldoLanding ::
  AircraftFlightExpense
vhldoLanding =
  ExpenseAircraftLanding (AircraftLandingExpense 2979 "LDO Landing & Terminal Charge")  

vhafrUnderInstruction ::
  AircraftFlightExpense
vhafrUnderInstruction =
  flightoneC172AircraftUsageExpense "VH-AFR Under Instruction"

vhafrLanding ::
  AircraftFlightExpense
vhafrLanding =
  ExpenseAircraftLanding (AircraftLandingExpense 3273 "AFR Landing & Terminal Charge")

vhzwyUnderInstruction ::
  AircraftFlightExpense
vhzwyUnderInstruction =
  flightoneC172AircraftUsageExpense "VH-ZWY Under Instruction"

vhzwyLanding ::
  AircraftFlightExpense
vhzwyLanding =
  ExpenseAircraftLanding (AircraftLandingExpense 3273 "ZWY Landing & Terminal Charge")

vhvvoUnderInstruction ::
  AircraftFlightExpense
vhvvoUnderInstruction =
  flightoneC172AircraftUsageExpense "VH-VVO Under Instruction"

vhvvoLanding ::
  AircraftFlightExpense
vhvvoLanding =
  ExpenseAircraftLanding (AircraftLandingExpense 3273 "VVO Landing & Terminal Charge")

preflightBriefing ::
  Briefing
preflightBriefing =
  Briefing "Pre-flight Inspection" flightone (dayonly (fromGregorian 2015 12 10)) michaelward (parttimeamount x5)

preflightBriefingMeta ::
  BriefingMeta
preflightBriefingMeta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing - pre flight inspection"] 

effectofcontrols ::
  AircraftFlight
effectofcontrols =
  noif_dualonlyflight
    "Effects of Controls"
    vhldo
    michaelward
    (day 1 x2)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2015 12 14)))    

effectofcontrolsMeta ::
  AircraftFlightMeta
effectofcontrolsMeta =
  AircraftFlightMeta
    []
    []
    []
    []
    [
      vhldoUnderInstruction
    , vhldoLanding
    ]
  
effectofcontrolsBriefing ::
  Briefing
effectofcontrolsBriefing =
  Briefing "Effect of controls" flightone (dayonly (fromGregorian 2015 12 14)) michaelward (parttimeamount x8)

effectofcontrolsBriefingMeta ::
  BriefingMeta
effectofcontrolsBriefingMeta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing - Operation and effects"] 

straightandlevelBriefing :: 
  Briefing
straightandlevelBriefing =
  Briefing "Straight and Level" flightone (dayonly (fromGregorian 2015 12 18)) michaelward (TimeAmount 1 x0)

straightandlevelBriefingMeta ::
  BriefingMeta
straightandlevelBriefingMeta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing - Basic"] 

straightandlevel ::
  AircraftFlight
straightandlevel =
  noif_dualonlyflight
    "Straight & Level"
    vhldo
    michaelward
    (day 1 x0)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2015 12 18)))    
    
straightandlevelMeta ::
  AircraftFlightMeta
straightandlevelMeta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20151218-vh-ldo.gpx"
        Gpx
        (Just "Garmin 62s")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20151218-vh-ldo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "595690"
        "6rAdypE"
        Nothing
    ]
    []
    [
      Video
        "13BVior4VmY"
        YouTube
        Nothing
        (Just "Head camera")
    ]
    [
      vhldoUnderInstruction
    , vhldoLanding
    ]

climbinganddescendingBriefing ::
  Briefing
climbinganddescendingBriefing =
  Briefing "Climbing and Descending" flightone (dayonly (fromGregorian 2015 12 20)) ryanlow (TimeAmount 1 x0)

climbinganddescendingBriefingMeta ::
  BriefingMeta
climbinganddescendingBriefingMeta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing - Basic"] 

climbinganddescending ::
  AircraftFlight
climbinganddescending =
  noif_dualonlyflight
    "Climbing & Descending"
    vhldo
    michaelward
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2015 12 20)))    
    
climbinganddescendingMeta ::
  AircraftFlightMeta
climbinganddescendingMeta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20151220-vh-ldo.gpx"
        Gpx
        (Just "Garmin 62s")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20151220-vh-ldo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "596790"
        "6rZrPp6"
        Nothing
    ]
    []
    [
      Video
        "8tZ8kxsVz6E"
        YouTube
        Nothing
        (Just "Head camera")
    ]
    [
      vhldoUnderInstruction
    , vhldoLanding
    ]

turning ::
  AircraftFlight
turning =
  noif_dualonlyflight
    "Turning"
    vhldo
    ryanlow
    (day 1 x3)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 1 4)))    
    
turningMeta ::
  AircraftFlightMeta
turningMeta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160104-vh-ldo.gpx"
        Gpx
        (Just "Garmin 62s")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160104-vh-ldo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "607004"
        "6x5Az8e"
        Nothing
    ]
    []
    []
    [
      vhldoUnderInstruction
    , ExpenseAircraftUsage (AircraftUsageExpense 1000 "Concierge - Headset rental")
    , vhldoLanding
    ]

stallingBriefing1 :: 
  Briefing
stallingBriefing1 =
  Briefing "Stalling" flightone (dayonly (fromGregorian 2016 1 4)) michaelward (TimeAmount 1 x0)

stallingBriefing1Meta ::
  BriefingMeta
stallingBriefing1Meta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing - stalling"] 

stalling1 ::
  AircraftFlight
stalling1 =
  noif_dualonlyflight
    "Stalling (1)"
    vhldo
    davidschofield
    (day 1 x3)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 1 8)))    
    
stalling1Meta ::
  AircraftFlightMeta
stalling1Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160108-vh-ldo.gpx"
        Gpx
        (Just "Garmin 62s")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160108-vh-ldo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "611980"
        "eYQA7ve"
        Nothing
    ]
    []
    []
    [
      vhldoUnderInstruction
    , vhldoLanding
    ]

stallingBriefing2 :: 
  Briefing
stallingBriefing2 =
  Briefing "Stalling" flightone (dayonly (fromGregorian 2016 1 8)) davidschofield (TimeAmount 1 x3)

stallingBriefing2Meta ::
  BriefingMeta
stallingBriefing2Meta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing - basic"] 

stalling2 ::
  AircraftFlight
stalling2 =
  noif_dualonlyflight
    "Stalling (2)"
    vhafr
    davidschofield
    (day 1 x4)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 1 15)))    
    
stalling2Meta ::
  AircraftFlightMeta
stalling2Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160115-vh-afr.gpx"
        Gpx
        (Just "Garmin 62s")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160115-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "617610"
        "k8p38wk"
        Nothing
    ]
    []
    []
    [
      flightoneC172AircraftUsageExpense "VH-AFR dual -ab-initio consolidation"
    , vhafrLanding
    ]
  
circuits1 ::
  AircraftFlight
circuits1 =
  noif_dualonlyflight
    "Circuits"
    vhafr
    davidschofield
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 1 22)))    
    
circuits1Meta ::
  AircraftFlightMeta
circuits1Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160122-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160122-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "622258"
        "k0nXwd6"
        Nothing
    ]
    [
      Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082411.jpg"
        Jpg
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082856.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082902.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082905.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_083348.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_083352.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_083353.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_084902.jpg"
        Jpg 
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_084906.jpg"
        Jpg 
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_084908.jpg"
        Jpg      
        Nothing
        Nothing
    ]
    [
      Video
        "gz8Ivcjas9o"
        YouTube
        Nothing
        (Just "Head camera")
    ]
    [
      flightoneC172AircraftUsageExpense "VH-AFR dual/ circuits #1"
    , vhafrLanding
    ]
  
circuits2 ::
  AircraftFlight
circuits2 =
  noif_dualonlyflight
    "Circuits"
    vhafr
    davidschofield
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 1 29)))    
    
circuits2Meta ::
  AircraftFlightMeta
circuits2Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160129-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160129-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "622258"
        "k0nXwd6"
        Nothing
    ]
    [
      Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082411.jpg"
        Jpg
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082856.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082902.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082905.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_083348.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_083352.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_083353.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_084902.jpg"
        Jpg 
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_084906.jpg"
        Jpg 
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_084908.jpg"
        Jpg      
        Nothing
        Nothing
    ]
    [
      Video
        "gz8Ivcjas9o"
        YouTube
        Nothing
        (Just "Head camera")
    ]
    [
      vhafrUnderInstruction
    , vhafrLanding
    ]
  
circuits3 ::
  AircraftFlight
circuits3 =
  noif_dualonlyflight
    "Circuits"
    vhafr
    davidschofield
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 2 5)))
    
circuits3Meta ::
  AircraftFlightMeta
circuits3Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160205-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160205-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "635027"
        "eBdaGlE"
        Nothing
    ]
    []
    []
    [
      flightoneC172AircraftUsageExpense "VH-AFR circuits #3"
    , vhafrLanding
    ]
  
circuits4 ::
  AircraftFlight
circuits4 =
  noif_dualonlyflight
    "Circuits"
    vhafr
    davidschofield
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 2 12)))    
    
circuits4Meta ::
  AircraftFlightMeta
circuits4Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160212-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160212-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "640001"
        "E2y3mnk"
        Nothing
    ]
    []
    [
      Video
        "e7UcjgxOmDw"
        YouTube
        Nothing
        (Just "Head camera")
    ]
    [
      flightoneC172AircraftUsageExpense "VH-AFR circuits #4"
    , vhafrLanding
    ]

circuitemergenciesBriefing1 ::
  Briefing
circuitemergenciesBriefing1 =
  Briefing "Circuit Emergencies" flightone (dayonly (fromGregorian 2016 2 12)) davidschofield (parttimeamount x5)

circuitemergenciesBriefing1Meta ::
  BriefingMeta
circuitemergenciesBriefing1Meta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing - Basic"] 

circuitemergencies1 ::
  AircraftFlight
circuitemergencies1 =
  noif_dualonlyflight
    "Circuit Emergencies"
    vhafr
    davidschofield
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 2 18)))    
    
circuitemergencies1Meta ::
  AircraftFlightMeta
circuitemergencies1Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160218-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160218-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "644663"
        "Ep1r9P6"
        Nothing
    ]
    [
      Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090347.jpg"
        Jpg
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090348.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090351.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090409.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090411.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090421.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090423.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090432.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090436.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090441.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090446.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090452.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090454.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090459.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090501.jpg"
        Jpg            
        Nothing
        Nothing
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090502.jpg"
        Jpg            
        Nothing
        Nothing        
    ]
    [
      Video
        "EO4D4zFnpIU"
        YouTube
        Nothing
        (Just "Head camera")
    ]
    [
      vhafrUnderInstruction
    , vhafrLanding
    ]

circuitemergenciesBriefing2 ::
  Briefing
circuitemergenciesBriefing2 =  
  Briefing "Circuit Emergencies" flightone (dayonly (fromGregorian 2016 2 18)) davidschofield (TimeAmount 1 x0)

circuitemergenciesBriefing2Meta ::
  BriefingMeta
circuitemergenciesBriefing2Meta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing - Basic cct emer"] 

firstsoloexam ::
  Exam
firstsoloexam =
  dayonlyexam
    "First Solo Theory Exam"
    flightone
    (fromGregorian 2016 2 25)
    davidschofield
    31
    40

firstsoloexamMeta ::
  ExamMeta
firstsoloexamMeta =
  ExamMeta
    [ExamExpense 13200 "EXAM - In House Solo exam Pass 77%"]

circuitemergencies2 ::
  AircraftFlight
circuitemergencies2 =
  noif_dualonlyflight
    "Circuit Emergencies"
    vhafr
    davidschofield
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 3 4)))    

circuitemergencies2Meta ::
  AircraftFlightMeta
circuitemergencies2Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160304-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160304-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "674600"
        "6XbQDjk"
        Nothing
    ]
    []
    [
      Video
        "jWUijREg4aE"
        YouTube
        Nothing
        (Just "Head camera")
    ]
    [
      vhafrUnderInstruction
    , vhafrLanding
    ]

circuitemergenciesBriefing3 ::
  Briefing
circuitemergenciesBriefing3 =
  Briefing "Circuit Emergencies" flightone (dayonly (fromGregorian 2016 2 25)) davidschofield (parttimeamount x5)

circuitemergenciesBriefing3Meta ::
  BriefingMeta
circuitemergenciesBriefing3Meta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing - CCT emergencies (DS)"] 

areasoloexam ::
  Exam
areasoloexam =
  dayonlyexam
    "First Solo Theory Exam"
    flightone
    (fromGregorian 2016 3 9)
    davidschofield
    38
    40

areasoloexamMeta ::
  ExamMeta
areasoloexamMeta =
  ExamMeta
    [ExamExpense 13200 "EXAM - In House area solo Pass!"]

circuitemergencies3 ::
  AircraftFlight
circuitemergencies3 =
  noif_dualonlyflight
    "Circuit Emergencies"
    vhzwy
    davidschofield
    (day 1 x0)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 3 24)))    
    
circuitemergencies3Meta ::
  AircraftFlightMeta
circuitemergencies3Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160324-vh-zwy.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160324-vh-zwy.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "691106"
        "6bPVnnk"
        Nothing
    ]
    []
    [
      Video
        "RNiYu3ZsfZw"
        YouTube
        Nothing
        (Just "Forward camera")
    ]
    [
      vhzwyUnderInstruction
    , vhzwyLanding
    ]

circuitemergenciesBriefing4 ::
  Briefing
circuitemergenciesBriefing4 =
  Briefing "Circuit Emergencies" flightone (dayonly (fromGregorian 2016 3 4)) davidschofield (parttimeamount x7)

circuitemergenciesBriefing4Meta ::
  BriefingMeta
circuitemergenciesBriefing4Meta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing - circuits"] 

circuits5 ::
  AircraftFlight
circuits5 =
  noif_dualonlyflight
    "Circuits"
    vhzwy
    davidschofield
    (day 0 x9)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 3 30)))    
    
circuits5Meta ::
  AircraftFlightMeta
circuits5Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160330-vh-zwy.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160330-vh-zwy.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "696748"
        "6xV5dVk"
        Nothing
    ]
    []
    []
    [
      flightoneC172AircraftUsageExpense "VH-ZWY Under Instruction.9/solo .4"
    ]

circuitemergenciesBriefing5 ::
  Briefing
circuitemergenciesBriefing5 =    
  Briefing "Circuits" flightone (dayonly (fromGregorian 2016 3 9)) davidschofield (parttimeamount x5)

circuitemergenciesBriefing5Meta ::
  BriefingMeta
circuitemergenciesBriefing5Meta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing - Basic"]

firstsolo ::
  AircraftFlight
firstsolo =
  noif_commandonlyflight
    "Circuits (first solo)"
    vhzwy
    (day 0 x4)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 3 30)))    
    
firstsoloMeta ::
  AircraftFlightMeta
firstsoloMeta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160330-vh-zwy.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160330-vh-zwy.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "696748"
        "6xV5dVk"
        Nothing
    ]
    []
    []
    [
      flightoneC172AircraftUsageExpense "VH-ZWY Under Instruction.9/solo .4"
    , vhzwyLanding
    ]

circuitemergenciesBriefing6 ::
  Briefing
circuitemergenciesBriefing6 =
  Briefing "Circuits" flightone (dayonly (fromGregorian 2016 3 24)) davidschofield (parttimeamount x4)

circuitemergenciesBriefing6Meta ::
  BriefingMeta
circuitemergenciesBriefing6Meta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing - Basic"]

circuitscrosswind1 ::
  AircraftFlight
circuitscrosswind1 =
  noif_dualonlyflight
    "Circuits (Crosswind)"
    vhafr
    davidschofield
    (day 1 x0)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 4 15)))    
    
circuitscrosswind1Meta ::
  AircraftFlightMeta
circuitscrosswind1Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160415-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160415-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "711553"
        "k9nWdbE"
        Nothing
    ]
    []
    []
    [
      flightoneC172AircraftUsageExpense "VH-AFR Under Instruction.9/solo .4"
    , vhafrLanding
    ]

circuitscrosswindBriefing :: 
  Briefing
circuitscrosswindBriefing =
  Briefing "Circuits Crosswind" flightone (dayonly (fromGregorian 2016 4 15)) davidschofield (parttimeamount x5)

circuitscrosswindBriefingMeta :: 
  BriefingMeta
circuitscrosswindBriefingMeta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing - X-Wind CCTs"]

circuitssolocheck1 ::
  AircraftFlight
circuitssolocheck1 =
  noif_dualonlyflight
    "Circuits (solo check)"
    vhafr
    davidschofield
    (day 0 x5)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 4 22)))    
    
circuitssolocheck1Meta ::
  AircraftFlightMeta
circuitssolocheck1Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160422-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160422-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "719248"
        "6xVbR8k"
        Nothing
    ]
    []
    [
      Video
        "UR1IkF5RSh4"
        YouTube
        Nothing
        (Just "Forward camera")
    ]
    [
      flightoneC172AircraftUsageExpense "VH-AFR Under Instruction/solo"
    ]

circuitssolo1 ::
  AircraftFlight
circuitssolo1 =
  noif_commandonlyflight
    "Circuits (solo)"
    vhafr
    (day 1 x0)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 4 22)))    
    
circuitssolo1Meta ::
  AircraftFlightMeta
circuitssolo1Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160422-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160422-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "719248"
        "6xVbR8k"
        Nothing
    ]
    []
    [
      Video
        "UR1IkF5RSh4"
        YouTube
        Nothing
        (Just "Forward camera")
    ]
    [
      flightoneC172AircraftUsageExpense "VH-AFR Under Instruction/solo"
    , vhafrLanding
    ]

circuitssolocheck2 ::
  AircraftFlight
circuitssolocheck2 =
  noif_dualonlyflight
    "Circuits (solo check)"
    vhvvo
    davidschofield
    (day 0 x6)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 4 27)))   
    
circuitssolocheck2Meta ::
  AircraftFlightMeta
circuitssolocheck2Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160427-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160427-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "724934"
        "eo1yvBE"
        Nothing
    ]
    []
    []
    [
      flightoneC172AircraftUsageExpense "VH-VVO Under Instruction.6/Solo.8"
    ]

circuitssolo2 ::
  AircraftFlight
circuitssolo2 =
  noif_commandonlyflight
    "Circuits (solo)"
    vhvvo
    (day 0 x8)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 4 27)))    
    
circuitssolo2Meta ::
  AircraftFlightMeta
circuitssolo2Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160427-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160427-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "724934"
        "eo1yvBE"
        Nothing
    ]
    []
    []
    [
      flightoneC172AircraftUsageExpense "VH-VVO Under Instruction.6/Solo.8"
    , vhvvoLanding
    ]

practiceforcedlandingsBriefing :: 
  Briefing
practiceforcedlandingsBriefing =
  Briefing "Practice Forced Landings" flightone (dayonly (fromGregorian 2016 5 9)) davidschofield (TimeAmount 1 x5)

practiceforcedlandingsBriefingMeta ::
  BriefingMeta
practiceforcedlandingsBriefingMeta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing - Basic"]

practiceforcedlandings1 ::
  AircraftFlight
practiceforcedlandings1 =
  noif_dualonlyflight
    "Practice Forced Landings"
    vhafr
    davidschofield
    (day 1 x0)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 5 9)))    
     
practiceforcedlandings1Meta ::
  AircraftFlightMeta
practiceforcedlandings1Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160509-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160509-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "741813"
        "edWMLO6"
        Nothing
    ]
    []
    [
      Video
        "o1dx3hZuov0"
        YouTube
        Nothing
        (Just "Forward camera")
    , Video
        "6250595"
        Bambuser
        Nothing
        (Just "Live Stream")
    ]
    [
      vhafrUnderInstruction
    , vhafrLanding
    ]

practiceforcedlandings2 ::
  AircraftFlight
practiceforcedlandings2 =
  noif_dualonlyflight
    "Practice Forced Landings"
    vhvvo
    davidschofield
    (day 1 x2)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 5 14)))
    
practiceforcedlandings2Meta ::
  AircraftFlightMeta
practiceforcedlandings2Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160514-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160514-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "748638"
        "6jAODJE"
        Nothing
    ]
    []
    [
      Video
        "vw9KjjqkOEg"
        YouTube
        Nothing
        (Just "Forward camera")
    , Video
        "6258610"
        Bambuser
        Nothing
        (Just "Live Stream")
    ]
    [
      vhvvoUnderInstruction
    , vhvvoLanding
    ]

steepturnsBriefing ::
  Briefing
steepturnsBriefing =
  Briefing "Steep Turns" flightone (dayonly (fromGregorian 2016 5 9)) davidschofield (TimeAmount 1 x5)

steepturnsBriefingMeta ::
  BriefingMeta
steepturnsBriefingMeta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing"]

steepturns ::
  AircraftFlight
steepturns =
  noif_dualonlyflight
    "Steep Turns"
    vhvvo
    davidschofield
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 5 19)))    
    
steepturnsMeta ::
  AircraftFlightMeta
steepturnsMeta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160519-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160519-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "756397"
        "k9nNR4E"
        Nothing
    ]
    []
    [
      Video
        "ZEABSxrN4xM"
        YouTube
        Nothing
        (Just "Forward camera")
    , Video
        "6267124"
        Bambuser
        Nothing
        (Just "Live Stream")
    ]
    [
      flightoneC172AircraftUsageExpense "VH-VVO Under Instruction - steep turns"
    , vhvvoLanding
    ]

sydneycircuit ::
  AircraftFlight
sydneycircuit =
  noif_dualonlyflight
    "Sydney Circuit, Stalls"
    vhwkm
    kenosbourne
    (day 1 x8)
    (pointsatdate "YSCN" ["PSP", "PAR", "LRF", "V1", "JIBN", "SECF"] "YSCN" (fromGregorian 2016 5 19))
   
sydneycircuitMeta ::
  AircraftFlightMeta
sydneycircuitMeta =
  AircraftFlightMeta
    []
    []
    []
    []
    [
      ExpenseAircraftUsage (AircraftUsageExpense 28844 "VH-WKM 1.8 hours dual")
    ]

steepturnsandsideslipping ::
  AircraftFlight
steepturnsandsideslipping =
  noif_dualonlyflight
    "Steep Turns and Sideslipping"
    vhvvo
    damienboyer
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 5 30)))    
    
steepturnsandsideslippingMeta ::
  AircraftFlightMeta
steepturnsandsideslippingMeta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160530-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160530-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "770143"
        "65oMwXk"
        Nothing
    ]
    []
    [
      Video
        "6288135"
        Bambuser
        Nothing
        (Just "Live Stream")
    ]
    [
      vhvvoUnderInstruction
    , vhvvoLanding
    ]

circuitscrosswind2 ::
  AircraftFlight
circuitscrosswind2 =
  noif_dualonlyflight
    "Circuits (Crosswind)"
    vhvvo
    davidschofield
    (day 0 x9)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 6 6)))    
  
circuitscrosswind2Meta ::
  AircraftFlightMeta
circuitscrosswind2Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160606-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160606-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "778822"
        "eOrqL86"
        Nothing
    ]
    []
    [
      Video
        "OYuR7E1xjyg"
        YouTube
        Nothing            
        (Just "Forward camera")
    , Video
        "6300417"
        Bambuser
        Nothing
        (Just "Live stream")
    ]
    [
      flightoneC172AircraftUsageExpense "VH-VVO x-wind circuits"
    , vhvvoLanding
    ]

areasolocheck ::
  AircraftFlight
areasolocheck =
  noif_dualonlyflight
    "Area Solo Check"
    vhvvo
    davidschofield
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 6 16)))    

areasolocheckMeta ::
  AircraftFlightMeta
areasolocheckMeta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160616-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160616-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "798173"
        "67W2vz6"
        Nothing
    ]
    []
    [
      Video
        "6318524"
        Bambuser
        Nothing
        (Just "Live stream")
    ]
    [
      flightoneC172AircraftUsageExpense "VH-VVO Under Instruction (DS)"
    , vhvvoLanding
    ]

areasoloBriefing1 ::
  Briefing
areasoloBriefing1 =
  Briefing "Area Solo Preparation" flightone (dayonly (fromGregorian 2016 6 16)) davidschofield (parttimeamount x5)

areasoloBriefing1Meta ::
  BriefingMeta
areasoloBriefing1Meta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing - Area solo prep"]    

areasolo1 ::
  AircraftFlight
areasolo1 =
  noif_commandonlyflight
    "Area Solo"
    vhvvo
    (day 1 x5)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 6 17)))    
    
areasolo1Meta ::
  AircraftFlightMeta
areasolo1Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160617-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160617-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "801102"
        "6rabNGE"
        Nothing
    ]
    []
    [
      Video
        "zF51UQ377E8"
        YouTube  
        Nothing          
        (Just "Forward camera -- Eastern Departure")
    , Video
        "AWLegaBJGFs"
        YouTube
        Nothing            
        (Just "Forward camera -- Practice Forced Landings")
    , Video
        "6320415"
        Bambuser
        Nothing
        (Just "Live stream")
    ]
    [
      flightoneC172AircraftUsageExpense "VH-VVO area solo"
    , vhvvoLanding
    ]

areasoloBriefing2 ::
  Briefing
areasoloBriefing2 =
  Briefing "Area Solo Post Brief" flightone (dayonly (fromGregorian 2016 6 17)) davidschofield (parttimeamount x4)

areasoloBriefing2Meta ::
  BriefingMeta
areasoloBriefing2Meta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing - area solo pre & post brief"]

areasolo2 ::
  AircraftFlight
areasolo2 =
  noif_commandonlyflight
    "Area Solo"
    vhvvo
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 6 26)))    
    
areasolo2Meta ::
  AircraftFlightMeta
areasolo2Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160626-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160626-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "815958"
        "k0zbK26"
        Nothing
    ]
    []
    [
      Video
        "G0-RoTxRsGw"
        YouTube
        Nothing
        (Just "Eastern Arrival, fly-over")
    ]
    [
      flightoneC172AircraftUsageExpense "VH-VVO 2nd area solo"
    , vhvvoLanding
    ]

frolexam ::
  Exam
frolexam =
  dayonlyexam
    "Flight Radiotelephone Operator Licence"
    flightone
    (fromGregorian 2016 6 26)
    davidschofield
    38
    40

frolexamMeta ::
  ExamMeta
frolexamMeta =
  ExamMeta
    [ExamExpense 17600 "Flight Radio Operators Licence", ExamExpense (-17600) "Flight Radio Operators Licence"]

areasoloBriefing3 :: 
  Briefing
areasoloBriefing3 =
  Briefing "Area Solo Post Brief" flightone (dayonly (fromGregorian 2016 6 26)) davidschofield (parttimeamount x4)

areasoloBriefing3Meta ::
  BriefingMeta
areasoloBriefing3Meta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing - Area Solo"]

basicinstrumentflightsim ::
  SimulatorFlight
basicinstrumentflightsim =
  dayonlysimulator
    "Basic Instrument Flight"
    (fromGregorian 2016 7 6)
    "ATD-02"
    [damienboyer]
    (parttimeamount x5)

basicinstrumentflightsimMeta ::
  SimulatorFlightMeta
basicinstrumentflightsimMeta =
  SimulatorFlightMeta
    [SimulatorFlightExpense 26400 "Synthetic Trainer Under Instruction"]

basicinstrumentflightBriefing1 ::
  Briefing
basicinstrumentflightBriefing1 =
  Briefing "Basic Instrument Flight" flightone (dayonly (fromGregorian 2016 7 5)) damienboyer (TimeAmount 1 x0)

basicinstrumentflightBriefing1Meta ::
  BriefingMeta
basicinstrumentflightBriefing1Meta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing - Basic"]

basicinstrumentflight ::
  AircraftFlight
basicinstrumentflight =
  dualonlyflight
    "Basic Instrument Flight"
    vhafr
    davidschofield
    (day 1 x1)
    (directcircuit (runwayatdate "YBAF" "28R" (fromGregorian 2016 7 14)))
    (parttimeamount x8)

basicinstrumentflightMeta ::
  AircraftFlightMeta
basicinstrumentflightMeta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160714-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160714-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "841382"
        "6jr3Rb6"
        Nothing
    ]
    []
    []
    [
      vhafrUnderInstruction
    , vhafrLanding
    ]

basicinstrumentflightBriefing2 ::
  Briefing
basicinstrumentflightBriefing2 =
  Briefing "Basic Instrument Flight Post Brief" flightone (dayonly (fromGregorian 2016 7 14)) davidschofield (parttimeamount x3)

basicinstrumentflightBriefing2Meta ::
  BriefingMeta
basicinstrumentflightBriefing2Meta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing - Basic"]

shortfieldtakeofflanding ::
  AircraftFlight
shortfieldtakeofflanding =
  noif_dualonlyflight
    "Short field take-off and landing"
    vhafr
    ryanmeyles
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 7 22)))

shortfieldtakeofflandingMeta ::
  AircraftFlightMeta
shortfieldtakeofflandingMeta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160722-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160722-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "854963"
        "EpoX79k"
        Nothing
    ]
    []
    [
      Video
        "9o8OM_f5Uxg"
        YouTube
        Nothing
        (Just "Cabin live stream")
    ]
    [
      vhafrUnderInstruction
    , vhafrLanding
    ]

precautionarysearchandlanding ::
  AircraftFlight
precautionarysearchandlanding =
  noif_dualonlyflight
    "Precautionary search and landing"
    vhvvo
    ryanmeyles
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 7 29)))

precautionarysearchandlandingMeta ::
  AircraftFlightMeta
precautionarysearchandlandingMeta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160729-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160729-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "865050"
        "6bWZjWe"
        Nothing
    ]
    []
    [
      Video
        "lBq9InluAPo"
        YouTube
        Nothing
        (Just "Cabin live stream")
    ]
    [
      vhvvoUnderInstruction
    , vhvvoLanding
    ]

shortfieldtakeofflandingBriefing ::
  Briefing
shortfieldtakeofflandingBriefing =
  Briefing "Short field take-off and landing" flightone (dayonly (fromGregorian 2016 7 22)) ryanmeyles (parttimeamount x5)

shortfieldtakeofflandingBriefingMeta ::
  BriefingMeta
shortfieldtakeofflandingBriefingMeta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing - Basic"]

precautionarysearchandlandingBriefing ::
  Briefing
precautionarysearchandlandingBriefing =
  Briefing "Precautionary search and landing" flightone (dayonly (fromGregorian 2016 7 29)) ryanmeyles (parttimeamount x5)

precautionarysearchandlandingBriefingMeta ::
  BriefingMeta
precautionarysearchandlandingBriefingMeta =
  BriefingMeta
    [BriefingExpense 7700 "Briefing - Basic PSL"]

generalrevision ::
  AircraftFlight
generalrevision =
  dualonlyflight
    "General Revision"
    vhafr
    ryanmeyles
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 8 5)))
    (parttimeamount x5)

generalrevisionMeta ::
  AircraftFlightMeta
generalrevisionMeta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160805-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160805-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "876462"
        "kLZ8Kp6"
        Nothing
    ]
    []
    []
    [
      vhafrUnderInstruction
    , vhafrLanding
    ]

generalrevisionBriefing ::
  Briefing
generalrevisionBriefing =
  Briefing "Precautionary search and landing" flightone (dayonly (fromGregorian 2016 8 5)) ryanmeyles (parttimeamount x1)

generalrevisionBriefingMeta ::
  BriefingMeta
generalrevisionBriefingMeta =
  BriefingMeta
    [BriefingExpense 7700 "Approach brief"]

preexamBriefing ::
  Briefing
preexamBriefing =
  Briefing "Pre-exam briefing" flightone (dayonly (fromGregorian 2016 8 5)) davidschofield (parttimeamount x5)

preexamBriefingMeta ::
  BriefingMeta
preexamBriefingMeta =
  BriefingMeta
    [BriefingExpense 7700 "brief pressure & density altitude weight & balance & performance"]

areasoloconsolidation ::
  AircraftFlight
areasoloconsolidation =
  noif_commandonlyflight
    "Area Solo Consolidation"
    vhvvo
    (day 1 x2)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 8 9)))   

areasoloconsolidationMeta ::
  AircraftFlightMeta
areasoloconsolidationMeta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160809-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160809-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "882382"
        "6jnWOa6"
        Nothing
    ]
    []
    [
      Video
        "zKu7-WoWyXo"
        YouTube
        Nothing
        (Just "Cabin live stream 1")
    , Video
        "HMaIC3TJFXI"
        YouTube
        Nothing
        (Just "Cabin live stream 2")
    ]
    [
      vhvvoUnderInstruction
    , vhvvoLanding
    ]

preexamBriefing2 ::
  Briefing
preexamBriefing2 =
  Briefing "Pre-exam briefing 2" flightone (dayonly (fromGregorian 2016 8 11)) davidschofield (TimeAmount 1 x5)

preexamBriefing2Meta ::
  BriefingMeta
preexamBriefing2Meta =
  BriefingMeta
    [BriefingExpense 7700 "RPL theory"]

rplexam ::
  Exam
rplexam =
  dayonlyexam
    "RPLA: Recreational Pilot Licence (Aeroplane)"
    aslExamArcherfield
    (fromGregorian 2016 8 15)
    davidschofield
    79
    100

rplexamMeta ::
  ExamMeta
rplexamMeta =
  ExamMeta
    [ExamExpense 9695 "RPLA: Recreational Pilot Licence (Aeroplane)", ExamExpense 5500 "CASA delivery fee"]

rpltestpreparation ::
  Briefing
rpltestpreparation =
  Briefing "RPLA flight test preparation" flightone (dayonly (fromGregorian 2016 8 16)) davidschofield (TimeAmount 2 x0)

rpltestpreparationMeta ::
  BriefingMeta
rpltestpreparationMeta =
  BriefingMeta
    [BriefingExpense 7700 "RPL test theory Prep"]

rplrecommendation ::
  AircraftFlight
rplrecommendation =
  dualonlyflight
    "RPL(A) Recommendation"
    vhafr
    clintdudman
    (day 1 x3)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 8 18)))
    (parttimeamount x3)

rplrecommendationMeta ::
  AircraftFlightMeta
rplrecommendationMeta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160818-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160818-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "903352"
        "emn77Zk"
        Nothing
    ]
    []
    []
    [
      vhvvoUnderInstruction
    , vhvvoLanding
    ]

areasolo3 ::
  AircraftFlight
areasolo3 =
  noif_commandonlyflight
    "Area Solo"
    vhafr
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 8 22)))    
    
areasolo3Meta ::
  AircraftFlightMeta
areasolo3Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160822-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160822-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "911431"
        ""
        Nothing
    ]
    []
    []
    [
      flightoneC172AircraftUsageExpense "VH-AFR 3rd area solo"
    , vhafrLanding
    ]

logbook1007036 ::
  Logbook AircraftFlightMeta SimulatorFlightMeta ExamMeta BriefingMeta
logbook1007036 =
  aviatorlogbook
    tonymorris
    [
      BriefingEntry preflightBriefing preflightBriefingMeta
    , AircraftFlightEntry effectofcontrols effectofcontrolsMeta
    , AircraftFlightEntry straightandlevel straightandlevelMeta
    , BriefingEntry straightandlevelBriefing straightandlevelBriefingMeta
    , BriefingEntry effectofcontrolsBriefing effectofcontrolsBriefingMeta
    , AircraftFlightEntry climbinganddescending climbinganddescendingMeta
    , BriefingEntry climbinganddescendingBriefing climbinganddescendingBriefingMeta
    , AircraftFlightEntry turning turningMeta
    , BriefingEntry stallingBriefing1 stallingBriefing1Meta
    , AircraftFlightEntry stalling1 stalling1Meta
    , BriefingEntry stallingBriefing2 stallingBriefing2Meta
    , AircraftFlightEntry stalling2 stalling2Meta
    , AircraftFlightEntry circuits1 circuits1Meta
    , AircraftFlightEntry circuits2 circuits2Meta
    , AircraftFlightEntry circuits3 circuits3Meta
    , AircraftFlightEntry circuits4 circuits4Meta
    , BriefingEntry circuitemergenciesBriefing1 circuitemergenciesBriefing1Meta
    , AircraftFlightEntry circuitemergencies1 circuitemergencies1Meta
    , BriefingEntry circuitemergenciesBriefing2 circuitemergenciesBriefing2Meta
    , ExamEntry firstsoloexam firstsoloexamMeta
    , BriefingEntry circuitemergenciesBriefing3 circuitemergenciesBriefing3Meta
    , AircraftFlightEntry circuitemergencies2 circuitemergencies2Meta
    , BriefingEntry circuitemergenciesBriefing4 circuitemergenciesBriefing4Meta
    , ExamEntry areasoloexam areasoloexamMeta
    , BriefingEntry circuitemergenciesBriefing5 circuitemergenciesBriefing5Meta 
    , AircraftFlightEntry circuitemergencies3 circuitemergencies3Meta
    , BriefingEntry circuitemergenciesBriefing6 circuitemergenciesBriefing6Meta 
    , AircraftFlightEntry circuits5 circuits5Meta
    , AircraftFlightEntry firstsolo firstsoloMeta
    , AircraftFlightEntry circuitscrosswind1 circuitscrosswind1Meta
    , BriefingEntry circuitscrosswindBriefing circuitscrosswindBriefingMeta
    , AircraftFlightEntry circuitssolocheck1 circuitssolocheck1Meta
    , AircraftFlightEntry circuitssolo1 circuitssolo1Meta
    , AircraftFlightEntry circuitssolocheck2 circuitssolocheck2Meta
    , AircraftFlightEntry circuitssolo2 circuitssolo2Meta
    , BriefingEntry practiceforcedlandingsBriefing practiceforcedlandingsBriefingMeta
    , AircraftFlightEntry practiceforcedlandings1 practiceforcedlandings1Meta
    , AircraftFlightEntry practiceforcedlandings2 practiceforcedlandings2Meta
    , BriefingEntry steepturnsBriefing steepturnsBriefingMeta
    , AircraftFlightEntry steepturns steepturnsMeta
    , AircraftFlightEntry sydneycircuit sydneycircuitMeta
    , AircraftFlightEntry steepturnsandsideslipping steepturnsandsideslippingMeta
    , AircraftFlightEntry circuitscrosswind2 circuitscrosswind2Meta
    , AircraftFlightEntry areasolocheck areasolocheckMeta
    , BriefingEntry areasoloBriefing1 areasoloBriefing1Meta
    , AircraftFlightEntry areasolo1 areasolo1Meta
    , BriefingEntry areasoloBriefing2 areasoloBriefing2Meta 
    , AircraftFlightEntry areasolo2 areasolo2Meta
    , ExamEntry frolexam frolexamMeta
    , BriefingEntry areasoloBriefing3 areasoloBriefing3Meta
    , BriefingEntry basicinstrumentflightBriefing1 basicinstrumentflightBriefing1Meta
    , SimulatorFlightEntry basicinstrumentflightsim basicinstrumentflightsimMeta
    , AircraftFlightEntry basicinstrumentflight basicinstrumentflightMeta
    , BriefingEntry basicinstrumentflightBriefing2 basicinstrumentflightBriefing2Meta
    , AircraftFlightEntry shortfieldtakeofflanding shortfieldtakeofflandingMeta
    , BriefingEntry shortfieldtakeofflandingBriefing shortfieldtakeofflandingBriefingMeta
    , AircraftFlightEntry precautionarysearchandlanding precautionarysearchandlandingMeta
    , BriefingEntry shortfieldtakeofflandingBriefing shortfieldtakeofflandingBriefingMeta
    , AircraftFlightEntry generalrevision generalrevisionMeta
    , BriefingEntry generalrevisionBriefing generalrevisionBriefingMeta
    , BriefingEntry preexamBriefing preexamBriefingMeta
    , AircraftFlightEntry areasoloconsolidation areasoloconsolidationMeta
    , BriefingEntry preexamBriefing2 preexamBriefing2Meta
    , ExamEntry rplexam rplexamMeta
    , BriefingEntry rpltestpreparation rpltestpreparationMeta
    , AircraftFlightEntry rplrecommendation rplrecommendationMeta
    , AircraftFlightEntry areasolo3 areasolo3Meta
    ]

---- Reports

data FlightTimeReport =
  FlightTimeReport {
    _hoursTotal ::
      TimeAmount -- Hours total
  , _hoursTotalICUS ::
      TimeAmount --   Hours total in-command under-instruction
  , _hoursTotalDual ::
      TimeAmount --   Hours total dual under-instruction
  , _hoursTotalInCommand ::
      TimeAmount --   Hours total in-command
    -- Hours in aircraft type
    --   Hours in aircraft type in-command under-instruction
    --   Hours in aircraft type dual under-instruction
    --   Hours in aircraft type in-command
  , _hoursInAircraftType ::
      Map String (TimeAmount, TimeAmount, TimeAmount, TimeAmount)
    -- Hours in aircraft registration
    --   Hours in aircraft registration in-command under-instruction
    --   Hours in aircraft registration dual under-instruction
    --   Hours in aircraft registration in-command
  , _hoursInAircraftRegistration ::
      Map String (TimeAmount, TimeAmount, TimeAmount, TimeAmount)
  , _hoursSingleEngine ::
      TimeAmount -- Hours in single-engine
  , _hoursSingleEngineICUS ::
      TimeAmount --   Hours in single-engine in-command under-instruction
  , _hoursSingleEngineDual :: 
      TimeAmount --   Hours in single-engine dual under-instruction
  , _hoursSingleEngineInCommand ::
      TimeAmount --   Hours in single-engine in-command
  , _hoursMultiEngine ::
      TimeAmount -- Hours in multi-engine
  , _hoursMultiEngineICUS ::
      TimeAmount --   Hours multi-engine in-command under-instruction
  , _hoursMultiEngineDual ::
      TimeAmount --   Hours multi-engine dual under-instruction
  , _hoursMultiEngineInCommand ::
      TimeAmount --   Hours multi-engine in-command
  , _hoursDay ::
      TimeAmount -- Hours during day
  , _hoursDayICUS ::
      TimeAmount --   Hours during day in-command under-instruction
  , _hoursDayDual ::
      TimeAmount --   Hours during day dual under-instruction
  , _hoursDayInCommand ::
      TimeAmount --   Hours during day in-command
  , _hoursNight ::
      TimeAmount -- Hours during night
  , _hoursNightICUS ::
      TimeAmount --   Hours during night in-command under-instruction
  , _hoursNightDual ::
      TimeAmount --   Hours during night dual under-instruction
  , _hoursNightInCommand ::
      TimeAmount --   Hours during night in-command
  , _hoursWithPiC ::
      Map Aviator TimeAmount -- Hours with PiC
  , _hoursInstrument ::
      TimeAmount -- Hours instrument in-flight
  } deriving (Eq, Ord, Show)

makeClassy ''FlightTimeReport

instance Monoid FlightTimeReport where
  mempty =
    FlightTimeReport
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
  FlightTimeReport tl1 tli1 tld1 tlc1 tp1 rg1 se1 sei1 sed1 sec1 me1 mei1 med1 mec1 dy1 dyi1 dyd1 dyc1 nt1 nti1 ntd1 ntc1 wpc1 is1 `mappend` FlightTimeReport tl2 tli2 tld2 tlc2 tp2 rg2 se2 sei2 sed2 sec2 me2 mei2 med2 mec2 dy2 dyi2 dyd2 dyc2 nt2 nti2 ntd2 ntc2 wpc2 is2 =
    FlightTimeReport
      (tl1 `mappend` tl2)
      (tli1 `mappend` tli2)
      (tld1 `mappend` tld2)
      (tlc1 `mappend` tlc2)
      (Map.unionWith mappend tp1 tp2)
      (Map.unionWith mappend rg1 rg2)
      (se1 `mappend` se2)
      (sei1 `mappend` sei2)
      (sed1 `mappend` sed2)
      (sec1 `mappend` sec2)
      (me1 `mappend` me2)
      (mei1 `mappend` mei2)
      (med1 `mappend` med2)
      (mec1 `mappend` mec2)
      (dy1 `mappend` dy2)
      (dyi1 `mappend` dyi2)
      (dyd1 `mappend` dyd2)
      (dyc1 `mappend` dyc2)
      (nt1 `mappend` nt2)
      (nti1 `mappend` nti2)
      (ntd1 `mappend` ntd2)
      (ntc1 `mappend` ntc2)
      (Map.unionWith mappend wpc1 wpc2)
      (is1 `mappend` is2)

singleFlightTimeReport ::
  Entry a b c d
  -> FlightTimeReport
singleFlightTimeReport (AircraftFlightEntry fl _) =
  let hoursdaynight = totalDayNight (fl ^. daynight)
      icus x =
        case fl ^. command of
          ICUS _ ->
            x
          Dual _ ->
            mempty
          InCommand ->
            mempty
      dual x =
        case fl ^. command of
          ICUS _ ->
            mempty
          Dual _ ->
            x
          InCommand ->
            mempty            
      comd x =
        case fl ^. command of
          ICUS _ ->
            mempty
          Dual _ ->
            mempty
          InCommand ->
            x
      hoursmap k =
        Map.singleton k (hoursdaynight, (icus hoursdaynight), (dual hoursdaynight), (comd hoursdaynight))
      singleengine x =
        case fl ^. flightaircraft . aircraftEngine of
          Single ->
            x
          Multi ->
            mempty
      multiengine x =
        case fl ^. flightaircraft . aircraftEngine of
          Single ->
            mempty
          Multi ->
            x
      totalhoursday =
        fl ^. daynight . dayDayNight
      totalhoursnight =
        fl ^. daynight . nightDayNight
      pic x =
        case getInstructingPic (fl ^. command) of
          Just a ->
            Map.singleton a x
          Nothing ->
            Map.empty
  in  FlightTimeReport
        hoursdaynight
        (icus hoursdaynight)
        (dual hoursdaynight)
        (comd hoursdaynight)
        (hoursmap (fl ^. flightaircraft . aircraftType))
        (hoursmap (fl ^. flightaircraft . aircraftRegistration))
        (singleengine hoursdaynight)
        (singleengine (icus hoursdaynight))
        (singleengine (dual hoursdaynight))
        (singleengine (comd hoursdaynight))
        (multiengine hoursdaynight)
        (multiengine (icus hoursdaynight))
        (multiengine (dual hoursdaynight))
        (multiengine (comd hoursdaynight))
        totalhoursday
        (icus totalhoursday)
        (dual totalhoursday)
        (comd totalhoursday)
        totalhoursnight
        (icus totalhoursnight)
        (dual totalhoursnight)
        (comd totalhoursnight)
        (pic hoursdaynight)
        (fl ^. instrumentflightTime)
singleFlightTimeReport _ =
  mempty

getFlightTimeReport ::
  Logbook a b c d
  -> FlightTimeReport
getFlightTimeReport (Logbook _ (Entries es)) =
  foldl' (\a -> mappend a . singleFlightTimeReport) mempty es

htmlFlightTimeReport ::
  Logbook a b c d
  -> FlightTimeReport
  -> Html ()
htmlFlightTimeReport _ r =
  div_ [class_ "flighttimereport"] $
    do  h3_ [class_ "flighttimereportname"] "Flight Time Summary Report"          
        ul_ [] $
          do  li_ [] $
                do  span_ [class_ "key"] "Total Flight Hours: "
                    span_ [class_ "value"] .
                      htmlTimeAmount $ r ^. hoursTotal
                    ul_ [] $
                      do  li_ [] $
                            do  span_ [class_ "key"] "in-command under-instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursTotalICUS
                          li_ [] $
                            do  span_ [class_ "key"] "dual under-instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursTotalDual
                          li_ [] $
                            do  span_ [class_ "key"] "in-command: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursTotalInCommand
              li_ [] $
                do  span_ [class_ "key"] "Hours in type: "
                    span_ [class_ "value"] .
                      ul_ [] . Map.foldrWithKey (\y (tl, iu, dl, ic) x ->
                        li_ [] $
                          do  span_ [class_ "aircrafttype"] $ fromString y
                              ul_ [] $
                                do  li_ [] $
                                      do  span_ [class_ "key"] "total: "
                                          span_ [class_ "value"] . htmlTimeAmount $ tl
                                    li_ [] $
                                      do  span_ [class_ "key"] "in-command under-instruction: "
                                          span_ [class_ "value"] . htmlTimeAmount $ iu
                                    li_ [] $
                                      do  span_ [class_ "key"] "dual under-instruction: "
                                          span_ [class_ "value"] . htmlTimeAmount $ dl
                                    li_ [] $
                                      do  span_ [class_ "key"] "in-command: "
                                          span_ [class_ "value"] . htmlTimeAmount $ ic
                              x) mempty $ r ^. hoursInAircraftType
              li_ [] $
                do  span_ [class_ "key"] "Hours in registration: "
                    span_ [class_ "value"] .
                      ul_ [] . Map.foldrWithKey (\y (tl, iu, dl, ic) x ->
                        li_ [] $
                          do  span_ [class_ "aircraftregistration"] $ fromString y
                              ul_ [] $
                                do  li_ [] $
                                      do  span_ [class_ "key"] "total: "
                                          span_ [class_ "value"] . htmlTimeAmount $ tl
                                    li_ [] $
                                      do  span_ [class_ "key"] "in-command under-instruction: "
                                          span_ [class_ "value"] . htmlTimeAmount $ iu
                                    li_ [] $
                                      do  span_ [class_ "key"] "dual under-instruction: "
                                          span_ [class_ "value"] . htmlTimeAmount $ dl
                                    li_ [] $
                                      do  span_ [class_ "key"] "in-command: "
                                          span_ [class_ "value"] . htmlTimeAmount $ ic
                              x) mempty $ r ^. hoursInAircraftRegistration
              li_ [] $
                do  span_ [class_ "key"] "Hours in Single-Engine: "
                    span_ [class_ "value"] .
                      htmlTimeAmount $ r ^. hoursSingleEngine
                    ul_ [] $
                      do  li_ [] $
                            do  span_ [class_ "key"] "in-command under-instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursSingleEngineICUS
                          li_ [] $
                            do  span_ [class_ "key"] "dual under-instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursSingleEngineDual
                          li_ [] $
                            do  span_ [class_ "key"] "in-command: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursSingleEngineInCommand
              li_ [] $
                do  span_ [class_ "key"] "Hours in Multi-Engine: "
                    span_ [class_ "value"] .
                      htmlTimeAmount $ r ^. hoursMultiEngine
                    ul_ [] $
                      do  li_ [] $
                            do  span_ [class_ "key"] "in-command under-instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursMultiEngineICUS
                          li_ [] $
                            do  span_ [class_ "key"] "dual under-instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursMultiEngineDual
                          li_ [] $
                            do  span_ [class_ "key"] "in-command: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursMultiEngineInCommand
              li_ [] $
                do  span_ [class_ "key"] "Hours in Day: "
                    span_ [class_ "value"] .
                      htmlTimeAmount $ r ^. hoursDay
                    ul_ [] $
                      do  li_ [] $
                            do  span_ [class_ "key"] "in-command under-instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursDayICUS
                          li_ [] $
                            do  span_ [class_ "key"] "dual under-instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursDayDual
                          li_ [] $
                            do  span_ [class_ "key"] "in-command: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursDayInCommand
              li_ [] $
                do  span_ [class_ "key"] "Hours in Night: "
                    span_ [class_ "value"] .
                      htmlTimeAmount $ r ^. hoursNight
                    ul_ [] $
                      do  li_ [] $
                            do  span_ [class_ "key"] "in-command under-instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursNightICUS
                          li_ [] $
                            do  span_ [class_ "key"] "dual under-instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursNightDual
                          li_ [] $
                            do  span_ [class_ "key"] "in-command: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursNightInCommand
              li_ [] $
                do  span_ [class_ "key"] "Hours with PiC: "
                    span_ [class_ "value"] .
                      ul_ [] . Map.foldrWithKey (\a t x ->
                        li_ [] $
                          do  span_ [class_ "key"] $ 
                                do  htmlAviatorShort a
                                    ": "
                              span_ [class_ "value"] . htmlTimeAmount $ t                                      
                              x) mempty $ r ^. hoursWithPiC
              li_ [] $
                do  span_ [class_ "key"] "Hours instrument in-flight: "
                    span_ [class_ "value"] .
                      htmlTimeAmount $ r ^. hoursInstrument                        

---- Html (meta)

htmlAircraftUsageExpense ::
  AircraftFlight
  -> AircraftUsageExpense
  -> Html ()
htmlAircraftUsageExpense fl (AircraftUsageExpense perhour name) =
  let z = totalDayNight (fl ^. daynight)
  in  span_ [class_ "aircraftusageexpense"] $
        do  span_ [class_ "aircraftusageexpensecost"] . fromString . ('$':) . showThousandCentsAsDollars $ timeAmountBy10 z * perhour
            span_ [class_ "aircraftusageexpensephrase"] " at "
            span_ [class_ "aircraftusageexpenseperhour"] . fromString . ('$':) . showCentsAsDollars $ perhour
            span_ [class_ "aircraftusageexpensephrase"] " per hour"
            when (not . null $ name) . span_ [class_ "aircraftusageexpensename"] $
              do  " ("
                  fromString name
                  ")"

htmlAircraftLandingExpense ::
  AircraftFlight
  -> AircraftLandingExpense
  -> Html ()
htmlAircraftLandingExpense _ (AircraftLandingExpense amount name) =
  span_ [class_ "aircraftlandingexpense"] $
    do  span_ [class_ "aircraftlandingexpensecost"] . fromString . ('$':) . showThousandCentsAsDollars $ (amount * 10)
        when (not . null $ name) . span_ [class_ "aircraftlandingexpensename"] $
          do  " ("
              fromString name
              ")"

htmlAircraftFlightExpense ::
  AircraftFlight
  -> AircraftFlightExpense
  -> Html ()
htmlAircraftFlightExpense fl (ExpenseAircraftUsage e) =
  htmlAircraftUsageExpense fl e
htmlAircraftFlightExpense fl (ExpenseAircraftLanding e) =
  htmlAircraftLandingExpense fl e

htmlSimulatorFlightExpense ::
  SimulatorFlight
  -> SimulatorFlightExpense
  -> Html ()
htmlSimulatorFlightExpense sf (SimulatorFlightExpense perhour name) =
  let z = sf ^. instrumentsimulatorTimeAmount
  in  span_ [class_ "simulatorflightexpense"] $
        do  span_ [class_ "simulatorflightcost"] . fromString . ('$':) . showThousandCentsAsDollars $ timeAmountBy10 z * perhour
            span_ [class_ "simulatorflightexpensephrase"] " at "
            span_ [class_ "simulatorflightexpenseperhour"] . fromString . ('$':) . showCentsAsDollars $ perhour
            span_ [class_ "simulatorflightexpensephrase"] " per hour"
            when (not . null $ name) . span_ [class_ "simulatorflightexpensename"] $
              do  " ("
                  fromString name
                  ")"

htmlExamExpense ::
  Exam
  -> ExamExpense
  -> Html ()
htmlExamExpense _ (ExamExpense amount name) =
  span_ [class_ "examexpense"] $
    do  span_ [class_ "examexpensecost"] . fromString . ('$':) . showThousandCentsAsDollars $ (amount * 10)
        when (not . null $ name) . span_ [class_ "examexpensename"] $
          do  " ("
              fromString name
              ")"

htmlBriefingExpense ::
  Briefing
  -> BriefingExpense
  -> Html ()
htmlBriefingExpense br (BriefingExpense perhour name) =
  let z = br ^. briefingTimeAmount
  in  span_ [class_ "briefingexpense"] $
        do  span_ [class_ "briefingexpensecost"] . fromString . ('$':) . showThousandCentsAsDollars $ timeAmountBy10 z * perhour
            span_ [class_ "briefingexpensephrase"] " at "
            span_ [class_ "briefingexpenseperhour"] . fromString . ('$':) . showCentsAsDollars $ perhour
            span_ [class_ "briefingexpensephrase"] " per hour"
            when (not . null $ name) . span_ [class_ "briefingexpensename"] $
              do  " ("
                  fromString name
                  ")"

htmlVisualisation ::
  AircraftFlight
  -> Visualisation
  -> Html ()
htmlVisualisation _ (Doarama i _ n) =
  let n' = fromMaybe "doarama.com" n
  in  do  a_ [href_ ("http://doarama.com/view/" <> Text.pack i)] $ 
            span_ [class_ "Visualisation_name"] (fromString n')
          -- p_ (iframe_ [src_ ("http://www.doarama.com/embed?k=" <> Text.pack e), width_ "560", height_ "315", termWith -- "allowfullscreen" [] "allowfullscreen"] "")

strImageType ::
  ImageType
  -> String
strImageType Jpg =
  "jpg"
strImageType Png =
  "png"
strImageType Gif =
  "gif"

htmlImageSource ::
  AircraftFlight
  -> Maybe String
  -> Html ()
htmlImageSource _ =
  maybe mempty (\s' -> span_ [] (fromString ("Image source: " ++ s')))

htmlImage ::
  AircraftFlight
  -> Image
  -> Html ()
htmlImage fl (Image u t s n) =
  let u' = fromString u      
      n' = fromMaybe ("Image (" ++ strImageType t ++ ")") n
  in  do  a_ [href_ u'] $
            img_ [src_ u', width_ "120", alt_ (Text.pack n')]
          htmlImageSource fl s

strTrackLogType ::
  TrackLogType
  -> String
strTrackLogType Gpx =
  "gpx"
strTrackLogType Kml =
  "kml"
strTrackLogType Kmz =
  "kmz"
strTrackLogType (ImageTrackLog i) =
  strImageType i

htmlTrackLogSource ::
  AircraftFlight
  -> Maybe String
  -> Html ()
htmlTrackLogSource _ =
  maybe "" (\q -> span_ [] (fromString (" from " ++ q)))

htmlTrackLog ::
  AircraftFlight
  -> TrackLog
  -> Html ()
htmlTrackLog fl (TrackLog u t s n) =
  let u' = fromString u
      n' = fromMaybe (strTrackLogType t) n
      o = do  fromString n'
              htmlTrackLogSource fl s
  in  do  a_ [href_ u'] o
          case t of 
            ImageTrackLog _ ->
              do  br_ []
                  a_ [href_ u'] $
                    img_ [src_ u', width_ "360", alt_ (fromString n')]
            _ ->
              mempty

strVideoType ::
  VideoType
  -> String
strVideoType YouTube =
  "youtube"
strVideoType Vimeo =
  "vimeo"
strVideoType Bambuser =
  "bambuser"

htmlVideoSource ::
  AircraftFlight
  -> Maybe String
  -> Html ()
htmlVideoSource _ s =
  maybe mempty (\q -> span_ [] (fromString (" from " ++ q))) s

htmlVideo ::
  AircraftFlight
  -> Video
  -> Html ()
htmlVideo fl (Video u t s n) =
  let n' = fromMaybe ("Video (" ++ strVideoType t ++ ")") n
  in  do  a_ [href_ (fromString (linkVideoType t u))] (fromString n')
          htmlVideoSource fl s

htmlTrackLogs ::
  AircraftFlight
  -> [TrackLog]
  -> Html ()
htmlTrackLogs fl x =
  whenEmpty (\q -> div_ [class_ "tracklogs"] $
    do  span_ [class_ "tracklogsheader"] "Track Logs"
        ul_ [] $
          mapM_ (li_ [class_ "tracklog"] . htmlTrackLog fl) q) x

htmlVisualisations ::
  AircraftFlight
  -> [Visualisation]
  -> Html ()
htmlVisualisations fl x =
  whenEmpty (\q -> div_ [class_ "visualisations"] $
    do  span_ [class_ "visualisationsheader"] "Visualisations"
        ul_ [] $
          mapM_ (li_ [class_ "visualisation"] . htmlVisualisation fl) q) x

htmlImages ::
  AircraftFlight
  -> [Image]
  -> Html ()
htmlImages fl x =
  whenEmpty (\q -> div_ [class_ "tracklogs"] $
    do  span_ [class_ "imagesheader"] "Images"
        div_ [style_ "text-align: justify"] $ 
          mapM_ (htmlImage fl) q) x

htmlVideos ::
  AircraftFlight
  -> [Video]
  -> Html ()
htmlVideos fl x =
  whenEmpty (\q -> div_ [class_ "videos"] $
    do  span_ [class_ "videosheader"] "Videos"
        ul_ [] $
          mapM_ (li_ [class_ "video"] . htmlVideo fl) q) x

htmlAircraftFlightExpenses ::
  AircraftFlight
  -> [AircraftFlightExpense]
  -> Html ()
htmlAircraftFlightExpenses fl x =
  whenEmpty (\q -> div_ [class_ "aircraftflightexpenses"] $
    do  span_ [class_ "aircraftflightexpensesheader"] "Aircraft Flight Expenses"
        ul_ [] $
          mapM_ (li_ [class_ "aircraftflightexpense"] . htmlAircraftFlightExpense fl) q) x

htmlAircraftFlightMeta ::
  AircraftFlight
  -> AircraftFlightMeta
  -> Html ()
htmlAircraftFlightMeta fl (AircraftFlightMeta tls vls ims vds exs) =
  div_ $ 
    do  htmlTrackLogs fl tls
        htmlVisualisations fl vls
        htmlImages fl ims
        htmlVideos fl vds
        htmlAircraftFlightExpenses fl exs

htmlSimulatorFlightMeta ::
  SimulatorFlight
  -> SimulatorFlightMeta
  -> Html ()
htmlSimulatorFlightMeta fl (SimulatorFlightMeta s) =
  whenEmpty (\q -> div_ [class_ "simulatormeta"] $
    do  span_ [class_ "simulatorheader"] "Expenses"
        ul_ [] $
          mapM_ (li_ [class_ "expense"] . htmlSimulatorFlightExpense fl) q) s  

htmlExamMeta ::
  Exam
  -> ExamMeta
  -> Html ()
htmlExamMeta e (ExamMeta s) =
  whenEmpty (\q -> div_ [class_ "exammeta"] $
    do  span_ [class_ "exammetaheader"] "Expenses"
        ul_ [] $
          mapM_ (li_ [class_ "expense"] . htmlExamExpense e) q) s

htmlBriefingMeta ::
  Briefing
  -> BriefingMeta
  -> Html ()
htmlBriefingMeta b (BriefingMeta s) =
  whenEmpty (\q -> div_ [class_ "briefingmeta"] $
    do  span_ [class_ "briefingmetaheader"] "Expenses"
        ul_ [] $
          mapM_ (li_ [class_ "expense"] . htmlBriefingExpense b) q) s

---- Html (no meta)

strEngine ::
  Engine
  -> String
strEngine Single =
  "single-engine"
strEngine Multi =
  "multi-engine"

htmlAircraft ::
  AircraftFlight
  -> Aircraft
  -> Html ()
htmlAircraft _ (Aircraft t r e) =
  span_ [class_ "aircraft"] $
    do  span_ [class_ "aircrafttype"] (fromString t)
        " "
        span_ [class_ "aircraftregistration"] (fromString r)
        " "
        span_ [class_ "aircraftengine"] (fromString (strEngine e))

htmlRatingDay ::
  Maybe Day
  -> Html ()
htmlRatingDay =
  maybe mempty (\q -> 
    do  " "
        span_ [] $
          fromString (show q))

htmlRating ::
  Rating
  -> Html ()
htmlRating (Rating n d) =
  span_ [] $
    do  span_ [] (fromString n)
        htmlRatingDay d

htmlRatingShort ::
  Rating
  -> Html ()
htmlRatingShort (Rating n _) =
  span_ [] (fromString n)

htmlRatings ::
  [Rating]
  -> Html ()
htmlRatings =
  sequence_ . intersperse ", " . map htmlRating

htmlRatingsShort ::
  [Rating]
  -> Html ()
htmlRatingsShort =
  sequence_ . intersperse ", " . map htmlRatingShort

htmlAviatorName ::
  String
  -> String
  -> Html ()
htmlAviatorName s f =
  do  li_ [id_ "aviatorname"] $
        do  span_ [class_ "key"] "Name: "
            span_ [class_ "value"] $
              do  fromString (map toUpper s)
                  ", "
                  fromString f

htmlAviatorARN ::
  [Digit]
  -> Html ()
htmlAviatorARN a =
  when (not . null $ a) $
    do  li_ [id_ "aviatorarn"] $ 
          do  span_ [class_ "key"] "ARN: "
              span_ [class_ "value"] $
                fromString (a >>= show)

htmlAviatorDob ::
  Maybe Day
  -> Html ()
htmlAviatorDob =
  maybe mempty (\q ->
    do  li_ [id_ "aviatordob"] $
          do  span_ [class_ "key"] "Date of Birth: "
              span_ [class_ "value"] .
                fromString . show $ q)

htmlAviatorRatings ::
  [Rating]
  -> Html ()
htmlAviatorRatings r =
  when (not . null $ r) $
    do  li_ [id_ "aviatorratings"] $ 
          do  span_ [class_ "key"] "Ratings: "
              span_ [class_ "value"] .
                htmlRatings $ r

htmlAviator ::
  Aviator
  -> Html ()
htmlAviator (Aviator s f a d r) =
  div_ [id_ "aviator", class_ "aviator"] .
    ul_ [] $
      do  htmlAviatorName s f
          htmlAviatorARN a
          htmlAviatorDob d
          htmlAviatorRatings r
            
htmlAviatorShort ::
  Aviator
  -> Html ()    
htmlAviatorShort (Aviator s f a _ r) =
  do  fromString f
      " "
      fromString s
      when (not . null $ a) $ " "
      fromString (a >>= show)
      when (not . null $ r) $ " "
      htmlRatingsShort r

htmlFlightPoint ::
  AircraftFlight
  -> FlightPoint
  -> Html ()
htmlFlightPoint _ (FlightPoint p _ _) =
  span_ [class_ "flightpoint"] $
    fromString p

htmlFlightPath ::
  AircraftFlight
  -> FlightPath
  -> Html ()
htmlFlightPath fl p =
  span_ [class_ "flightpath"] $
    fold (intersperse (toHtmlRaw (" &mdash; " :: Text.Text)) (htmlFlightPoint fl <$> flightPathList p))
    
htmlCommand ::
  AircraftFlight
  -> Command
  -> Html ()
htmlCommand _ InCommand =
  span_ [class_ "command incommand"] "In-Command"
htmlCommand _ (ICUS a) =
  do  span_ [class_ "command incommandunderinstruction"] "In-Command Under-Instruction"
      span_ [class_ "commandphrase"] $ " by "
      span_ [class_ "commandaviator"] $ htmlAviatorShort a
htmlCommand _ (Dual a) =
  do  span_ [class_ "command dualunderinstruction"] "Dual Under-Instruction"
      span_ [class_ "commandphrase"] $ " by "
      span_ [class_ "commandaviator"] $ htmlAviatorShort a

strTimeAmount ::
  TimeAmount
  -> String
strTimeAmount (TimeAmount h x) =
  show h ++ "." ++ show x

htmlTimeAmount ::
  TimeAmount
  -> Html ()
htmlTimeAmount t =
  span_ [] $
    do  fromString (strTimeAmount t)
        "hrs"

htmlTimeAmountZero ::
  TimeAmount
  -> Html ()
htmlTimeAmountZero =
  htmlTimeAmountZeroWith id

htmlTimeAmountZeroWith :: 
  Monoid a =>
  (Html () -> a)
  -> TimeAmount
  -> a
htmlTimeAmountZeroWith f z =
  if z == zerotimeamount
    then
      mempty
    else
      f (htmlTimeAmount z)

htmlAviators ::
  [Aviator]
  -> Html ()
htmlAviators =
  ul_ [] .
    mapM_ (li_ [] . htmlAviatorShort)

htmlAircraftFlightName ::
  String
  -> Html ()
htmlAircraftFlightName n =
  h3_ [class_ "aircraftflightname"] $
          fromString n

htmlAircraftFlight ::
  AircraftFlight
  -> Html ()
htmlAircraftFlight fl@(AircraftFlight n a c (DayNight d m) p o i) =
  div_ [class_ "aircraftflight"] $
    do  htmlAircraftFlightName n
        ul_ [] $
          do  li_ [] $
                do  span_ [class_ "key"] "Time: "
                    span_ [class_ "value"] .
                     htmlFlightPathTime $ p
              li_ [] $
                do  span_ [class_ "key"] "Aircraft: "
                    span_ [class_ "value"] .
                     htmlAircraft fl $ a
              li_ [] $
                do  span_ [class_ "key"] "Command: "
                    span_ [class_ "value"] .
                     htmlCommand fl $ c
              htmlTimeAmountZeroWith (\t ->
                li_ [] $
                do  span_ [class_ "key"] "Amount (day): "
                    span_ [class_ "value"] t) d
              htmlTimeAmountZeroWith (\t ->
                li_ [] $
                do  span_ [class_ "key"] "Amount (night): "
                    span_ [class_ "value"] t) m
              htmlTimeAmountZeroWith (\t ->
                li_ [] $
                do  span_ [class_ "key"] "Amount (instrument): "
                    span_ [class_ "value"] t) i
              li_ [] $
                do  span_ [class_ "key"] "Flight Path: "
                    span_ [class_ "value"] .
                      htmlFlightPath fl $ p
              when (not . null $ o) . li_ [] $
                do  span_ [class_ "key"] "Other Crew: "
                    span_ [class_ "value"] .
                      htmlAviators $ o

htmlTimeOfDayTime ::
  Maybe TimeOfDay
  -> Html ()
htmlTimeOfDayTime =
  maybe mempty (\e -> do  " "
                          fromString (show e))

htmlTime ::
  Time
  -> Html ()
htmlTime (Time t d) =
  span_ [class_ "time"] $ 
    do  fromString (show t)
        htmlTimeOfDayTime d

htmlFlightPathTime ::
  FlightPath
  -> Html()
htmlFlightPathTime p =
  let s = p ^. flightStart . landingTime
      e = p ^. flightEnd . landingTime
  in  if s == e
        then
          htmlTime s
        else
          do  htmlTime s
              toHtmlRaw (" &mdash; " :: Text.Text)
              htmlTime e

htmlSimulatorFlightName ::
  String
  -> Html ()
htmlSimulatorFlightName n =
  h3_ [class_ "simulatorflightname"] $
          fromString n

htmlSimulatorFlight ::
  SimulatorFlight
  -> Html ()
htmlSimulatorFlight (SimulatorFlight n t y o i) =
  div_ [class_ "simulatorflight"] $
    do  htmlSimulatorFlightName n
        ul_ [] $
          do  li_ [] $
                do  span_ [class_ "key"] "Time: "
                    span_ [class_ "value"] .
                     htmlTime $ t
              li_ [] $
                do  span_ [class_ "key"] "Type: "
                    span_ [class_ "value"] (fromString y)
              when (not . null $ o) . li_ [] $
                do  span_ [class_ "key"] "Other Crew: "
                    div_ [class_ "value"] .
                      htmlAviators $ o
              li_ [] $
                do  span_ [class_ "key"] "Amount: "
                    span_ [class_ "value"] .
                     htmlTimeAmount $ i
  
htmlLocation ::
  Location
  -> Html ()
htmlLocation (Location n t o) =  
  span_ [class_ "location"] $ 
    do  fromString n
        " "
        let t' = fromString (show t)
            o' = fromString (show o)
        span_ [class_ "locationopenstreetmap"] $
          a_ [href_ ("http://www.openstreetmap.org/?mlat=" <> t' <> "&mlon=" <> o' <> "#map=16/" <> t' <> "/" <> o')] 
          "osm"
        " "
        span_ [class_ "locationgooglemaps"] $
          a_ [href_ ("https://www.google.com/maps/?q=" <> t' <> "," <> o')]
          "gmap"
        
htmlExamResult ::
  Int
  -> Int
  -> Html ()
htmlExamResult x y =
  do  fromString (show x)
      "/"
      fromString (show y)

htmlExamName ::
  String
  -> Html ()
htmlExamName n =
  h3_ [class_ "examname"] $ 
    fromString n

htmlExam ::
  Exam
  -> Html ()
htmlExam (Exam n l t a r m) =
  let r' = do  span_ [class_ "examresult"] . fromString . show $ r
               span_ [class_ "examresultoutof"] "/"
               span_ [class_ "examresultmaximum"] . fromString . show $ m
               " ("
               span_ [class_ "examresultpercentage"] . fromString . printf "%.2f" $ (100 * fromIntegral r / fromIntegral m :: Double)
               span_ [class_ "examresultpercentsign"] "%"
               ")"
  in  div_ [class_ "exam"] $
        do  htmlExamName n
            ul_ [] $
              do  li_ [] $
                    do  span_ [class_ "key"] "Time: "
                        span_ [class_ "value"] .
                          htmlTime $ t
                  li_ [] $
                    do  span_ [class_ "key"] "Location: "
                        span_ [class_ "value"] .
                          htmlLocation $ l
                  li_ [] $
                    do  span_ [class_ "key"] "Delegate: "
                        span_ [class_ "value"] .
                          htmlAviatorShort $ a
                  li_ [] $
                    do  span_ [class_ "key"] "Result: "
                        span_ [class_ "value"] r'

htmlBriefingName ::
  String
  -> Html ()
htmlBriefingName n =
  h3_ [class_ "briefingname"] $ 
    fromString n

htmlBriefing ::
  Briefing
  -> Html ()
htmlBriefing (Briefing n l t a m) =
  div_ [class_ "briefing"] $
    do  htmlBriefingName n
        ul_ [] $
          do  li_ [] $
                do  span_ [class_ "key"] "Time: "
                    span_ [class_ "value"] .
                      htmlTime $ t
              li_ [] $
                do  span_ [class_ "key"] "Location: "
                    span_ [class_ "value"] .
                      htmlLocation $ l
              li_ [] $
                do  span_ [class_ "key"] "Amount: "
                    span_ [class_ "value"] .
                      htmlTimeAmountZero $ m
              li_ [] $
                do  span_ [class_ "key"] "Briefer: "
                    span_ [class_ "value"] .
                      htmlAviatorShort $ a

space2dot ::
  String
  -> String
space2dot =
  map $ \c -> case c of 
                ' ' -> '.'
                _   -> c

htmlEntryTag ::
  Entry a b c d
  -> Html ()
htmlEntryTag (AircraftFlightEntry e _) =
  let lk = space2dot . concat $
                          [
                            "FLT_"
                          , e ^. aircraftflightname
                          , "_"
                          , e ^. flightaircraft . aircraftRegistration
                          , "_"
                          , e ^. flightpath . flightStart . point
                          , "-"
                          , e ^. flightpath . flightEnd . point
                          ]
  in  a_ [href_ (Text.pack ('#' : lk))] . span_ [class_ "entrytag"] $ "FLT"
htmlEntryTag (SimulatorFlightEntry e _) =
  let lk = space2dot . concat $
                          [
                            "SIM_"
                          , e ^. simulatorflightname
                          , "_"
                          , e ^. simulatortype
                          ]
  in  a_ [href_ (Text.pack ('#' : lk))] . span_ [class_ "entrytag"] $ "SIM"
htmlEntryTag (ExamEntry e _) =
  let lk = space2dot . concat $
                          [
                            "EXM_"
                          , e ^. examName
                          , "_"
                          , show (e ^. examTime . daytime)
                          ]
  in  a_ [href_ (Text.pack ('#' : lk))] . span_ [class_ "entrytag"] $ "EXM"
htmlEntryTag (BriefingEntry e _) =
  let lk = space2dot . concat $
                          [
                            "BRF_"
                          , e ^. briefingName
                          , "_"
                          , show (e ^. briefingTime . daytime)
                          ]
  in  a_ [href_ (Text.pack ('#' : lk))] . span_ [class_ "entrytag"] $ "BRF"

htmlEntry :: 
  (AircraftFlight -> a -> Html x)
  -> (SimulatorFlight -> b -> Html x)
  -> (Exam -> c -> Html x)
  -> (Briefing -> d -> Html x)
  -> Entry a b c d
  -> Html x
htmlEntry aircraftFlightMeta' simulatorFlightMeta' examMeta' briefingMeta' x =
  do  htmlEntryTag x
      case x of
        AircraftFlightEntry e ae ->
          do  div_ [] $
                do  htmlAircraftFlight e
                    aircraftFlightMeta' e ae
        SimulatorFlightEntry e ae ->
          do  div_ [] $
                do  htmlSimulatorFlight e
                    simulatorFlightMeta' e ae
        ExamEntry e ae ->
          do  div_ [] $
                do  htmlExam e
                    examMeta' e ae
        BriefingEntry e ae ->
          do  div_ [] $
                do  htmlBriefing e
                    briefingMeta' e ae
  
htmlEntries :: 
  (AircraftFlight -> a -> Html x)
  -> (SimulatorFlight -> b -> Html x)
  -> (Exam -> c -> Html x)
  -> (Briefing -> d -> Html x)
  -> Entries a b c d
  -> Html ()
htmlEntries aircraftFlightMeta' simulatorFlightMeta' examMeta' briefingMeta' (Entries es) =
  mapM_ (\e -> hr_ [] *> htmlEntry aircraftFlightMeta' simulatorFlightMeta' examMeta' briefingMeta' e) es

htmlLogbook ::
  (AircraftFlight -> a -> Html x)
  -> (SimulatorFlight -> b -> Html x)
  -> (Exam -> c -> Html x)
  -> (Briefing -> d -> Html x)
  -> Html ()
  -> Logbook a b c d
  -> Html ()
htmlLogbook aircraftFlightMeta' simulatorFlightMeta' examMeta' briefingMeta' reports' (Logbook a es) =
  do  htmlAviator a
      hr_ []
      reports'
      htmlEntries aircraftFlightMeta' simulatorFlightMeta' examMeta' briefingMeta' es

htmlTitleAviator ::
  Aviator
  -> Html ()
htmlTitleAviator a =
  fromString (concat
                [
                  a ^. firstname
                , " "
                , a ^. surname
                , " ("
                , show =<< (a ^. arn)
                , ")"
                ])

htmlLogbookDocument ::
  (AircraftFlight -> a -> Html x)
  -> (SimulatorFlight -> b -> Html x)
  -> (Exam -> c -> Html x)
  -> (Briefing -> d -> Html x)
  -> Html ()
  -> Logbook a b c d
  -> Html ()
htmlLogbookDocument aircraftFlightMeta' simulatorFlightMeta' examMeta' briefingMeta' reports' b =
  do  doctype_
      html_ [lang_ "en"] $
        do  head_ $ 
              do  title_ ("Pilot Personal Logbook " <> toHtmlRaw (" &mdash; " :: Text.Text) <> htmlTitleAviator (b ^. logbookaviator))
                  link_ [href_ "https://fonts.googleapis.com/css?family=Inconsolata:400,700", rel_ "stylesheet", type_ "text/css"]
                  link_ [href_ "casr-logbook.css", rel_ "stylesheet", type_ "text/css"]
                  link_ [href_ "/atom.xml", rel_ "alternate", type_ "application/atom+xml", title_ "Atom feed"]
                  script_ [type_ "text/javascript", src_ "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"] ("" :: Text.Text)
                  script_ [type_ "text/javascript", src_ "https://raw.github.com/Mathapedia/LaTeX2HTML5/master/latex2html5.min.js"] ("" :: Text.Text)                  
            body_ [class_ "casr-logbook"] $ 
              do  htmlLogbookHeader b
                  htmlLogbook aircraftFlightMeta' simulatorFlightMeta' examMeta' briefingMeta' reports' b

htmlLogbookHeader ::
  Logbook a b c d
  -> Html ()
htmlLogbookHeader _ =
  do  div_ [id_ "header", class_ "header"] $
        h1_ "Pilot Personal Log Book"
      div_ [id_ "subheader", class_ "subheader"] $
        h2_ $
          do  "Civil Aviation Safety Regulation 1998 (61.345)"
              " "
              span_ [class_ "austlii"] $
                a_ [href_ "http://www.austlii.edu.au/au/legis/cth/consol_reg/casr1998333/s61.345.html"] "austlii.edu.au"

---- helpers, belong elsewhere


totalDayNight ::
  DayNight
  -> TimeAmount
totalDayNight (DayNight d n) =
  d `mappend` n

getInstructingPic ::
  Command
  -> Maybe Aviator
getInstructingPic (ICUS a) =
  Just a
getInstructingPic (Dual a) =
  Just a
getInstructingPic InCommand =
  Nothing


showCentsAsDollars ::
  Int
  -> String
showCentsAsDollars n =
  let pos ::
        String
        -> String
      pos [] =
        []
      pos [x] =
        "0.0" ++ [x]
      pos [x, y] =
        "0." ++ [y, x]
      pos (x:y:z) =
        reverse z ++ "." ++ [y, x]
  in  (if n < 0 then ('-':) else id) . pos . reverse . show . abs $ n

showThousandCentsAsDollars ::
  Int
  -> String
showThousandCentsAsDollars n =
  let pos ::
        String
        -> String
      pos [] =
        []
      pos [x] =
        [x] ++ "0.0"
      pos [x, y] =
        [x, y] ++ ".0"
      pos [x, y, z] =
        [x, y, z] ++ ".0"
      pos (x:y:z:r) =
        [x, y, z] ++ "." ++ r
      drop0 [] =
        []
      drop0 ('0':r) =
        r
      drop0 w =
        w
  in  (if n < 0 then ('-':) else id) . reverse . drop0 . pos . reverse . show . abs $ n

timeAmountBy10 ::
  TimeAmount
  -> Int
timeAmountBy10 (TimeAmount a b) =
  a * 10 + digit # b

flightPathList ::
  FlightPath
  -> [FlightPoint]
flightPathList (FlightPath s x e) =
  s : x ++ [e]

whenEmpty ::
  Monoid a =>
  ([t] -> a)
  -> [t]
  -> a
whenEmpty _ [] =
  mempty
whenEmpty f x =
  f x

---- Test


writetest ::
  IO ()
writetest =
  renderToFile "/tmp/z.html" (htmlLogbookDocument htmlAircraftFlightMeta htmlSimulatorFlightMeta htmlExamMeta htmlBriefingMeta (htmlFlightTimeReport logbook1007036 (getFlightTimeReport logbook1007036)) logbook1007036)

