{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event
  ( lookupEvent
  , Event(..)
  , ownerOfEvent
  , eventAttrs
  )
where

import Arkham.Json
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Cards
import Arkham.Types.Event.Runner
import Arkham.Types.EventId
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import ClassyPrelude
import Data.Coerce
import Safe (fromJustNote)

data Event
  = OnTheLam' OnTheLam
  | DarkMemory' DarkMemory
  | Evidence' Evidence
  | Dodge' Dodge
  | DynamiteBlast' DynamiteBlast
  | ExtraAmmunition1' ExtraAmmunition1
  | MindOverMatter' MindOverMatter
  | WorkingAHunch' WorkingAHunch
  | Barricade' Barricade
  | CrypticResearch4' CrypticResearch4
  | Elusive' Elusive
  | Backstab' Backstab
  | SneakAttack' SneakAttack
  | SureGamble3' SureGamble3
  | HotStreak4' HotStreak4
  | DrawnToTheFlame' DrawnToTheFlame
  | WardOfProtection' WardOfProtection
  | BlindingLight' BlindingLight
  | MindWipe1' MindWipe1
  | BlindingLight2' BlindingLight2
  | CunningDistraction' CunningDistraction
  | LookWhatIFound' LookWhatIFound
  | Lucky' Lucky
  | CloseCall2' CloseCall2
  | Lucky2' Lucky2
  | WillToSurvive4' WillToSurvive4
  | EmergencyCache' EmergencyCache
  | SearchForTheTruth' SearchForTheTruth
  | BaitAndSwitch' BaitAndSwitch
  | LetMeHandleThis' LetMeHandleThis
  | BloodRite' BloodRite
  | AstoundingRevelation' AstoundingRevelation
  | FirstWatch' FirstWatch
  | DynamiteBlast2' DynamiteBlast2
  | Barricade3' Barricade3
  | HotStreak2' HotStreak2
  | MindWipe3' MindWipe3
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance HasActions env Event
deriving anyclass instance HasModifiersFor env Event
deriving anyclass instance EventRunner env => RunMessage env Event

instance HasCardCode Event where
  getCardCode = eventCardCode . eventAttrs

instance HasId EventId () Event where
  getId _ = getId () . eventAttrs

lookupEvent :: CardCode -> (InvestigatorId -> EventId -> Event)
lookupEvent cardCode =
  fromJustNote ("Unknown event: " <> show cardCode) $ lookup cardCode allEvents

allEvents :: HashMap CardCode (InvestigatorId -> EventId -> Event)
allEvents = mapFromList
  [ ("01010", (OnTheLam' .) . onTheLam)
  , ("01013", (DarkMemory' .) . darkMemory)
  , ("01022", (Evidence' .) . evidence)
  , ("01023", (Dodge' .) . dodge)
  , ("01024", (DynamiteBlast' .) . dynamiteBlast)
  , ("01026", (ExtraAmmunition1' .) . extraAmmunition1)
  , ("01036", (MindOverMatter' .) . mindOverMatter)
  , ("01037", (WorkingAHunch' .) . workingAHunch)
  , ("01038", (Barricade' .) . barricade)
  , ("01043", (CrypticResearch4' .) . crypticResearch4)
  , ("01050", (Elusive' .) . elusive)
  , ("01051", (Backstab' .) . backstab)
  , ("01052", (SneakAttack' .) . sneakAttack)
  , ("01056", (SureGamble3' .) . sureGamble3)
  , ("01057", (HotStreak4' .) . hotStreak4)
  , ("01064", (DrawnToTheFlame' .) . drawnToTheFlame)
  , ("01065", (WardOfProtection' .) . wardOfProtection)
  , ("01066", (BlindingLight' .) . blindingLight)
  , ("01068", (MindWipe1' .) . mindWipe1)
  , ("01069", (BlindingLight2' .) . blindingLight2)
  , ("01078", (CunningDistraction' .) . cunningDistraction)
  , ("01079", (LookWhatIFound' .) . lookWhatIFound)
  , ("01080", (Lucky' .) . lucky)
  , ("01083", (CloseCall2' .) . closeCall2)
  , ("01084", (Lucky2' .) . lucky2)
  , ("01085", (WillToSurvive4' .) . willToSurvive4)
  , ("01088", (EmergencyCache' .) . emergencyCache)
  , ("02008", (SearchForTheTruth' .) . searchForTheTruth)
  , ("02034", (BaitAndSwitch' .) . baitAndSwitch)
  , ("03022", (LetMeHandleThis' .) . letMeHandleThis)
  , ("05317", (BloodRite' .) . bloodRite)
  , ("06023", (AstoundingRevelation' .) . astoundingRevelation)
  , ("06110", (FirstWatch' .) . firstWatch)
  , ("50002", (DynamiteBlast2' .) . dynamiteBlast2)
  , ("50004", (Barricade3' .) . barricade3)
  , ("50006", (HotStreak2' .) . hotStreak2)
  , ("50008", (MindWipe3' .) . mindWipe3)
  ]

ownerOfEvent :: Event -> InvestigatorId
ownerOfEvent = eventOwner . eventAttrs

eventAttrs :: Event -> Attrs
eventAttrs = \case
  OnTheLam' attrs -> coerce attrs
  DarkMemory' attrs -> coerce attrs
  Evidence' attrs -> coerce attrs
  Dodge' attrs -> coerce attrs
  DynamiteBlast' attrs -> coerce attrs
  ExtraAmmunition1' attrs -> coerce attrs
  MindOverMatter' attrs -> coerce attrs
  WorkingAHunch' attrs -> coerce attrs
  Barricade' attrs -> coerce attrs
  CrypticResearch4' attrs -> coerce attrs
  Elusive' attrs -> coerce attrs
  Backstab' attrs -> coerce attrs
  SneakAttack' attrs -> coerce attrs
  SureGamble3' attrs -> coerce attrs
  HotStreak4' attrs -> coerce attrs
  DrawnToTheFlame' attrs -> coerce attrs
  WardOfProtection' attrs -> coerce attrs
  BlindingLight' attrs -> coerce attrs
  MindWipe1' attrs -> coerce attrs
  BlindingLight2' attrs -> coerce attrs
  CunningDistraction' attrs -> coerce attrs
  LookWhatIFound' attrs -> coerce attrs
  Lucky' attrs -> coerce attrs
  CloseCall2' attrs -> coerce attrs
  Lucky2' attrs -> coerce attrs
  WillToSurvive4' attrs -> coerce attrs
  EmergencyCache' attrs -> coerce attrs
  SearchForTheTruth' attrs -> coerce attrs
  BaitAndSwitch' attrs -> coerce attrs
  LetMeHandleThis' attrs -> coerce attrs
  BloodRite' attrs -> coerce attrs
  AstoundingRevelation' attrs -> coerce attrs
  FirstWatch' (FirstWatch (attrs `With` _)) -> attrs
  DynamiteBlast2' attrs -> coerce attrs
  Barricade3' attrs -> coerce attrs
  HotStreak2' attrs -> coerce attrs
  MindWipe3' attrs -> coerce attrs
