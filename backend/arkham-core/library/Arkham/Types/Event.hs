{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event
  ( lookupEvent
  , Event(..)
  , ownerOfEvent
  , toAttrs
  , getEventId
  )
where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Cards
import Arkham.Types.Event.Runner

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
  | SecondWind' SecondWind
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

instance Entity Event where
  type EntityId Event = EventId
  toId = eventId . toAttrs
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance HasCardCode Event where
  getCardCode = eventCardCode . toAttrs

getEventId :: Event -> EventId
getEventId = eventId . toAttrs

instance HasId EventId env Event where
  getId = pure . getEventId

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
  , ("04149", (SecondWind' .) . secondWind)
  , ("05317", (BloodRite' .) . bloodRite)
  , ("06023", (AstoundingRevelation' .) . astoundingRevelation)
  , ("06110", (FirstWatch' .) . firstWatch)
  , ("50002", (DynamiteBlast2' .) . dynamiteBlast2)
  , ("50004", (Barricade3' .) . barricade3)
  , ("50006", (HotStreak2' .) . hotStreak2)
  , ("50008", (MindWipe3' .) . mindWipe3)
  ]

ownerOfEvent :: Event -> InvestigatorId
ownerOfEvent = eventOwner . toAttrs

instance HasAttrs Event where
  type AttrsT Event = Attrs
  toAttrs = toAttrs . toAttrs
