module Arkham.Types.Event
  ( module Arkham.Types.Event
  )
where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Cards
import Arkham.Types.Event.Runner
import Arkham.Types.EventId
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.Query

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
  | Taunt' Taunt
  | Teamwork' Teamwork
  | Taunt2' Taunt2
  | Shortcut' Shortcut
  | SeekingAnswers' SeekingAnswers
  | ThinkOnYourFeet' ThinkOnYourFeet
  | BindMonster2' BindMonster2
  | BaitAndSwitch' BaitAndSwitch
  | EmergencyAid' EmergencyAid
  | IveGotAPlan' IveGotAPlan
  | Contraband' Contraband
  | DelveTooDeep' DelveTooDeep
  | LetMeHandleThis' LetMeHandleThis
  | SecondWind' SecondWind
  | BloodRite' BloodRite
  | AstoundingRevelation' AstoundingRevelation
  | FirstWatch' FirstWatch
  | DynamiteBlast2' DynamiteBlast2
  | Barricade3' Barricade3
  | HotStreak2' HotStreak2
  | MindWipe3' MindWipe3
  | Contraband2' Contraband2
  | Taunt3' Taunt3
  | IveGotAPlan2' IveGotAPlan2
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance HasActions env Event
deriving anyclass instance HasCount ClueCount env InvestigatorId => HasModifiersFor env Event
deriving anyclass instance EventRunner env => RunMessage env Event

instance Entity Event where
  type EntityId Event = EventId
  toId = eventId . eventAttrs
  toTarget = toTarget . eventAttrs
  isTarget = isTarget . eventAttrs
  toSource = toSource . eventAttrs
  isSource = isSource . eventAttrs

instance IsCard Event where
  getCardId = getCardId . eventAttrs
  getCardCode = getCardCode . eventAttrs
  getTraits = getTraits . eventAttrs
  getKeywords = getKeywords . eventAttrs

getEventId :: Event -> EventId
getEventId = eventId . eventAttrs

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
  , ("02017", (Taunt' .) . taunt)
  , ("02018", (Teamwork' .) . teamwork)
  , ("02019", (Taunt2' .) . taunt2)
  , ("02022", (Shortcut' .) . shortcut)
  , ("02023", (SeekingAnswers' .) . seekingAnswers)
  , ("02025", (ThinkOnYourFeet' .) . thinkOnYourFeet)
  , ("02031", (BindMonster2' .) . bindMonster2)
  , ("02034", (BaitAndSwitch' .) . baitAndSwitch)
  , ("02105", (EmergencyAid' .) . emergencyAid)
  , ("02107", (IveGotAPlan' .) . iveGotAPlan)
  , ("02109", (Contraband' .) . contraband)
  , ("02111", (DelveTooDeep' .) . delveTooDeep)
  , ("03022", (LetMeHandleThis' .) . letMeHandleThis)
  , ("04149", (SecondWind' .) . secondWind)
  , ("05317", (BloodRite' .) . bloodRite)
  , ("06023", (AstoundingRevelation' .) . astoundingRevelation)
  , ("06110", (FirstWatch' .) . firstWatch)
  , ("50002", (DynamiteBlast2' .) . dynamiteBlast2)
  , ("50004", (Barricade3' .) . barricade3)
  , ("50006", (HotStreak2' .) . hotStreak2)
  , ("50008", (MindWipe3' .) . mindWipe3)
  , ("51005", (Contraband2' .) . contraband2)
  , ("60130", (Taunt3' .) . taunt3)
  , ("60255", (IveGotAPlan2' .) . iveGotAPlan2)
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
  Taunt' attrs -> coerce attrs
  Teamwork' attrs -> coerce attrs
  Taunt2' attrs -> coerce attrs
  Shortcut' attrs -> coerce attrs
  SeekingAnswers' attrs -> coerce attrs
  ThinkOnYourFeet' attrs -> coerce attrs
  BindMonster2' attrs -> coerce attrs
  BaitAndSwitch' attrs -> coerce attrs
  EmergencyAid' attrs -> coerce attrs
  IveGotAPlan' attrs -> coerce attrs
  Contraband' attrs -> coerce attrs
  DelveTooDeep' attrs -> coerce attrs
  LetMeHandleThis' attrs -> coerce attrs
  SecondWind' attrs -> coerce attrs
  BloodRite' attrs -> coerce attrs
  AstoundingRevelation' attrs -> coerce attrs
  FirstWatch' (FirstWatch (attrs `With` _)) -> attrs
  DynamiteBlast2' attrs -> coerce attrs
  Barricade3' attrs -> coerce attrs
  HotStreak2' attrs -> coerce attrs
  MindWipe3' attrs -> coerce attrs
  Contraband2' attrs -> coerce attrs
  Taunt3' attrs -> coerce attrs
  IveGotAPlan2' attrs -> coerce attrs
