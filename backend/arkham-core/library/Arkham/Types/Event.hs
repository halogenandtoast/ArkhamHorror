{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event
  ( lookupEvent
  , Event(..)
  , eventLocation
  , ownerOfEvent
  )
where

import Arkham.Json
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Cards.Backstab
import Arkham.Types.Event.Cards.Barricade
import Arkham.Types.Event.Cards.Barricade3
import Arkham.Types.Event.Cards.BlindingLight
import Arkham.Types.Event.Cards.CrypticResearch4
import Arkham.Types.Event.Cards.CunningDistraction
import Arkham.Types.Event.Cards.DarkMemory
import Arkham.Types.Event.Cards.Dodge
import Arkham.Types.Event.Cards.DrawnToTheFlame
import Arkham.Types.Event.Cards.DynamiteBlast
import Arkham.Types.Event.Cards.DynamiteBlast2
import Arkham.Types.Event.Cards.Elusive
import Arkham.Types.Event.Cards.EmergencyCache
import Arkham.Types.Event.Cards.Evidence
import Arkham.Types.Event.Cards.ExtraAmmunition1
import Arkham.Types.Event.Cards.HotStreak2
import Arkham.Types.Event.Cards.LookWhatIFound
import Arkham.Types.Event.Cards.Lucky
import Arkham.Types.Event.Cards.MindOverMatter
import Arkham.Types.Event.Cards.MindWipe1
import Arkham.Types.Event.Cards.MindWipe3
import Arkham.Types.Event.Cards.OnTheLam
import Arkham.Types.Event.Cards.SneakAttack
import Arkham.Types.Event.Cards.SureGamble3
import Arkham.Types.Event.Cards.WardOfProtection
import Arkham.Types.Event.Cards.WorkingAHunch
import Arkham.Types.Event.Runner
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import ClassyPrelude
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import Safe (fromJustNote)

lookupEvent :: CardCode -> (InvestigatorId -> EventId -> Event)
lookupEvent cardCode =
  fromJustNote ("Unknown event: " <> show cardCode)
    $ HashMap.lookup cardCode allEvents

allEvents :: HashMap CardCode (InvestigatorId -> EventId -> Event)
allEvents = HashMap.fromList
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
  , ("01064", (DrawnToTheFlame' .) . drawnToTheFlame)
  , ("01065", (WardOfProtection' .) . wardOfProtection)
  , ("01066", (BlindingLight' .) . blindingLight)
  , ("01068", (MindWipe1' .) . mindWipe1)
  , ("01078", (CunningDistraction' .) . cunningDistraction)
  , ("01079", (LookWhatIFound' .) . lookWhatIFound)
  , ("01080", (Lucky' .) . lucky)
  , ("01088", (EmergencyCache' .) . emergencyCache)
  , ("50002", (DynamiteBlast2' .) . dynamiteBlast2)
  , ("50004", (Barricade3' .) . barricade3)
  , ("50006", (HotStreak2' .) . hotStreak2)
  , ("50008", (MindWipe3' .) . mindWipe3)
  ]

instance HasCardCode Event where
  getCardCode = eventCardCode . eventAttrs

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
  | DrawnToTheFlame' DrawnToTheFlame
  | WardOfProtection' WardOfProtection
  | BlindingLight' BlindingLight
  | MindWipe1' MindWipe1
  | CunningDistraction' CunningDistraction
  | LookWhatIFound' LookWhatIFound
  | Lucky' Lucky
  | EmergencyCache' EmergencyCache
  | DynamiteBlast2' DynamiteBlast2
  | Barricade3' Barricade3
  | HotStreak2' HotStreak2
  | MindWipe3' MindWipe3
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  DrawnToTheFlame' attrs -> coerce attrs
  WardOfProtection' attrs -> coerce attrs
  BlindingLight' attrs -> coerce attrs
  MindWipe1' attrs -> coerce attrs
  CunningDistraction' attrs -> coerce attrs
  LookWhatIFound' attrs -> coerce attrs
  Lucky' attrs -> coerce attrs
  EmergencyCache' attrs -> coerce attrs
  DynamiteBlast2' attrs -> coerce attrs
  Barricade3' attrs -> coerce attrs
  HotStreak2' attrs -> coerce attrs
  MindWipe3' attrs -> coerce attrs

instance HasActions env investigator Event where
  getActions i window = \case
    OnTheLam' x -> getActions i window x
    DarkMemory' x -> getActions i window x
    Evidence' x -> getActions i window x
    Dodge' x -> getActions i window x
    DynamiteBlast' x -> getActions i window x
    ExtraAmmunition1' x -> getActions i window x
    MindOverMatter' x -> getActions i window x
    WorkingAHunch' x -> getActions i window x
    Barricade' x -> getActions i window x
    CrypticResearch4' x -> getActions i window x
    Elusive' x -> getActions i window x
    Backstab' x -> getActions i window x
    SneakAttack' x -> getActions i window x
    SureGamble3' x -> getActions i window x
    DrawnToTheFlame' x -> getActions i window x
    WardOfProtection' x -> getActions i window x
    BlindingLight' x -> getActions i window x
    MindWipe1' x -> getActions i window x
    CunningDistraction' x -> getActions i window x
    LookWhatIFound' x -> getActions i window x
    Lucky' x -> getActions i window x
    EmergencyCache' x -> getActions i window x
    DynamiteBlast2' x -> getActions i window x
    Barricade3' x -> getActions i window x
    HotStreak2' x -> getActions i window x
    MindWipe3' x -> getActions i window x

eventLocation :: Event -> Maybe LocationId
eventLocation = eventAttachedLocation . eventAttrs

ownerOfEvent :: Event -> InvestigatorId
ownerOfEvent = eventOwner . eventAttrs

instance (EventRunner env) => RunMessage env Event where
  runMessage msg = \case
    OnTheLam' x -> OnTheLam' <$> runMessage msg x
    DarkMemory' x -> DarkMemory' <$> runMessage msg x
    Evidence' x -> Evidence' <$> runMessage msg x
    Dodge' x -> Dodge' <$> runMessage msg x
    DynamiteBlast' x -> DynamiteBlast' <$> runMessage msg x
    ExtraAmmunition1' x -> ExtraAmmunition1' <$> runMessage msg x
    MindOverMatter' x -> MindOverMatter' <$> runMessage msg x
    WorkingAHunch' x -> WorkingAHunch' <$> runMessage msg x
    Barricade' x -> Barricade' <$> runMessage msg x
    CrypticResearch4' x -> CrypticResearch4' <$> runMessage msg x
    Elusive' x -> Elusive' <$> runMessage msg x
    Backstab' x -> Backstab' <$> runMessage msg x
    SneakAttack' x -> SneakAttack' <$> runMessage msg x
    SureGamble3' x -> SureGamble3' <$> runMessage msg x
    DrawnToTheFlame' x -> DrawnToTheFlame' <$> runMessage msg x
    WardOfProtection' x -> WardOfProtection' <$> runMessage msg x
    BlindingLight' x -> BlindingLight' <$> runMessage msg x
    MindWipe1' x -> MindWipe1' <$> runMessage msg x
    CunningDistraction' x -> CunningDistraction' <$> runMessage msg x
    LookWhatIFound' x -> LookWhatIFound' <$> runMessage msg x
    Lucky' x -> Lucky' <$> runMessage msg x
    EmergencyCache' x -> EmergencyCache' <$> runMessage msg x
    DynamiteBlast2' x -> DynamiteBlast2' <$> runMessage msg x
    Barricade3' x -> Barricade3' <$> runMessage msg x
    HotStreak2' x -> HotStreak2' <$> runMessage msg x
    MindWipe3' x -> MindWipe3' <$> runMessage msg x
