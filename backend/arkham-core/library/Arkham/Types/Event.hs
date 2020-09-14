{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event
  ( lookupEvent
  , Event(..)
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
import Arkham.Types.Event.Cards.BlindingLight2
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
import Arkham.Types.Event.Cards.HotStreak4
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
import ClassyPrelude
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import Generics.SOP hiding (Generic)
import qualified Generics.SOP as SOP
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
  , ("01057", (HotStreak4' .) . hotStreak4)
  , ("01064", (DrawnToTheFlame' .) . drawnToTheFlame)
  , ("01065", (WardOfProtection' .) . wardOfProtection)
  , ("01066", (BlindingLight' .) . blindingLight)
  , ("01068", (MindWipe1' .) . mindWipe1)
  , ("01069", (BlindingLight2' .) . blindingLight2)
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
  | HotStreak4' HotStreak4
  | DrawnToTheFlame' DrawnToTheFlame
  | WardOfProtection' WardOfProtection
  | BlindingLight' BlindingLight
  | MindWipe1' MindWipe1
  | BlindingLight2' BlindingLight2
  | CunningDistraction' CunningDistraction
  | LookWhatIFound' LookWhatIFound
  | Lucky' Lucky
  | EmergencyCache' EmergencyCache
  | DynamiteBlast2' DynamiteBlast2
  | Barricade3' Barricade3
  | HotStreak2' HotStreak2
  | MindWipe3' MindWipe3
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, SOP.Generic)

deriving anyclass instance HasActions env investigator Event
deriving anyclass instance (EventRunner env) => RunMessage env Event

eventAttrs :: Event -> Attrs
eventAttrs = getAttrs

ownerOfEvent :: Event -> InvestigatorId
ownerOfEvent = eventOwner . eventAttrs

class (Coercible a Attrs) => IsAttrs a
instance (Coercible a Attrs) => IsAttrs a

getAttrs :: (All2 IsAttrs (Code a), SOP.Generic a) => a -> Attrs
getAttrs a = go (unSOP $ from a)
 where
  go :: (All2 IsAttrs xs) => NS (NP I) xs -> Attrs
  go (S next) = go next
  go (Z (I x :* _)) = coerce x
  go (Z Nil) = error "should not happen"
