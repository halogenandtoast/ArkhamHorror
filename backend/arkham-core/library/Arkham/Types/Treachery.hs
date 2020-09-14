{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery
  ( lookupTreachery
  , Treachery(..)
  , isWeakness
  , treacheryLocation
  )
where

import Arkham.Json
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Query
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Cards.AbandonedAndAlone
import Arkham.Types.Treachery.Cards.Amnesia
import Arkham.Types.Treachery.Cards.AncientEvils
import Arkham.Types.Treachery.Cards.CoverUp
import Arkham.Types.Treachery.Cards.CryptChill
import Arkham.Types.Treachery.Cards.DissonantVoices
import Arkham.Types.Treachery.Cards.DreamsOfRlyeh
import Arkham.Types.Treachery.Cards.FalseLead
import Arkham.Types.Treachery.Cards.FrozenInFear
import Arkham.Types.Treachery.Cards.GraspingHands
import Arkham.Types.Treachery.Cards.Haunted
import Arkham.Types.Treachery.Cards.HospitalDebts
import Arkham.Types.Treachery.Cards.HuntingShadow
import Arkham.Types.Treachery.Cards.Hypochondria
import Arkham.Types.Treachery.Cards.LockedDoor
import Arkham.Types.Treachery.Cards.MysteriousChanting
import Arkham.Types.Treachery.Cards.ObscuringFog
import Arkham.Types.Treachery.Cards.OfferOfPower
import Arkham.Types.Treachery.Cards.OnWingsOfDarkness
import Arkham.Types.Treachery.Cards.Paranoia
import Arkham.Types.Treachery.Cards.Psychosis
import Arkham.Types.Treachery.Cards.RottingRemains
import Arkham.Types.Treachery.Cards.TheYellowSign
import Arkham.Types.Treachery.Cards.UmordhothsWrath
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import Safe (fromJustNote)

lookupTreachery
  :: CardCode -> (TreacheryId -> Maybe InvestigatorId -> Treachery)
lookupTreachery =
  fromJustNote "Unkown treachery" . flip HashMap.lookup allTreacheries

allTreacheries
  :: HashMap CardCode (TreacheryId -> Maybe InvestigatorId -> Treachery)
allTreacheries = HashMap.fromList
  [ ("01007", (CoverUp' .) . coverUp)
  , ("01011", (HospitalDebts' .) . hospitalDebts)
  , ("01015", (AbandonedAndAlone' .) . abandonedAndAlone)
  , ("01096", (Amnesia' .) . amnesia)
  , ("01097", (Paranoia' .) . paranoia)
  , ("01098", (Haunted' .) . haunted)
  , ("01099", (Psychosis' .) . psychosis)
  , ("01100", (Hypochondria' .) . hypochondria)
  , ("01135", (HuntingShadow' .) . huntingShadow)
  , ("01136", (FalseLead' .) . falseLead)
  , ("01158", (UmordhothsWrath' .) . umordhothsWrath)
  , ("01162", (GraspingHands' .) . graspingHands)
  , ("01166", (AncientEvils' .) . ancientEvils)
  , ("01163", (RottingRemains' .) . rottingRemains)
  , ("01164", (FrozenInFear' .) . frozenInFear)
  , ("01165", (DissonantVoices' .) . dissonantVoices)
  , ("01167", (CryptChill' .) . cryptChill)
  , ("01168", (ObscuringFog' .) . obscuringFog)
  , ("01171", (MysteriousChanting' .) . mysteriousChanting)
  , ("01173", (OnWingsOfDarkness' .) . onWingsOfDarkness)
  , ("01174", (LockedDoor' .) . lockedDoor)
  , ("01176", (TheYellowSign' .) . theYellowSign)
  , ("01178", (OfferOfPower' .) . offerOfPower)
  , ("01182", (DreamsOfRlyeh' .) . dreamsOfRlyeh)
  ]

instance HasCardCode Treachery where
  getCardCode = treacheryCardCode . treacheryAttrs

instance HasTraits Treachery where
  getTraits = treacheryTraits . treacheryAttrs

instance HasCount DoomCount () Treachery where
  getCount _ = DoomCount . treacheryDoom . treacheryAttrs

instance HasId (Maybe OwnerId) () Treachery where
  getId _ = (OwnerId <$>) . treacheryOwner . treacheryAttrs

data Treachery
  = CoverUp' CoverUp
  | HospitalDebts' HospitalDebts
  | AbandonedAndAlone' AbandonedAndAlone
  | Amnesia' Amnesia
  | Paranoia' Paranoia
  | Haunted' Haunted
  | Psychosis' Psychosis
  | Hypochondria' Hypochondria
  | HuntingShadow' HuntingShadow
  | FalseLead' FalseLead
  | UmordhothsWrath' UmordhothsWrath
  | GraspingHands' GraspingHands
  | AncientEvils' AncientEvils
  | RottingRemains' RottingRemains
  | FrozenInFear' FrozenInFear
  | DissonantVoices' DissonantVoices
  | CryptChill' CryptChill
  | ObscuringFog' ObscuringFog
  | MysteriousChanting' MysteriousChanting
  | OnWingsOfDarkness' OnWingsOfDarkness
  | LockedDoor' LockedDoor
  | TheYellowSign' TheYellowSign
  | OfferOfPower' OfferOfPower
  | DreamsOfRlyeh' DreamsOfRlyeh
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance (ActionRunner env investigator) => HasActions env investigator Treachery
deriving anyclass instance (TreacheryRunner env) => RunMessage env Treachery

treacheryAttrs :: Treachery -> Attrs
treacheryAttrs = \case
  CoverUp' (CoverUp (attrs `With` _)) -> attrs
  HospitalDebts' (HospitalDebts (attrs `With` _)) -> attrs
  AbandonedAndAlone' attrs -> coerce attrs
  Amnesia' attrs -> coerce attrs
  Paranoia' attrs -> coerce attrs
  Haunted' attrs -> coerce attrs
  Psychosis' attrs -> coerce attrs
  Hypochondria' attrs -> coerce attrs
  HuntingShadow' attrs -> coerce attrs
  FalseLead' attrs -> coerce attrs
  UmordhothsWrath' attrs -> coerce attrs
  GraspingHands' attrs -> coerce attrs
  AncientEvils' attrs -> coerce attrs
  RottingRemains' attrs -> coerce attrs
  FrozenInFear' attrs -> coerce attrs
  DissonantVoices' attrs -> coerce attrs
  CryptChill' attrs -> coerce attrs
  ObscuringFog' attrs -> coerce attrs
  MysteriousChanting' attrs -> coerce attrs
  OnWingsOfDarkness' attrs -> coerce attrs
  LockedDoor' attrs -> coerce attrs
  TheYellowSign' attrs -> coerce attrs
  OfferOfPower' attrs -> coerce attrs
  DreamsOfRlyeh' attrs -> coerce attrs

isWeakness :: Treachery -> Bool
isWeakness = treacheryWeakness . treacheryAttrs

treacheryLocation :: Treachery -> Maybe LocationId
treacheryLocation = treacheryAttachedLocation . treacheryAttrs
