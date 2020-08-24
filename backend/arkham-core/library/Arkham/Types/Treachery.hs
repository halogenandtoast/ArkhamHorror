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
import Arkham.Types.LocationId
import Arkham.Types.Query
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Cards.AncientEvils
import Arkham.Types.Treachery.Cards.CoverUp
import Arkham.Types.Treachery.Cards.CryptChill
import Arkham.Types.Treachery.Cards.DissonantVoices
import Arkham.Types.Treachery.Cards.DreamsOfRlyeh
import Arkham.Types.Treachery.Cards.FalseLead
import Arkham.Types.Treachery.Cards.FrozenInFear
import Arkham.Types.Treachery.Cards.GraspingHands
import Arkham.Types.Treachery.Cards.HuntingShadow
import Arkham.Types.Treachery.Cards.LockedDoor
import Arkham.Types.Treachery.Cards.MysteriousChanting
import Arkham.Types.Treachery.Cards.ObscuringFog
import Arkham.Types.Treachery.Cards.OfferOfPower
import Arkham.Types.Treachery.Cards.OnWingsOfDarkness
import Arkham.Types.Treachery.Cards.RottingRemains
import Arkham.Types.Treachery.Cards.TheYellowSign
import Arkham.Types.Treachery.Cards.UmordhothsWrath
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import Safe (fromJustNote)

lookupTreachery :: CardCode -> (TreacheryId -> Treachery)
lookupTreachery =
  fromJustNote "Unkown treachery" . flip HashMap.lookup allTreacheries

allTreacheries :: HashMap CardCode (TreacheryId -> Treachery)
allTreacheries = HashMap.fromList
  [ ("01007", CoverUp' . coverUp)
  , ("01135", HuntingShadow' . huntingShadow)
  , ("01136", FalseLead' . falseLead)
  , ("01158", UmordhothsWrath' . umordhothsWrath)
  , ("01162", GraspingHands' . graspingHands)
  , ("01166", AncientEvils' . ancientEvils)
  , ("01163", RottingRemains' . rottingRemains)
  , ("01164", FrozenInFear' . frozenInFear)
  , ("01165", DissonantVoices' . dissonantVoices)
  , ("01167", CryptChill' . cryptChill)
  , ("01168", ObscuringFog' . obscuringFog)
  , ("01171", MysteriousChanting' . mysteriousChanting)
  , ("01173", OnWingsOfDarkness' . onWingsOfDarkness)
  , ("01174", LockedDoor' . lockedDoor)
  , ("01176", TheYellowSign' . theYellowSign)
  , ("01178", OfferOfPower' . offerOfPower)
  , ("01182", DreamsOfRlyeh' . dreamsOfRlyeh)
  ]

instance HasCardCode Treachery where
  getCardCode = treacheryCardCode . treacheryAttrs

instance HasTraits Treachery where
  getTraits = treacheryTraits . treacheryAttrs

instance HasCount DoomCount () Treachery where
  getCount _ = DoomCount . treacheryDoom . treacheryAttrs

data Treachery
  = CoverUp' CoverUp
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

treacheryAttrs :: Treachery -> Attrs
treacheryAttrs = \case
  CoverUp' (CoverUp (attrs `With` _)) -> attrs
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

instance (ActionRunner env investigator) => HasActions env investigator Treachery where
  getActions i window = \case
    CoverUp' x -> getActions i window x
    HuntingShadow' x -> getActions i window x
    FalseLead' x -> getActions i window x
    UmordhothsWrath' x -> getActions i window x
    GraspingHands' x -> getActions i window x
    AncientEvils' x -> getActions i window x
    RottingRemains' x -> getActions i window x
    FrozenInFear' x -> getActions i window x
    DissonantVoices' x -> getActions i window x
    CryptChill' x -> getActions i window x
    ObscuringFog' x -> getActions i window x
    MysteriousChanting' x -> getActions i window x
    OnWingsOfDarkness' x -> getActions i window x
    LockedDoor' x -> getActions i window x
    TheYellowSign' x -> getActions i window x
    OfferOfPower' x -> getActions i window x
    DreamsOfRlyeh' x -> getActions i window x

isWeakness :: Treachery -> Bool
isWeakness = treacheryWeakness . treacheryAttrs

treacheryLocation :: Treachery -> Maybe LocationId
treacheryLocation = treacheryAttachedLocation . treacheryAttrs

instance (TreacheryRunner env) => RunMessage env Treachery where
  runMessage msg = \case
    CoverUp' x -> CoverUp' <$> runMessage msg x
    HuntingShadow' x -> HuntingShadow' <$> runMessage msg x
    FalseLead' x -> FalseLead' <$> runMessage msg x
    UmordhothsWrath' x -> UmordhothsWrath' <$> runMessage msg x
    GraspingHands' x -> GraspingHands' <$> runMessage msg x
    AncientEvils' x -> AncientEvils' <$> runMessage msg x
    RottingRemains' x -> RottingRemains' <$> runMessage msg x
    FrozenInFear' x -> FrozenInFear' <$> runMessage msg x
    DissonantVoices' x -> DissonantVoices' <$> runMessage msg x
    CryptChill' x -> CryptChill' <$> runMessage msg x
    ObscuringFog' x -> ObscuringFog' <$> runMessage msg x
    MysteriousChanting' x -> MysteriousChanting' <$> runMessage msg x
    OnWingsOfDarkness' x -> OnWingsOfDarkness' <$> runMessage msg x
    LockedDoor' x -> LockedDoor' <$> runMessage msg x
    TheYellowSign' x -> TheYellowSign' <$> runMessage msg x
    OfferOfPower' x -> OfferOfPower' <$> runMessage msg x
    DreamsOfRlyeh' x -> DreamsOfRlyeh' <$> runMessage msg x
