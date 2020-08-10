{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery
  ( lookupTreachery
  , Treachery(..)
  , isWeakness
  )
where

import Arkham.Json
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Cards.AncientEvils
import Arkham.Types.Treachery.Cards.CoverUp
import Arkham.Types.Treachery.Cards.CryptChill
import Arkham.Types.Treachery.Cards.DissonantVoices
import Arkham.Types.Treachery.Cards.FrozenInFear
import Arkham.Types.Treachery.Cards.GraspingHands
import Arkham.Types.Treachery.Cards.ObscuringFog
import Arkham.Types.Treachery.Cards.RottingRemains
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
  , ("01162", GraspingHands' . graspingHands)
  , ("01166", AncientEvils' . ancientEvils)
  , ("01163", RottingRemains' . rottingRemains)
  , ("01164", FrozenInFear' . frozenInFear)
  , ("01165", DissonantVoices' . dissonantVoices)
  , ("01167", CryptChill' . cryptChill)
  , ("01168", ObscuringFog' . obscuringFog)
  ]

instance HasCardCode Treachery where
  getCardCode = treacheryCardCode . treacheryAttrs

instance HasAbilities Treachery where
  getAbilities = treacheryAbilities . treacheryAttrs

instance HasTraits Treachery where
  getTraits = treacheryTraits . treacheryAttrs

data Treachery
  = CoverUp' CoverUp
  | GraspingHands' GraspingHands
  | AncientEvils' AncientEvils
  | RottingRemains' RottingRemains
  | FrozenInFear' FrozenInFear
  | DissonantVoices' DissonantVoices
  | CryptChill' CryptChill
  | ObscuringFog' ObscuringFog
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

treacheryAttrs :: Treachery -> Attrs
treacheryAttrs = \case
  CoverUp' (CoverUp (attrs `With` _)) -> attrs
  GraspingHands' attrs -> coerce attrs
  AncientEvils' attrs -> coerce attrs
  RottingRemains' attrs -> coerce attrs
  FrozenInFear' attrs -> coerce attrs
  DissonantVoices' attrs -> coerce attrs
  CryptChill' attrs -> coerce attrs
  ObscuringFog' attrs -> coerce attrs

isWeakness :: Treachery -> Bool
isWeakness = treacheryWeakness . treacheryAttrs

instance (TreacheryRunner env) => RunMessage env Treachery where
  runMessage msg = \case
    CoverUp' x -> CoverUp' <$> runMessage msg x
    GraspingHands' x -> GraspingHands' <$> runMessage msg x
    AncientEvils' x -> AncientEvils' <$> runMessage msg x
    RottingRemains' x -> RottingRemains' <$> runMessage msg x
    FrozenInFear' x -> FrozenInFear' <$> runMessage msg x
    DissonantVoices' x -> DissonantVoices' <$> runMessage msg x
    CryptChill' x -> CryptChill' <$> runMessage msg x
    ObscuringFog' x -> ObscuringFog' <$> runMessage msg x
