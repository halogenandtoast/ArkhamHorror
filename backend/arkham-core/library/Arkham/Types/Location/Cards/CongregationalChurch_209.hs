module Arkham.Types.Location.Cards.CongregationalChurch_209
  ( congregationalChurch_209
  , CongregationalChurch_209(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

newtype CongregationalChurch_209 = CongregationalChurch_209 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

congregationalChurch_209 :: LocationCard CongregationalChurch_209
congregationalChurch_209 = location
  CongregationalChurch_209
  Cards.congregationalChurch_209
  2
  (PerPlayer 1)
  Diamond
  [Plus, Triangle, Squiggle]

ability :: LocationAttrs -> Ability
ability attrs = mkAbility
  (toSource attrs)
  1
  (ActionAbility Nothing
  $ Costs [ActionCost 1, HandDiscardCost 1 Nothing mempty mempty]
  )

instance HasAbilities env CongregationalChurch_209 where
  getAbilities iid window@(Window Timing.When NonFast) (CongregationalChurch_209 attrs)
    | locationRevealed attrs
    = withBaseAbilities iid window attrs
      $ pure [locationAbility (ability attrs)]
  getAbilities iid window@(Window Timing.When FastPlayerWindow) (CongregationalChurch_209 attrs)
    | locationRevealed attrs
    = withBaseAbilities iid window attrs
      $ pure
          [ drawCardUnderneathLocationAction attrs | locationClues attrs == 0 ]
  getAbilities iid window (CongregationalChurch_209 attrs) =
    getAbilities iid window attrs

instance LocationRunner env => RunMessage env CongregationalChurch_209 where
  runMessage msg l@(CongregationalChurch_209 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (TakeResources iid 2 False)
    _ -> CongregationalChurch_209 <$> runMessage msg attrs
