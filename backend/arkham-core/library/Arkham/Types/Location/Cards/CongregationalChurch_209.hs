module Arkham.Types.Location.Cards.CongregationalChurch_209
  ( congregationalChurch_209
  , CongregationalChurch_209(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Message

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

instance HasAbilities CongregationalChurch_209 where
  getAbilities (CongregationalChurch_209 attrs) = do
    let rest = withDrawCardUnderneathAction attrs
    [ restrictedAbility attrs 1 Here $ ActionAbility Nothing $ Costs
            [ActionCost 1, HandDiscardCost 1 Nothing mempty mempty]
        | locationRevealed attrs
        ]
      <> rest

instance LocationRunner env => RunMessage env CongregationalChurch_209 where
  runMessage msg l@(CongregationalChurch_209 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (TakeResources iid 2 False)
    _ -> CongregationalChurch_209 <$> runMessage msg attrs
