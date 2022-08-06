module Arkham.Location.Cards.CongregationalChurch_209
  ( congregationalChurch_209
  , CongregationalChurch_209(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message

newtype CongregationalChurch_209 = CongregationalChurch_209 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

congregationalChurch_209 :: LocationCard CongregationalChurch_209
congregationalChurch_209 = location
  CongregationalChurch_209
  Cards.congregationalChurch_209
  2
  (PerPlayer 1)

instance HasAbilities CongregationalChurch_209 where
  getAbilities (CongregationalChurch_209 attrs) =
    let rest = withDrawCardUnderneathAction attrs
    in
      rest
        <> [ restrictedAbility attrs 1 Here $ ActionAbility Nothing $ Costs
               [ActionCost 1, HandDiscardCost 1 AnyCard]
           | locationRevealed attrs
           ]

instance RunMessage CongregationalChurch_209 where
  runMessage msg l@(CongregationalChurch_209 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (TakeResources iid 2 False)
    _ -> CongregationalChurch_209 <$> runMessage msg attrs
