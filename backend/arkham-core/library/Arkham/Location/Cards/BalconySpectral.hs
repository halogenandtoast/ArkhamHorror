module Arkham.Location.Cards.BalconySpectral (
  balconySpectral,
  BalconySpectral (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype BalconySpectral = BalconySpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

balconySpectral :: LocationCard BalconySpectral
balconySpectral =
  location BalconySpectral Cards.balconySpectral 1 (PerPlayer 1)

instance HasAbilities BalconySpectral where
  getAbilities (BalconySpectral attrs) =
    withBaseAbilities
      attrs
      [haunted "Each of your cards with health takes 1 direct damage." attrs 1]

instance RunMessage BalconySpectral where
  runMessage msg l@(BalconySpectral attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      assets <- selectList $ assetControlledBy iid <> AssetWithHealth
      player <- getPlayer iid
      push
        $ chooseOneAtATime player
        $ targetLabel iid [InvestigatorDirectDamage iid (toSource attrs) 1 0]
        : [ targetLabel asset [AssetDamage asset (toSource attrs) 1 0]
          | asset <- assets
          ]
      pure l
    _ -> BalconySpectral <$> runMessage msg attrs
