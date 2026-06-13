module Arkham.Location.Cards.TillinghastEsoterica (tillinghastEsoterica) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)

newtype TillinghastEsoterica = TillinghastEsoterica LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tillinghastEsoterica :: LocationCard TillinghastEsoterica
tillinghastEsoterica = location TillinghastEsoterica Cards.tillinghastEsoterica 3 (Static 1)

instance HasAbilities TillinghastEsoterica where
  getAbilities (TillinghastEsoterica a) =
    extendRevealed
      a
      [ groupLimit PerGame
          $ restricted a 1 Here
          $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) (be a))
      ]

instance RunMessage TillinghastEsoterica where
  runMessage msg a@(TillinghastEsoterica attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      record TheInvestigatorsDiscoveredAnAlienLanguage
      pure a
    _ -> TillinghastEsoterica <$> liftRunMessage msg attrs
