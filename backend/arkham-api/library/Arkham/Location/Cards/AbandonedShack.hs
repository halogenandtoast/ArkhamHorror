module Arkham.Location.Cards.AbandonedShack (abandonedShack) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Location (swapLocation)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Token (Token (..), countTokens)

newtype AbandonedShack = AbandonedShack LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abandonedShack :: LocationCard AbandonedShack
abandonedShack = locationWith AbandonedShack Cards.abandonedShack 2 (PerPlayer 2) connectsToAdjacent

instance HasAbilities AbandonedShack where
  getAbilities (AbandonedShack a) =
    let hasSinkhole = countTokens Damage a.tokens > 0
     in extendRevealed a
          [ playerLimit PerRound
              $ restricted a 1 (Here <> if hasSinkhole then exists (EnemyAt $ be a) else Never)
              $ FastAbility (ClueCost (Static 1))
          ]

instance RunMessage AbandonedShack where
  runMessage msg l@(AbandonedShack attrs) = runQueueT $ case msg of
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.openWater10597b
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      removeTokens (attrs.ability 1) attrs Damage 1
      enemies <- select $ EnemyAt (be attrs)
      chooseTargetM iid enemies \enemy ->
        nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 enemy
      pure l
    _ -> AbandonedShack <$> liftRunMessage msg attrs
