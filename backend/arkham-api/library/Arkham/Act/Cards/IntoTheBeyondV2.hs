module Arkham.Act.Cards.IntoTheBeyondV2 (intoTheBeyondV2) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Location.Cards qualified as Locations
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype IntoTheBeyondV2 = IntoTheBeyondV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheBeyondV2 :: ActCard IntoTheBeyondV2
intoTheBeyondV2 = act (1, A) IntoTheBeyondV2 Cards.intoTheBeyondV2 Nothing

instance HasAbilities IntoTheBeyondV2 where
  getAbilities (IntoTheBeyondV2 x) =
    extend
      x
      [ mkAbility x 1 actionAbility
      , mkAbility x 2 $ Objective $ forced $ Enters #when Anyone "The Edge of the Universe"
      ]

instance RunMessage IntoTheBeyondV2 where
  runMessage msg a@(IntoTheBeyondV2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ DiscardTopOfEncounterDeck iid 3 (toSource attrs) (Just $ toTarget attrs)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      edge <- selectJust $ locationIs Locations.theEdgeOfTheUniverse
      createEnemyAt_ Enemies.sethBishopThrallOfYogSothoth edge
      advanceActDeck attrs
      pure a
    DiscardedTopOfEncounterDeck iid cards _ target | isTarget attrs target -> do
      let locationCards = map toCard $ filterLocations cards
      focusCards locationCards do
        chooseTargetM iid locationCards \card -> do
          unfocusCards
          push $ ResolveRevelation iid card
      pure a
    _ -> IntoTheBeyondV2 <$> liftRunMessage msg attrs
