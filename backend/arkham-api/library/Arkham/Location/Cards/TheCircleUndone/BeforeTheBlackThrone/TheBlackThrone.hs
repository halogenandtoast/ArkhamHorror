module Arkham.Location.Cards.TheCircleUndone.BeforeTheBlackThrone.TheBlackThrone (theBlackThrone) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.TheCircleUndone.BeforeTheBlackThrone qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.CardDefs.TheCircleUndone.BeforeTheBlackThrone qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.TheCircleUndone.BeforeTheBlackThrone.Helpers

newtype TheBlackThrone = TheBlackThrone LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBlackThrone :: LocationCard TheBlackThrone
theBlackThrone =
  locationWith TheBlackThrone Cards.theBlackThrone 1 (PerPlayer 2)
    $ connectsToL
    .~ adjacentLocations

instance HasModifiersFor TheBlackThrone where
  getModifiersFor (TheBlackThrone a) = whenRevealed a $ maybeModifySelf a do
    azathoth <- MaybeT $ selectOne $ IncludeOmnipotent $ enemyIs Enemies.azathoth
    doom <- lift $ field EnemyDoom azathoth
    let x = ceiling (fromIntegral doom / (2 :: Double))
    pure [ShroudModifier x]

instance HasAbilities TheBlackThrone where
  getAbilities (TheBlackThrone attrs) =
    extendRevealed1 attrs $ scenarioI18n $ hauntedI "theBlackThrone.haunted" attrs 1

instance RunMessage TheBlackThrone where
  runMessage msg l@(TheBlackThrone attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
      chooseOneM iid $ scenarioI18n do
        labeled "theBlackThrone.doom" $ placeDoom (attrs.ability 1) azathoth 1
        labeled "theBlackThrone.attack" $ initiateEnemyAttack azathoth (attrs.ability 1) iid
      pure l
    Do (PlaceCosmos _ (is attrs -> True) cloc) -> do
      handleCosmos attrs cloc
      pure l
    _ -> TheBlackThrone <$> liftRunMessage msg attrs
