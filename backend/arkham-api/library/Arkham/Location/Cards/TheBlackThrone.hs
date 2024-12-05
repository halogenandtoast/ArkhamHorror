module Arkham.Location.Cards.TheBlackThrone (theBlackThrone, TheBlackThrone (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

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
    extendRevealed1 attrs
      $ haunted "You must either place 1 doom on Azathoth, or Azathoth attacks you" attrs 1

instance RunMessage TheBlackThrone where
  runMessage msg l@(TheBlackThrone attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
      chooseOneM iid do
        labeled "Place 1 doom on Azathoth" $ placeDoom (attrs.ability 1) azathoth 1
        labeled "Azathoth attacks you" $ initiateEnemyAttack azathoth (attrs.ability 1) iid
      pure l
    _ -> TheBlackThrone <$> liftRunMessage msg attrs
