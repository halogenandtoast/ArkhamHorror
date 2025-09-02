module Arkham.Location.Cards.WesternRidge (westernRidge) where

import Arkham.Ability
import Arkham.Helpers.Window (evadingEnemy, windowSkillTestId)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype WesternRidge = WesternRidge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

westernRidge :: LocationCard WesternRidge
westernRidge = location WesternRidge Cards.westernRidge 2 (PerPlayer 1)

instance HasAbilities WesternRidge where
  getAbilities (WesternRidge a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ mkAbility a 1
      $ triggered (AttemptToEvade #when You (enemyAt a)) (HandDiscardCost 2 #any)

instance RunMessage WesternRidge where
  runMessage msg l@(WesternRidge attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (evadingEnemy &&& windowSkillTestId -> (enemy, sid)) _ -> do
      skillTestModifier sid (attrs.ability 1) enemy (EnemyEvade (-2))
      pure l
    _ -> WesternRidge <$> liftRunMessage msg attrs
