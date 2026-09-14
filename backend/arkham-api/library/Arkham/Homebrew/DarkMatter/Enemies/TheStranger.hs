module Arkham.Homebrew.DarkMatter.Enemies.TheStranger (theStranger) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.MachineInYellow (resolveHiddenForcedEffects)
import Arkham.Matcher

newtype TheStranger = TheStranger EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStranger :: EnemyCard TheStranger
theStranger = enemy TheStranger Cards.theStranger & setSpawnAt (FarthestLocationFromAll Anywhere)

instance HasAbilities TheStranger where
  getAbilities (TheStranger a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ TurnEnds #when
      $ InvestigatorAt
      $ oneOf [locationWithEnemy a, connectedFrom (locationWithEnemy a)]

instance RunMessage TheStranger where
  runMessage msg e@(TheStranger attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resolveHiddenForcedEffects iid
      pure e
    _ -> TheStranger <$> liftRunMessage msg attrs
