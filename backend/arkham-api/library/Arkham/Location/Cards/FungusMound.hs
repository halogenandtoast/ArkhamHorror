module Arkham.Location.Cards.FungusMound (fungusMound) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheBlobThatAteEverything.Helpers
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (MiGo))

newtype FungusMound = FungusMound LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fungusMound :: LocationCard FungusMound
fungusMound = locationWith FungusMound Cards.fungusMound 5 (Static 0) connectsToAdjacent

instance HasAbilities FungusMound where
  getAbilities (FungusMound a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "fungusMound.test"
      $ restricted a 1 (Here <> exists (EnemyWithTrait MiGo))
      $ actionAbilityWithCost (SpendTokenCost Token.Resource (TargetIs ScenarioTarget))

instance RunMessage FungusMound where
  runMessage msg l@(FungusMound attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ EnemyWithTrait MiGo
      sid <- getRandom
      chooseTargetM iid enemies \enemy ->
        beginSkillTest sid iid (attrs.ability 1) enemy #intellect (Fixed 0)
      pure l
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) succeededBy -> do
      whenJustM getSkillTestTargetedEnemy
        $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) (min 5 succeededBy)
      pure l
    _ -> FungusMound <$> liftRunMessage msg attrs
