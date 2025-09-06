module Arkham.Investigator.Cards.ZoeySamaras where

import Arkham.Ability
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype ZoeySamaras = ZoeySamaras InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

zoeySamaras :: InvestigatorCard ZoeySamaras
zoeySamaras =
  investigator ZoeySamaras Cards.zoeySamaras
    $ Stats {health = 9, sanity = 6, willpower = 4, intellect = 2, combat = 4, agility = 2}

instance HasAbilities ZoeySamaras where
  getAbilities (ZoeySamaras x) =
    [selfAbility x 1 CanGainResources $ freeReaction $ EnemyEngaged #after You AnyEnemy]

instance HasChaosTokenValue ZoeySamaras where
  getChaosTokenValue iid ElderSign (ZoeySamaras attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage ZoeySamaras where
  runMessage msg i@(ZoeySamaras attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 1
      pure i
    ElderSignEffect (is attrs -> True) -> do
      withSkillTest \sid -> skillTestModifier sid attrs attrs (DamageDealt 1)
      pure i
    _ -> ZoeySamaras <$> liftRunMessage msg attrs
