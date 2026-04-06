module Arkham.Event.Events.ConsumeLife (consumeLife) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher hiding (EnemyDefeated)
import Arkham.Modifier

newtype ConsumeLife = ConsumeLife EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

consumeLife :: EventCard ConsumeLife
consumeLife = event ConsumeLife Cards.consumeLife

instance RunMessage ConsumeLife where
  runMessage msg e@(ConsumeLife attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (DamageDealt 1)
      chooseFightEnemyWith #willpower sid iid attrs
      pure e
    EnemyDefeated _ _ (isSource attrs -> True) _ -> do
      let iid = attrs.controller
      assets <-
        selectTargets $ HealableAsset (toSource attrs) #damage (AllyAsset <> assetAtLocationWith iid)
      investigators <- selectTargets $ HealableInvestigator (toSource attrs) #damage $ colocatedWith iid
      unless (null assets && null investigators) do
        chooseOneM iid $ targets (assets <> investigators) \target -> healDamage target attrs 1
      pure e
    _ -> ConsumeLife <$> liftRunMessage msg attrs
