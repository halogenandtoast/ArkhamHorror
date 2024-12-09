module Arkham.Enemy.Cards.HydraDeepInSlumber (
  hydraDeepInSlumber,
  HydraDeepInSlumber (..),
)
where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Message.Lifted.Choose

newtype HydraDeepInSlumber = HydraDeepInSlumber EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hydraDeepInSlumber :: EnemyCard HydraDeepInSlumber
hydraDeepInSlumber = enemyWith
  HydraDeepInSlumber
  Cards.hydraDeepInSlumber
  (0, Static 1, 0)
  (0, 0)
  $ \a -> a {enemyFight = Nothing, enemyHealth = Nothing, enemyEvade = Nothing}

instance HasModifiersFor HydraDeepInSlumber where
  getModifiersFor (HydraDeepInSlumber a) = modifySelf a [Omnipotent]

instance HasAbilities HydraDeepInSlumber where
  getAbilities (HydraDeepInSlumber a) =
    [ restricted
        a
        1
        ( exists (InvestigatorAt $ locationIs Locations.lairOfHydra)
            <> thisExists a (IncludeOmnipotent ReadyEnemy)
        )
        $ forced
        $ RoundEnds #when
    , skillTestAbility $ restricted a 1 OnSameLocation actionAbility
    ]

instance RunMessage HydraDeepInSlumber where
  runMessage msg e@(HydraDeepInSlumber attrs) = runQueueT $ case msg of
    EnemyCheckEngagement eid | eid == attrs.id -> pure e
    Flip _ _ (isTarget attrs -> True) -> do
      awakened <- genCard Cards.hydraAwakenedAndEnraged
      push $ ReplaceEnemy attrs.id awakened Swap
      pure e
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 4)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      chooseOrRunOneM iid do
        when attrs.ready do
          labeled "Exhaust Hydra" $ exhaustThis attrs
        when (attrs.doom > 0) do
          labeled "Remove 1 doom from Hydra" $ removeDoom (attrs.ability 2) attrs 1
      pure e
    FailedThisSkillTestBy _iid (isAbilitySource attrs 2 -> True) n | n >= 3 -> do
      placeDoom (attrs.ability 2) attrs 1
      pure e
    _ -> HydraDeepInSlumber <$> liftRunMessage msg attrs
