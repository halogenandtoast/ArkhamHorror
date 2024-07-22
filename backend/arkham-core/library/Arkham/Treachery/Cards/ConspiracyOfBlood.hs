module Arkham.Treachery.Cards.ConspiracyOfBlood (conspiracyOfBlood, ConspiracyOfBlood (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher
import Arkham.Source
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ConspiracyOfBlood = ConspiracyOfBlood TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

conspiracyOfBlood :: TreacheryCard ConspiracyOfBlood
conspiracyOfBlood = treachery ConspiracyOfBlood Cards.conspiracyOfBlood

instance HasModifiersFor ConspiracyOfBlood where
  getModifiersFor (AgendaTarget aid) (ConspiracyOfBlood a) | treacheryOnAgenda aid a = do
    pure $ toModifiers a [DoomThresholdModifier (-1)]
  getModifiersFor _ _ = pure []

instance HasAbilities ConspiracyOfBlood where
  getAbilities (ConspiracyOfBlood attrs) =
    [ restrictedAbility
        (proxied (EnemyMatcherSource $ EnemyWithTrait Cultist) attrs)
        1
        OnSameLocation
        parleyAction_
    ]

instance RunMessage ConspiracyOfBlood where
  runMessage msg t@(ConspiracyOfBlood attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      currentAgenda <- selectJust AnyAgenda
      attachTreachery attrs currentAgenda
      pure t
    UseThisAbility iid (ProxySource (EnemySource eid) source) 1 | isSource attrs source -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) eid #willpower (Fixed 4)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    FailedThisSkillTest _ (isSource attrs -> True) -> do
      getSkillTestTarget >>= \case
        Just e@(EnemyTarget _) -> placeDoom (attrs.ability 1) e 1
        _ -> pure ()
      pure t
    _ -> ConspiracyOfBlood <$> liftRunMessage msg attrs
