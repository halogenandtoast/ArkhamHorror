module Arkham.Investigator.Cards.RitaYoung (
  ritaYoung,
  ritaYoungElderSignEffect,
  RitaYoung (..),
) where

import Arkham.Ability
import Arkham.DamageEffect
import Arkham.Effect.Import
import Arkham.Game.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Movement

newtype RitaYoung = RitaYoung InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

ritaYoung :: InvestigatorCard RitaYoung
ritaYoung =
  investigator RitaYoung Cards.ritaYoung
    $ Stats {health = 9, sanity = 5, willpower = 3, intellect = 2, combat = 3, agility = 5}

instance HasAbilities RitaYoung where
  getAbilities (RitaYoung a) =
    [ playerLimit PerRound
        $ (restrictedAbility a 1)
          ( Self
              <> oneOf
                [ exists AccessibleLocation
                , exists (EvadingEnemy <> EnemyCanBeDamagedBySource (a.ability 1)) <> CanDealDamage
                ]
          )
          (freeReaction $ Matcher.EnemyEvaded #after You AnyEnemy)
    ]

instance HasChaosTokenValue RitaYoung where
  getChaosTokenValue iid ElderSign (RitaYoung attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 2
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage RitaYoung where
  runMessage msg i@(RitaYoung attrs) = runQueueT $ case msg of
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      createCardEffect Cards.ritaYoung Nothing attrs iid
      pure i
    UseCardAbility iid (isSource attrs -> True) 1 (evadedEnemy -> enemyId) _ -> do
      canDamage <-
        andM
          [ enemyId <=~> EnemyCanBeDamagedBySource (attrs.ability 1)
          , withoutModifier iid CannotDealDamage
          ]
      connectingLocations <- getAccessibleLocations iid attrs
      chooseOrRunOneM iid do
        when canDamage do
          labeled "Damage enemy" do
            push $ EnemyDamage enemyId $ nonAttack (attrs.ability 1) 1
        when (notNull connectingLocations) do
          labeled "Move to a connecting location" do
            chooseOne iid $ targetLabels connectingLocations $ only . Move . move attrs iid
      pure i
    _ -> RitaYoung <$> liftRunMessage msg attrs

newtype RitaYoungElderSignEffect = RitaYoungElderSignEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

ritaYoungElderSignEffect :: EffectArgs -> RitaYoungElderSignEffect
ritaYoungElderSignEffect = cardEffect RitaYoungElderSignEffect Cards.ritaYoung

instance HasModifiersFor RitaYoungElderSignEffect where
  getModifiersFor (RitaYoungElderSignEffect a) = case a.target of
    InvestigatorTarget iid ->
      selectOne (AbilityIs (toSource iid) 1) >>= \case
        Nothing -> pure mempty
        Just ab -> modified_ a (AbilityTarget iid ab) [IgnoreLimit]
    _ -> pure mempty

instance RunMessage RitaYoungElderSignEffect where
  runMessage msg e@(RitaYoungElderSignEffect attrs) = runQueueT $ case msg of
    EndRound -> disableReturn e
    _ -> RitaYoungElderSignEffect <$> liftRunMessage msg attrs
