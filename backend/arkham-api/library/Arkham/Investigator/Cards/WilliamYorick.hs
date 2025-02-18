module Arkham.Investigator.Cards.WilliamYorick where

import Arkham.Ability
import Arkham.Card
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Playable
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Window (mkWhen)
import Arkham.Window qualified as Window

newtype WilliamYorick = WilliamYorick InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

williamYorick :: InvestigatorCard WilliamYorick
williamYorick =
  investigator WilliamYorick Cards.williamYorick
    $ Stats {health = 8, sanity = 6, willpower = 3, intellect = 2, combat = 4, agility = 3}

instance HasChaosTokenValue WilliamYorick where
  getChaosTokenValue iid ElderSign (WilliamYorick attrs) | iid == attrs.id = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance HasAbilities WilliamYorick where
  getAbilities (WilliamYorick attrs) =
    [ playerLimit PerRound
        $ restricted attrs 1 (Self <> PlayableCardInDiscard (DiscardOf You) #asset)
        $ freeReaction (Matcher.EnemyDefeated #after You ByAny AnyEnemy)
    ]

instance RunMessage WilliamYorick where
  runMessage msg i@(WilliamYorick attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' _ -> do
      let windows'' = nub $ windows' <> [mkWhen Window.NonFast, mkWhen (Window.DuringTurn iid)]
      playableCards <-
        filterM (getIsPlayable iid (attrs.ability 1) (UnpaidCost NoAction) windows'')
          $ filterCards (card_ #asset) (map toCard $ investigatorDiscard attrs)

      focusCards (map toCard $ investigatorDiscard attrs) do
        chooseTargetM iid playableCards \card -> do
          unfocusCards
          playCardPayingCost iid card
      pure i
    ElderSignEffect iid | iid == toId attrs -> do
      createCardEffect Cards.williamYorick Nothing (toSource ElderSign) iid
      pure i
    _ -> WilliamYorick <$> liftRunMessage msg attrs

newtype WilliamYorickEffect = WilliamYorickEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

williamYorickEffect :: EffectArgs -> WilliamYorickEffect
williamYorickEffect = cardEffect WilliamYorickEffect Cards.williamYorick

instance RunMessage WilliamYorickEffect where
  runMessage msg e@(WilliamYorickEffect attrs) = runQueueT case msg of
    PassedSkillTest _ _ _ SkillTestInitiatorTarget {} _ _ -> do
      void $ runMaybeT do
        iid <- hoistMaybe attrs.target.investigator
        liftGuardM $ withoutModifier iid CardsCannotLeaveYourDiscardPile
        discards <- lift $ field InvestigatorDiscard iid
        guard $ notNull discards
        lift do
          focusCards (map toCard discards) do
            chooseOneM iid do
              labeled "Do not return card to hand" nothing
              targets discards $ addToHand iid . only
      disableReturn e
    SkillTestEnds {} -> disableReturn e
    _ -> WilliamYorickEffect <$> liftRunMessage msg attrs
