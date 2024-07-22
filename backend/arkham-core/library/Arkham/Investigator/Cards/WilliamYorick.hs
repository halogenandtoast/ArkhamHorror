module Arkham.Investigator.Cards.WilliamYorick where

import Arkham.Ability
import Arkham.Card
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude
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
  getChaosTokenValue iid ElderSign (WilliamYorick attrs) | iid == investigatorId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance HasAbilities WilliamYorick where
  getAbilities (WilliamYorick attrs) =
    [ playerLimit PerRound
        $ restrictedAbility attrs 1 (Self <> PlayableCardInDiscard (DiscardOf You) #asset)
        $ freeReaction (Matcher.EnemyDefeated #after You ByAny AnyEnemy)
    ]

instance RunMessage WilliamYorick where
  runMessage msg i@(WilliamYorick attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' _ -> do
      let
        windows'' = nub $ windows' <> [mkWhen Window.NonFast, mkWhen (Window.DuringTurn iid)]
        targets = filter ((== AssetType) . toCardType) (investigatorDiscard attrs)
        playCardMsgs c =
          [addToHand iid c]
            <> if isFastCard c
              then [InitiatePlayCard iid c Nothing NoPayment windows'' False]
              else [PayCardCost iid c windows'']
      playableTargets <-
        filterM (getIsPlayable iid (attrs.ability 1) (UnpaidCost NoAction) windows'' . PlayerCard) targets
      player <- getPlayer iid
      push
        $ chooseOne player
        $ [targetLabel (toCardId card) (playCardMsgs $ PlayerCard card) | card <- playableTargets]
      pure i
    ResolveChaosToken _ ElderSign iid | iid == toId attrs -> do
      push $ createCardEffect Cards.williamYorick Nothing (toSource ElderSign) iid
      pure i
    _ -> WilliamYorick <$> runMessage msg attrs

newtype WilliamYorickEffect = WilliamYorickEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

williamYorickEffect :: EffectArgs -> WilliamYorickEffect
williamYorickEffect = cardEffect WilliamYorickEffect Cards.williamYorick

instance RunMessage WilliamYorickEffect where
  runMessage msg e@(WilliamYorickEffect attrs) = case msg of
    PassedSkillTest _ _ _ SkillTestInitiatorTarget {} _ _ ->
      case effectTarget attrs of
        InvestigatorTarget iid -> do
          modifiers' <- getModifiers (InvestigatorTarget iid)
          unless (CardsCannotLeaveYourDiscardPile `elem` modifiers') $ do
            discards <- field InvestigatorDiscard iid
            player <- getPlayer iid
            when (notNull discards)
              $ push
              $ chooseOne player
              $ Done "Do not return card to hand"
              : [targetLabel (toCardId card) [addToHand iid $ PlayerCard card] | card <- discards]
          pure e
        _ -> pure e
    SkillTestEnds _ _ _ -> e <$ push (DisableEffect $ effectId attrs)
    _ -> WilliamYorickEffect <$> runMessage msg attrs
