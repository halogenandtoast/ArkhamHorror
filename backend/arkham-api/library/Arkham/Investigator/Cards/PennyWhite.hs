module Arkham.Investigator.Cards.PennyWhite (pennyWhite, pennyWhiteEffect, PennyWhite) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards hiding (pennyWhite)
import Arkham.Card
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Types (Field (..), discardL)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.SkillTest.Base

newtype PennyWhite = PennyWhite InvestigatorAttrs
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (IsInvestigator, ToJSON, FromJSON)
  deriving newtype Entity

pennyWhite :: InvestigatorCard PennyWhite
pennyWhite =
  startsWithInHand
    [ Cards.strayCat
    , Cards.lucky
    , Cards.knife
    , Cards.flashlight
    , Cards.actOfDesperation
    , Cards.actOfDesperation
    , Cards.ableBodied
    , Cards.ableBodied
    ]
    $ startsWith [Cards.digDeep, Cards.knife, Cards.flashlight]
    $ investigator PennyWhite Cards.pennyWhite
    $ Stats {health = 7, sanity = 5, willpower = 4, intellect = 1, combat = 3, agility = 2}

instance HasModifiersFor PennyWhite where
  getModifiersFor (PennyWhite a) = do
    modifySelfWith
      a
      setActiveDuringSetup
      [CannotPerformAction #draw, CannotDrawCards, CannotManipulateDeck, StartingResources (-3)]
    modifySelectWith a (assetIs Cards.flashlight) setActiveDuringSetup [AdditionalStartingUses (-1)]

instance HasAbilities PennyWhite where
  getAbilities (PennyWhite a) =
    [ playerLimit PerRound
        $ restricted a 1 (Self <> ClueOnLocation)
        $ freeReaction
        $ SkillTestResult #after You SkillTestFromRevelation (SuccessResult AnyValue)
    ]

instance HasChaosTokenValue PennyWhite where
  getChaosTokenValue iid ElderSign (PennyWhite attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 1
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage PennyWhite where
  runMessage msg i@(PennyWhite attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      pure i
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      mSkillTest <- getSkillTest
      for_ mSkillTest \skillTest ->
        when (skillTestIsRevelation skillTest) do
          createCardEffect Cards.pennyWhite Nothing attrs attrs
      pure i
    InvestigatorMulligan iid | iid == toId attrs -> do
      push $ FinishedWithMulligan iid
      pure i
    AddToDiscard iid pc | iid == toId attrs -> do
      push $ RemovedFromGame (PlayerCard pc)
      pure i
    DiscardCard iid _ cardId | iid == toId attrs -> do
      hand <- field InvestigatorHand iid
      let card = fromJustNote "must be in hand" $ find ((== cardId) . toCardId) hand
      pushAll [RemoveCardFromHand iid cardId, RemovedFromGame card]
      pure i
    Do (DiscardCard iid _ _) | iid == toId attrs -> pure i
    DrawCards iid cardDraw | iid == attrs.id && cardDraw.isPlayerDraw -> pure i
    _ -> do
      attrs' <- liftRunMessage msg attrs
      for_ (investigatorDiscard attrs) obtainCard
      pure $ PennyWhite $ attrs' & discardL .~ []

newtype PennyWhiteEffect = PennyWhiteEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

pennyWhiteEffect :: EffectArgs -> PennyWhiteEffect
pennyWhiteEffect = cardEffect PennyWhiteEffect Cards.pennyWhite

instance RunMessage PennyWhiteEffect where
  runMessage msg e@(PennyWhiteEffect attrs) = runQueueT $ case msg of
    BeginTurn iid | InvestigatorTarget iid == attrs.target -> do
      disable attrs
      push $ GainActions iid (ChaosTokenEffectSource ElderSign) 1
      pure e
    _ -> PennyWhiteEffect <$> liftRunMessage msg attrs
