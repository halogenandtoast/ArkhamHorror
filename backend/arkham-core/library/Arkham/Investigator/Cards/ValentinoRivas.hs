module Arkham.Investigator.Cards.ValentinoRivas (
  valentinoRivas,
  ValentinoRivas (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards hiding (valentinoRivas)
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards

newtype ValentinoRivas = ValentinoRivas InvestigatorAttrs
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (IsInvestigator, ToJSON, FromJSON)
  deriving newtype Entity

valentinoRivas :: InvestigatorCard ValentinoRivas
valentinoRivas =
  investigatorWith
    ValentinoRivas
    Cards.valentinoRivas
    (Stats {health = 5, sanity = 7, willpower = 1, intellect = 3, combat = 2, agility = 4})
    $ ( startsWithL
          .~ [Cards.wellConnected]
      )
    . ( startsWithInHandL
          .~ [ Cards.fortyOneDerringer
             , Cards.opportunist
             , Cards.sureGamble3
             , Cards.moneyTalks
             , Cards.moneyTalks
             , Cards.cunning
             , Cards.cunning
             ]
      )

instance HasModifiersFor ValentinoRivas where
  getModifiersFor target (ValentinoRivas a) | isTarget a target = do
    pure
      $ toModifiersWith
        a
        setActiveDuringSetup
        [ CannotTakeAction (IsAction Action.Draw)
        , CannotDrawCards
        , CannotManipulateDeck
        , StartingResources 5
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities ValentinoRivas where
  getAbilities (ValentinoRivas a) =
    [ playerLimit PerRound
        $ restrictedAbility a 1 (Self <> DuringSkillTest (YourSkillTest AnySkillTest))
        $ FastAbility
        $ ResourceCost 2
    ]

instance HasChaosTokenValue ValentinoRivas where
  getChaosTokenValue iid ElderSign (ValentinoRivas attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 1
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage ValentinoRivas where
  runMessage msg i@(ValentinoRivas attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ skillTestModifier (toSource attrs) SkillTestTarget (Difficulty (-1))
      pure i
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      push $ TakeResources iid 2 (ChaosTokenEffectSource ElderSign) False
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
    _ -> ValentinoRivas <$> runMessage msg attrs
