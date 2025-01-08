module Arkham.Investigator.Cards.ValentinoRivas (valentinoRivas, ValentinoRivas (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards hiding (valentinoRivas)
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Helpers.Investigator (startsWithInHand)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWith, setActiveDuringSetup)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards

newtype ValentinoRivas = ValentinoRivas InvestigatorAttrs
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (IsInvestigator, ToJSON, FromJSON)
  deriving newtype Entity

valentinoRivas :: InvestigatorCard ValentinoRivas
valentinoRivas =
  startsWithInHand
    [ Cards.fortyOneDerringer
    , Cards.opportunist
    , Cards.sureGamble3
    , Cards.moneyTalks
    , Cards.moneyTalks
    , Cards.cunning
    , Cards.cunning
    ]
    $ startsWith [Cards.wellConnected]
    $ investigator ValentinoRivas Cards.valentinoRivas
    $ Stats {health = 5, sanity = 7, willpower = 1, intellect = 3, combat = 2, agility = 4}

instance HasModifiersFor ValentinoRivas where
  getModifiersFor (ValentinoRivas a) = do
    modifySelfWith
      a
      setActiveDuringSetup
      [ CannotTakeAction #draw
      , CannotDrawCards
      , CannotManipulateDeck
      , StartingResources 5
      ]

instance HasAbilities ValentinoRivas where
  getAbilities (ValentinoRivas a) =
    [ playerLimit PerRound
        $ restricted a 1 (Self <> DuringSkillTest (YourSkillTest AnySkillTest))
        $ FastAbility
        $ ResourceCost 2
    ]

instance HasChaosTokenValue ValentinoRivas where
  getChaosTokenValue iid ElderSign (ValentinoRivas attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 1
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage ValentinoRivas where
  runMessage msg i@(ValentinoRivas attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      withSkillTest \sid -> skillTestModifier sid (toSource attrs) sid (Difficulty (-1))
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
    _ -> ValentinoRivas <$> liftRunMessage msg attrs
