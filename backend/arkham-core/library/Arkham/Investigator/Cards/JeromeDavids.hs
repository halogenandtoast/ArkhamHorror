module Arkham.Investigator.Cards.JeromeDavids (
  jeromeDavids,
  JeromeDavids (..),
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards hiding (jeromeDavids)
import Arkham.Card
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards

newtype JeromeDavids = JeromeDavids InvestigatorAttrs
  deriving stock (Show, Eq, Generic)
  deriving anyclass (IsInvestigator, ToJSON, FromJSON)
  deriving newtype (Entity)

jeromeDavids :: InvestigatorCard JeromeDavids
jeromeDavids =
  startsWithInHand
    [ Cards.hyperawareness
    , Cards.mindOverMatter
    , Cards.workingAHunch
    , Cards.barricade
    , Cards.deduction
    , Cards.magnifyingGlass1
    , Cards.fingerprintKit
    , Cards.connectTheDots
    , Cards.curiosity
    , Cards.curiosity
    ]
    $ investigator JeromeDavids Cards.jeromeDavids
    $ Stats {health = 4, sanity = 8, willpower = 2, intellect = 4, combat = 1, agility = 3}

instance HasModifiersFor JeromeDavids where
  getModifiersFor target (JeromeDavids a) | a `is` target = do
    pure
      $ toModifiersWith a setActiveDuringSetup
      $ [CannotTakeAction #draw, CannotDrawCards, CannotManipulateDeck, StartingResources (-2)]
  getModifiersFor _ _ = pure []

instance HasAbilities JeromeDavids where
  getAbilities (JeromeDavids a) =
    [ playerLimit PerRound
        $ restrictedAbility a 1 Self
        $ ReactionAbility
          (DrawCard #when (InvestigatorAt YourLocation) #treachery EncounterDeck)
          (SkillIconCost 2 $ singleton #intellect)
    ]

instance HasChaosTokenValue JeromeDavids where
  getChaosTokenValue iid ElderSign (JeromeDavids attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 1
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage JeromeDavids where
  runMessage msg i@(JeromeDavids attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ CancelNext (toSource attrs) RevelationMessage
      pure i
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      push $ Msg.DiscoverClues iid $ discoverAtYourLocation ElderSign 1
      pure i
    DrawStartingHand iid | attrs `is` iid -> pure i
    InvestigatorMulligan iid | attrs `is` iid -> do
      push $ FinishedWithMulligan iid
      pure i
    AddToDiscard iid pc | attrs `is` iid -> do
      push $ RemovedFromGame (PlayerCard pc)
      pure i
    DiscardCard iid _ cardId | attrs `is` iid -> do
      hand <- field InvestigatorHand iid
      let card = fromJustNote "must be in hand" $ find @[Card] ((== cardId) . toCardId) hand
      pushAll [RemoveCardFromHand iid cardId, RemovedFromGame card]
      pure i
    Do (DiscardCard iid _ _) | attrs `is` iid -> pure i
    DrawCards cardDraw | attrs `is` cardDraw.investigator -> pure i
    _ -> JeromeDavids <$> runMessage msg attrs
