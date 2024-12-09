module Arkham.Investigator.Cards.JeromeDavids (jeromeDavids, JeromeDavids (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards hiding (jeromeDavids)
import Arkham.Card
import Arkham.Discover hiding (discoverAtYourLocation)
import Arkham.Event.Cards qualified as Cards
import Arkham.Helpers.Investigator (startsWithInHand)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWith, setActiveDuringSetup)
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards

newtype JeromeDavids = JeromeDavids InvestigatorAttrs
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (IsInvestigator, ToJSON, FromJSON)
  deriving newtype Entity

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
  getModifiersFor (JeromeDavids a) =
    modifySelfWith
      a
      setActiveDuringSetup
      [CannotTakeAction #draw, CannotDrawCards, CannotManipulateDeck, StartingResources (-2)]

instance HasAbilities JeromeDavids where
  getAbilities (JeromeDavids a) =
    [ playerLimit PerRound
        $ restricted a 1 Self
        $ ReactionAbility
          (DrawCard #when (InvestigatorAt YourLocation) #treachery EncounterDeck)
          (SkillIconCost 2 $ singleton #intellect)
    ]

instance HasChaosTokenValue JeromeDavids where
  getChaosTokenValue iid ElderSign (JeromeDavids attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 1
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage JeromeDavids where
  runMessage msg i@(JeromeDavids attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      cardResolutionModifier card (attrs.ability 1) card IgnoreRevelation
      pure i
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      discoverAtYourLocation NotInvestigate iid ElderSign 1
      pure i
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
    DrawCards iid cardDraw | iid == attrs.id && cardDraw.isPlayerDraw -> pure i
    _ -> JeromeDavids <$> liftRunMessage msg attrs
