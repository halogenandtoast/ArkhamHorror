module Arkham.Investigator.Cards.JeromeDavids (
  jeromeDavids,
  JeromeDavids (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards hiding (jeromeDavids)
import Arkham.Card
import Arkham.Draw.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype JeromeDavids = JeromeDavids (InvestigatorAttrs `With` PrologueMetadata)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (IsInvestigator, ToJSON, FromJSON)
  deriving newtype (Entity)

jeromeDavids :: PrologueMetadata -> InvestigatorCard JeromeDavids
jeromeDavids meta =
  investigatorWith
    (JeromeDavids . (`with` meta))
    Cards.jeromeDavids
    (Stats {health = 4, sanity = 8, willpower = 2, intellect = 4, combat = 1, agility = 3})
    $ startsWithInHandL
      .~ [ Cards.hyperawareness
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

instance HasModifiersFor JeromeDavids where
  getModifiersFor target (JeromeDavids (a `With` _)) | isTarget a target = do
    pure
      $ toModifiersWith
        a
        setActiveDuringSetup
        [ CannotTakeAction (IsAction Action.Draw)
        , CannotDrawCards
        , CannotManipulateDeck
        , StartingResources (-2)
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities JeromeDavids where
  getAbilities (JeromeDavids (a `With` _)) =
    [ playerLimit PerRound
        $ restrictedAbility a 1 Self
        $ ReactionAbility
          ( DrawCard
              Timing.When
              (InvestigatorAt YourLocation)
              (BasicCardMatch $ CardWithType TreacheryType)
              EncounterDeck
          )
          (SkillIconCost 2 $ singleton (SkillIcon SkillIntellect))
    ]

instance HasChaosTokenValue JeromeDavids where
  getChaosTokenValue iid ElderSign (JeromeDavids (attrs `With` _)) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 1
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage JeromeDavids where
  runMessage msg i@(JeromeDavids (attrs `With` meta)) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ CancelNext (toSource attrs) RevelationMessage
      pure i
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      push $ InvestigatorDiscoverCluesAtTheirLocation iid (ChaosTokenEffectSource ElderSign) 1 Nothing
      pure i
    DrawStartingHand iid | iid == toId attrs -> pure i
    InvestigatorMulligan iid | iid == toId attrs -> do
      push $ FinishedWithMulligan iid
      pure i
    AddToDiscard iid pc | iid == toId attrs -> do
      push $ RemovedFromGame (PlayerCard pc)
      pure i
    DiscardCard iid _ cardId | iid == toId attrs -> do
      let card = fromJustNote "must be in hand" $ find ((== cardId) . toCardId) (investigatorHand attrs)
      pushAll [RemoveCardFromHand iid cardId, RemovedFromGame card]
      pure i
    Do (DiscardCard iid _ _) | iid == toId attrs -> do
      pure i
    DrawCards cardDraw | cardDrawInvestigator cardDraw == toId attrs -> do
      pure i
    _ -> JeromeDavids . (`with` meta) <$> runMessage msg attrs
