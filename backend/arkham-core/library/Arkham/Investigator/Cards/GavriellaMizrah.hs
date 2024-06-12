module Arkham.Investigator.Cards.GavriellaMizrah (
  gavriellaMizrah,
  GavriellaMizrah (..),
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards hiding (gavriellaMizrah)
import Arkham.Card
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards

newtype GavriellaMizrah = GavriellaMizrah InvestigatorAttrs
  deriving stock (Show, Eq, Generic)
  deriving anyclass (IsInvestigator, ToJSON, FromJSON)
  deriving newtype (Entity)

gavriellaMizrah :: InvestigatorCard GavriellaMizrah
gavriellaMizrah =
  startsWith [Cards.fortyFiveAutomatic, Cards.physicalTraining, Cards.fateOfAllFools]
    $ startsWithInHand
      [ Cards.firstAid
      , Cards.guardDog
      , Cards.evidence
      , Cards.dodge
      , Cards.extraAmmunition1
      , Cards.delayTheInevitable
      , Cards.delayTheInevitable
      ]
    $ investigator GavriellaMizrah Cards.gavriellaMizrah
    $ Stats {health = 8, sanity = 4, willpower = 3, intellect = 2, combat = 4, agility = 1}

instance HasModifiersFor GavriellaMizrah where
  getModifiersFor target (GavriellaMizrah a) | a `isTarget` target = do
    pure
      $ toModifiersWith a setActiveDuringSetup
      $ [CannotTakeAction #draw, CannotDrawCards, CannotManipulateDeck, StartingResources (-4)]
  getModifiersFor (AssetTarget aid) (GavriellaMizrah a) = do
    isFortyFiveAutomatic <- aid <=~> assetIs Cards.fortyFiveAutomatic
    pure $ toModifiersWith a setActiveDuringSetup [AdditionalStartingUses (-2) | isFortyFiveAutomatic]
  getModifiersFor _ _ = pure []

instance HasAbilities GavriellaMizrah where
  getAbilities (GavriellaMizrah a) =
    [ restrictedAbility a 1 (Self <> ClueOnLocation)
        $ freeReaction (EnemyAttacksEvenIfCancelled #after You AnyEnemyAttack AnyEnemy)
    ]

instance HasChaosTokenValue GavriellaMizrah where
  getChaosTokenValue iid ElderSign (GavriellaMizrah attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 1
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage GavriellaMizrah where
  runMessage msg i@(GavriellaMizrah attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Msg.DiscoverClues iid $ discoverAtYourLocation (toAbilitySource attrs 1) 1
      pure i
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      pushAll
        [HealHorror (toTarget attrs) (toSource attrs) 1, HealDamage (toTarget attrs) (toSource attrs) 1]
      pure i
    DrawStartingHand iid | attrs `is` iid -> pure i
    InvestigatorMulligan iid | iid == toId attrs -> do
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
    _ -> GavriellaMizrah <$> runMessage msg attrs
