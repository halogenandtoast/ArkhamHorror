module Arkham.Investigator.Cards.GavriellaMizrah (gavriellaMizrah, GavriellaMizrah) where

import Arkham.Asset.Cards qualified as Cards hiding (gavriellaMizrah)
import Arkham.Card
import Arkham.Ability
import Arkham.Investigator.Types (Field(..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards

newtype GavriellaMizrah = GavriellaMizrah InvestigatorAttrs
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (IsInvestigator, ToJSON, FromJSON)
  deriving newtype Entity

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
  getModifiersFor (GavriellaMizrah a) = do
    modifySelfWith
      a
      setActiveDuringSetup
      [CannotTakeAction #draw, CannotDrawCards, CannotManipulateDeck, StartingResources (-4)]
    modifySelectWith
      a
      (assetIs Cards.fortyFiveAutomatic)
      setActiveDuringSetup
      [AdditionalStartingUses (-2)]

instance HasAbilities GavriellaMizrah where
  getAbilities (GavriellaMizrah a) =
    [ playerLimit PerRound
        $ restricted a 1 (Self <> ClueOnLocation)
        $ freeReaction (EnemyAttacksEvenIfCancelled #after You AnyEnemyAttack AnyEnemy)
    ]

instance HasChaosTokenValue GavriellaMizrah where
  getChaosTokenValue iid ElderSign (GavriellaMizrah attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 1
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage GavriellaMizrah where
  runMessage msg i@(GavriellaMizrah attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      pure i
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      healDamage iid ElderSign 1
      healHorror iid ElderSign 1
      pure i
    InvestigatorMulligan iid | iid == toId attrs -> do
      push $ FinishedWithMulligan iid
      pure i
    AddToDiscard iid pc | attrs `is` iid -> do
      removeCardFromGame pc
      pure i
    DiscardCard iid _ cardId | attrs `is` iid -> do
      hand <- field InvestigatorHand iid
      let card = fromJustNote "must be in hand" $ find @[Card] ((== cardId) . toCardId) hand
      removeCardFromGame card
      pure i
    Do (DiscardCard iid _ _) | attrs `is` iid -> pure i
    DrawCards iid cardDraw | iid == attrs.id && cardDraw.isPlayerDraw -> pure i
    _ -> GavriellaMizrah <$> liftRunMessage msg attrs
