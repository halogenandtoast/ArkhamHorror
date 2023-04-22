module Arkham.Investigator.Cards.GavriellaMizrah
  ( gavriellaMizrah
  , GavriellaMizrah(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Card
import Arkham.Draw.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards

newtype GavriellaMizrah = GavriellaMizrah (InvestigatorAttrs `With` PrologueMetadata)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (IsInvestigator, ToJSON, FromJSON)
  deriving newtype Entity

gavriellaMizrah :: PrologueMetadata -> InvestigatorCard GavriellaMizrah
gavriellaMizrah meta = investigatorWith
  (GavriellaMizrah . (`with` meta))
  Cards.gavriellaMizrah
  Stats
    { health = 8
    , sanity = 4
    , willpower = 3
    , intellect = 2
    , combat = 4
    , agility = 1
    }
  ((startsWithL
   .~ [Cards.fortyFiveAutomatic, Cards.physicalTraining, Cards.fateOfAllFools]
   )
  . (startsWithInHandL
    .~ [ Cards.firstAid
       , Cards.guardDog
       , Cards.evidence
       , Cards.dodge
       , Cards.extraAmmunition1
       , Cards.delayTheInevitable
       , Cards.delayTheInevitable
       ]
    )
  )

instance HasModifiersFor GavriellaMizrah where
  getModifiersFor target (GavriellaMizrah (a `With` _)) | isTarget a target =
    pure $ toModifiersWith
      a
      setActiveDuringSetup
      [ CannotTakeAction (IsAction Action.Draw)
      , CannotDrawCards
      , CannotManipulateDeck
      , StartingResources (-4)
      ]
  getModifiersFor (AssetTarget aid) (GavriellaMizrah (a `With` _)) = do
    isFortyFiveAutomatic <-
      selectAny $ AssetWithId aid <> assetIs Cards.fortyFiveAutomatic
    pure $ toModifiersWith
      a
      setActiveDuringSetup
      [ AdditionalStartingUses (-2) | isFortyFiveAutomatic ]
  getModifiersFor _ _ = pure []

instance HasAbilities GavriellaMizrah where
  getAbilities (GavriellaMizrah (a `With` _)) =
    [ restrictedAbility a 1 (Self <> ClueOnLocation) $ ReactionAbility
        (EnemyAttacksEvenIfCancelled Timing.After You AnyEnemyAttack AnyEnemy)
        Free
    ]

instance HasTokenValue GavriellaMizrah where
  getTokenValue iid ElderSign (GavriellaMizrah (attrs `With` _))
    | iid == toId attrs = do
      pure $ TokenValue ElderSign $ PositiveModifier 1
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance RunMessage GavriellaMizrah where
  runMessage msg i@(GavriellaMizrah (attrs `With` meta)) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ InvestigatorDiscoverCluesAtTheirLocation iid 1 Nothing
      pure i
    ResolveToken _drawnToken ElderSign iid | iid == toId attrs -> do
      pushAll
        [ HealHorror (toTarget attrs) (toSource attrs) 1
        , HealDamage (toTarget attrs) (toSource attrs) 1
        ]
      pure i
    DrawStartingHand iid | iid == toId attrs -> pure i
    InvestigatorMulligan iid | iid == toId attrs -> do
      push $ FinishedWithMulligan iid
      pure i
    AddToDiscard iid pc | iid == toId attrs -> do
      push $ RemovedFromGame (PlayerCard pc)
      pure i
    DiscardCard iid _ cardId | iid == toId attrs -> do
      let
        card = fromJustNote "must be in hand"
          $ find ((== cardId) . toCardId) (investigatorHand attrs)
      pushAll [RemoveCardFromHand iid cardId, RemovedFromGame card]
      pure i
    Do (DiscardCard iid _ _) | iid == toId attrs -> do
      pure i
    DrawCards cardDraw | cardDrawInvestigator cardDraw == toId attrs -> do
      pure i
    _ -> GavriellaMizrah . (`with` meta) <$> runMessage msg attrs
