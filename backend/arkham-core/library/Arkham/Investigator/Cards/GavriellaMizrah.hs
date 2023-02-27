module Arkham.Investigator.Cards.GavriellaMizrah
  ( gavriellaMizrah
  , GavriellaMizrah(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Uses
import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Draw.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype GavriellaMizrah = GavriellaMizrah InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gavriellaMizrah :: InvestigatorCard GavriellaMizrah
gavriellaMizrah = investigatorWith
  GavriellaMizrah
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
   .~ [ toCardDef $ Cards.setUses Cards.fortyFiveAutomatic (Uses Ammo 2)
      , toCardDef Cards.physicalTraining
      ]
   )
  . (startsWithInHandL
    .~ [ toCardDef Cards.firstAid
       , toCardDef Cards.guardDog
       , toCardDef Cards.evidence
       , toCardDef Cards.dodge
       , toCardDef Cards.extraAmmunition1
       , toCardDef Cards.delayTheInevitable
       , toCardDef Cards.delayTheInevitable
       ]
    )
  )

instance HasModifiersFor GavriellaMizrah where
  getModifiersFor target (GavriellaMizrah a) | isTarget a target =
    pure $ toModifiers
      a
      [ CannotTakeAction (IsAction Action.Draw)
      , CannotDrawCards
      , CannotManipulateDeck
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities GavriellaMizrah where
  getAbilities (GavriellaMizrah a) =
    [ restrictedAbility a 1 Self $ ReactionAbility
        (EnemyAttacksEvenIfCancelled Timing.After You AnyEnemyAttack AnyEnemy)
        Free
    ]

instance HasTokenValue GavriellaMizrah where
  getTokenValue iid ElderSign (GavriellaMizrah attrs) | iid == toId attrs = do
    pure $ TokenValue ElderSign $ PositiveModifier 1
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance RunMessage GavriellaMizrah where
  runMessage msg i@(GavriellaMizrah attrs) = case msg of
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
      push $ RemovedFromGame card
      pure i
    Do (DiscardCard iid _ _) | iid == toId attrs -> do
      pure i
    DrawCards cardDraw | cardDrawInvestigator cardDraw == toId attrs -> do
      pure i
    _ -> GavriellaMizrah <$> runMessage msg attrs
