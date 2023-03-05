module Arkham.Investigator.Cards.ValentinoRivas
  ( valentinoRivas
  , ValentinoRivas(..)
  ) where

import Arkham.Prelude

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
import Arkham.Skill.Cards qualified as Cards
import Arkham.Source

newtype ValentinoRivas = ValentinoRivas (InvestigatorAttrs `With` PrologueMetadata)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (IsInvestigator, ToJSON, FromJSON)
  deriving newtype Entity

valentinoRivas :: PrologueMetadata -> InvestigatorCard ValentinoRivas
valentinoRivas meta = investigatorWith
  (ValentinoRivas . (`with` meta))
  Cards.valentinoRivas
  Stats
    { health = 5
    , sanity = 7
    , willpower = 1
    , intellect = 3
    , combat = 2
    , agility = 4
    }
  ((startsWithL
   .~ [Cards.wellConnected]
   )
  .(startsWithInHandL
   .~ [ Cards.fortyOneDerringer
      , Cards.opportunist
      , Cards.sureGamble3
      , Cards.moneyTalks
      , Cards.moneyTalks
      , Cards.cunning
      , Cards.cunning
      ]
   )
  )

instance HasModifiersFor ValentinoRivas where
  getModifiersFor target (ValentinoRivas (a `With` _)) | isTarget a target =
    pure $ toModifiersWith
      a
      setActiveDuringSetup
      [ CannotTakeAction (IsAction Action.Draw)
      , CannotDrawCards
      , CannotManipulateDeck
      , StartingResources 5
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities ValentinoRivas where
  getAbilities (ValentinoRivas (a `With` _)) =
    [ limitedAbility (PlayerLimit PerRound 1)
        $ restrictedAbility a 1 (Self <> DuringSkillTest (YourSkillTest AnySkillTest))
        $ FastAbility $ ResourceCost 2
    ]

instance HasTokenValue ValentinoRivas where
  getTokenValue iid ElderSign (ValentinoRivas (attrs `With` _))
    | iid == toId attrs = do
      pure $ TokenValue ElderSign $ PositiveModifier 1
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance RunMessage ValentinoRivas where
  runMessage msg i@(ValentinoRivas (attrs `With` meta)) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ skillTestModifier (toSource attrs) SkillTestTarget (Difficulty (-1))
      pure i
    ResolveToken _drawnToken ElderSign iid | iid == toId attrs -> do
      push $ TakeResources iid 2 (TokenEffectSource ElderSign) False
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
    _ -> ValentinoRivas . (`with` meta) <$> runMessage msg attrs
