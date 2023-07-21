module Arkham.Act.Cards.TheStrangerACityAflame (
  TheStrangerACityAflame (..),
  theStrangerACityAflame,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher hiding (Discarded)
import Arkham.Message
import Arkham.Scenarios.CurtainCall.Helpers
import Arkham.Timing qualified as Timing

newtype TheStrangerACityAflame = TheStrangerACityAflame ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerACityAflame :: ActCard TheStrangerACityAflame
theStrangerACityAflame =
  act (2, A) TheStrangerACityAflame Cards.theStrangerACityAflame Nothing

instance HasAbilities TheStrangerACityAflame where
  getAbilities (TheStrangerACityAflame a) =
    [ mkAbility a 1 $
        Objective $
          ForcedAbility $
            EnemyWouldBeDiscarded Timing.When $
              enemyIs Enemies.theManInThePallidMask
    ]

instance RunMessage TheStrangerACityAflame where
  runMessage msg a@(TheStrangerACityAflame attrs) = case msg of
    UseCardAbility _ source 1 _ _
      | isSource attrs source ->
          a <$ push (AdvanceAct (toId attrs) source AdvancedWithOther)
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      moveTheManInThePalidMaskToLobbyInsteadOfDiscarding
      theatre <-
        fromJustNote "theatre must be in play"
          <$> selectOne (LocationWithTitle "Theatre")
      card <- flipCard <$> genCard (toCardDef attrs)
      pushAll
        [ AddChaosToken Cultist
        , AddChaosToken Cultist
        , PlaceHorror (toSource attrs) (toTarget theatre) 1
        , PlaceNextTo ActDeckTarget [card]
        , CreateEffect "03047a" Nothing (toSource attrs) (toTarget attrs)
        , advanceActDeck attrs
        ]
      pure a
    _ -> TheStrangerACityAflame <$> runMessage msg attrs
