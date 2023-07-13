module Arkham.Act.Cards.TheStrangerTheShoresOfHali (
  TheStrangerTheShoresOfHali (..),
  theStrangerTheShoresOfHali,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher hiding (Discarded)
import Arkham.Message
import Arkham.Scenarios.CurtainCall.Helpers
import Arkham.Timing qualified as Timing
import Arkham.ChaosToken
import Arkham.Trait

newtype TheStrangerTheShoresOfHali = TheStrangerTheShoresOfHali ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerTheShoresOfHali :: ActCard TheStrangerTheShoresOfHali
theStrangerTheShoresOfHali =
  act (2, A) TheStrangerTheShoresOfHali Cards.theStrangerTheShoresOfHali Nothing

instance HasAbilities TheStrangerTheShoresOfHali where
  getAbilities (TheStrangerTheShoresOfHali a) =
    [ mkAbility a 1 $
        Objective $
          ForcedAbility $
            EnemyWouldBeDiscarded Timing.When $
              enemyIs Enemies.theManInThePallidMask
    ]

instance RunMessage TheStrangerTheShoresOfHali where
  runMessage msg a@(TheStrangerTheShoresOfHali attrs) = case msg of
    UseCardAbility _ source 1 _ _
      | isSource attrs source ->
          a <$ push (AdvanceAct (toId attrs) source AdvancedWithOther)
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      moveTheManInThePalidMaskToLobbyInsteadOfDiscarding
      privateLocations <-
        selectListMap LocationTarget $
          LocationWithTrait Private
      card <- flipCard <$> genCard (toCardDef attrs)
      a
        <$ pushAll
          ( [AddChaosToken ElderThing, AddChaosToken ElderThing]
              <> map (\l -> PlaceHorror (toSource attrs) l 1) privateLocations
              <> [ CreateEffect "03047c" Nothing (toSource attrs) (toTarget attrs)
                 , PlaceNextTo ActDeckTarget [card]
                 , AdvanceActDeck (actDeckId attrs) (toSource attrs)
                 ]
          )
    _ -> TheStrangerTheShoresOfHali <$> runMessage msg attrs
