module Arkham.Types.Act.Cards.TheStrangerTheShoresOfHali
  ( TheStrangerTheShoresOfHali(..)
  , theStrangerTheShoresOfHali
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Scenarios.CurtainCall.Helpers
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Matcher hiding (Discarded)
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Token
import Arkham.Types.Trait

newtype TheStrangerTheShoresOfHali = TheStrangerTheShoresOfHali ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerTheShoresOfHali :: ActCard TheStrangerTheShoresOfHali
theStrangerTheShoresOfHali =
  act (2, A) TheStrangerTheShoresOfHali Cards.theStrangerTheShoresOfHali Nothing

instance HasAbilities TheStrangerTheShoresOfHali where
  getAbilities (TheStrangerTheShoresOfHali a) =
    [ mkAbility a 1
        $ Objective
        $ ForcedAbility
        $ EnemyWouldBeDiscarded Timing.When
        $ enemyIs Enemies.theManInThePallidMask
    ]

instance ActRunner env => RunMessage env TheStrangerTheShoresOfHali where
  runMessage msg a@(TheStrangerTheShoresOfHali attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source)
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs -> do
      moveTheManInThePalidMaskToLobbyInsteadOfDiscarding
      privateLocations <- selectListMap LocationTarget
        $ LocationWithTrait Private
      card <- flipCard <$> genCard (toCardDef attrs)
      a <$ pushAll
        ([AddToken ElderThing, AddToken ElderThing]
        <> map (`PlaceHorror` 1) privateLocations
        <> [ CreateEffect "03047c" Nothing (toSource attrs) (toTarget attrs)
           , PlaceNextTo ActDeckTarget [card]
           , AdvanceActDeck (actDeckId attrs) (toSource attrs)
           ]
        )
    _ -> TheStrangerTheShoresOfHali <$> runMessage msg attrs
