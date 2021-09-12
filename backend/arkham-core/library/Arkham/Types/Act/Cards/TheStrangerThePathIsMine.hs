module Arkham.Types.Act.Cards.TheStrangerThePathIsMine
  ( TheStrangerThePathIsMine(..)
  , theStrangerThePathIsMine
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Enemy.Cards as Enemies
import Arkham.Scenarios.CurtainCall.Helpers
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Card.CardDef
import Arkham.Types.Classes
import Arkham.Types.Matcher hiding (Discarded)
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Token

newtype TheStrangerThePathIsMine = TheStrangerThePathIsMine ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerThePathIsMine :: ActCard TheStrangerThePathIsMine
theStrangerThePathIsMine =
  act (2, A) TheStrangerThePathIsMine Cards.theStrangerThePathIsMine Nothing

instance HasAbilities TheStrangerThePathIsMine where
  getAbilities (TheStrangerThePathIsMine a) =
    [ mkAbility a 1
        $ Objective
        $ ForcedAbility
        $ EnemyWouldBeDiscarded Timing.When
        $ enemyIs Enemies.theManInThePallidMask
    ]

instance ActRunner env => RunMessage env TheStrangerThePathIsMine where
  runMessage msg a@(TheStrangerThePathIsMine attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source)
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs -> do
      theManInThePallidMask <- getTheManInThePallidMask
      moveTheManInThePalidMaskToLobbyInsteadOfDiscarding
      lid <- getId theManInThePallidMask
      a <$ pushAll
        [ AddToken Tablet
        , AddToken Tablet
        , PlaceHorror (LocationTarget lid) 1
        , PlaceNextTo ActDeckTarget [toCardDef attrs]
        , CreateEffect "03047b" Nothing (toSource attrs) (toTarget attrs)
        , NextAct aid "03048"
        ]
    _ -> TheStrangerThePathIsMine <$> runMessage msg attrs
