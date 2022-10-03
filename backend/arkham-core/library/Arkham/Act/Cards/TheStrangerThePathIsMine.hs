module Arkham.Act.Cards.TheStrangerThePathIsMine
  ( TheStrangerThePathIsMine(..)
  , theStrangerThePathIsMine
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Types
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher hiding (Discarded)
import Arkham.Message
import Arkham.Scenarios.CurtainCall.Helpers
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token

newtype TheStrangerThePathIsMine = TheStrangerThePathIsMine ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
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

instance RunMessage TheStrangerThePathIsMine where
  runMessage msg a@(TheStrangerThePathIsMine attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source AdvancedWithOther)
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      theManInThePallidMask <- getTheManInThePallidMask
      moveTheManInThePalidMaskToLobbyInsteadOfDiscarding
      mlid <- selectOne $ LocationWithEnemy $ EnemyWithId theManInThePallidMask
      card <- flipCard <$> genCard (toCardDef attrs)
      for_ mlid $ \lid ->
        pushAll
          [ AddToken Tablet
          , AddToken Tablet
          , PlaceHorror (LocationTarget lid) 1
          , PlaceNextTo ActDeckTarget [card]
          , CreateEffect "03047b" Nothing (toSource attrs) (toTarget attrs)
          , AdvanceActDeck (actDeckId attrs) (toSource attrs)
          ]
      pure a
    _ -> TheStrangerThePathIsMine <$> runMessage msg attrs
