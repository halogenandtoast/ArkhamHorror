module Arkham.Agenda.Cards.OverTheThreshold
  ( OverTheThreshold(..)
  , overTheThreshold
  ) where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Trait (toTraits, Trait(SilverTwilight))

newtype OverTheThreshold = OverTheThreshold AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor OverTheThreshold where
  getModifiersFor (EnemyTarget eid) (OverTheThreshold a) = do
    isSilverTwilight <- eid <=~> EnemyWithTrait SilverTwilight
    pure $ toModifiers
      a
      [ CountsAsInvestigatorForHunterEnemies | isSilverTwilight ]
  getModifiersFor (CardIdTarget cid) (OverTheThreshold a) = do
    card <- getCard cid
    let isSilverTwilight = SilverTwilight `member` toTraits card
    pure $ toModifiers a [ GainVictory 0 | isSilverTwilight ]
  getModifiersFor _ _ = pure []


overTheThreshold :: AgendaCard OverTheThreshold
overTheThreshold =
  agenda (2, A) OverTheThreshold Cards.overTheThreshold (Static 11)

instance RunMessage OverTheThreshold where
  runMessage msg a@(OverTheThreshold attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> OverTheThreshold <$> runMessage msg attrs
