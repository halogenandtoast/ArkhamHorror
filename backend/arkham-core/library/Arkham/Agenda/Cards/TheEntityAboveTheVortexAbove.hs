module Arkham.Agenda.Cards.TheEntityAboveTheVortexAbove
  ( TheEntityAboveTheVortexAbove(..)
  , theEntityAboveTheVortexAbove
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Cost
import Arkham.Enemy.Types ( Field (EnemyTraits) )
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Target
import Arkham.Trait

newtype TheEntityAboveTheVortexAbove = TheEntityAboveTheVortexAbove AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEntityAboveTheVortexAbove :: AgendaCard TheEntityAboveTheVortexAbove
theEntityAboveTheVortexAbove = agenda
  (2, C)
  TheEntityAboveTheVortexAbove
  Cards.theEntityAboveTheVortexAbove
  (Static 6)

instance HasModifiersFor TheEntityAboveTheVortexAbove where
  getModifiersFor (EnemyTarget eid) (TheEntityAboveTheVortexAbove a) = do
    isMonster <- fieldP EnemyTraits (member Monster) eid
    pure $ toModifiers a [ EnemyFight 1 | isMonster ]
  getModifiersFor _ _ = pure []

instance HasAbilities TheEntityAboveTheVortexAbove where
  getAbilities (TheEntityAboveTheVortexAbove a) =
    [ limitedAbility (GroupLimit PerRound 1)
        $ mkAbility a 1
        $ FastAbility
        $ GroupClueCost (PerPlayer 1) Anywhere
    ]

instance RunMessage TheEntityAboveTheVortexAbove where
  runMessage msg a@(TheEntityAboveTheVortexAbove attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide D attrs -> do
      pushAll
        [Discard (AgendaTarget $ toId attrs), AddAct Acts.openThePathAbove]
      pure a
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      investigatorIds <- getInvestigatorIds
      pushAll
        $ [PlaceDoom (toTarget attrs) 1, AdvanceAgendaIfThresholdSatisfied]
        <> [ DrawCards iid 1 False | iid <- investigatorIds ]
      pure a
    _ -> TheEntityAboveTheVortexAbove <$> runMessage msg attrs
