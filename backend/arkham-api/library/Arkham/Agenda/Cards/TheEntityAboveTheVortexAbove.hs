module Arkham.Agenda.Cards.TheEntityAboveTheVortexAbove (
  TheEntityAboveTheVortexAbove (..),
  theEntityAboveTheVortexAbove,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Trait

newtype TheEntityAboveTheVortexAbove = TheEntityAboveTheVortexAbove AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEntityAboveTheVortexAbove :: AgendaCard TheEntityAboveTheVortexAbove
theEntityAboveTheVortexAbove =
  agenda (2, C) TheEntityAboveTheVortexAbove Cards.theEntityAboveTheVortexAbove (Static 6)

instance HasModifiersFor TheEntityAboveTheVortexAbove where
  getModifiersFor (TheEntityAboveTheVortexAbove a) =
    modifySelect a (EnemyWithTrait Monster) [EnemyFight 1]

instance HasAbilities TheEntityAboveTheVortexAbove where
  getAbilities (TheEntityAboveTheVortexAbove a) =
    [groupLimit PerRound $ mkAbility a 1 $ FastAbility $ GroupClueCost (PerPlayer 1) Anywhere]

instance RunMessage TheEntityAboveTheVortexAbove where
  runMessage msg a@(TheEntityAboveTheVortexAbove attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide D attrs -> do
      openThePathAbove <- getSetAsideCard Acts.openThePathAbove
      pushAll [toDiscard GameSource attrs, AddAct 2 openThePathAbove]
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      drawing <- map (\iid -> drawCards iid (attrs.ability 1) 1) <$> getInvestigators
      pushAll
        $ PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1
        : AdvanceAgendaIfThresholdSatisfied
        : drawing
      pure a
    _ -> TheEntityAboveTheVortexAbove <$> runMessage msg attrs
