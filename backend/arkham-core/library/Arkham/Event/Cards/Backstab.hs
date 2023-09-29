module Arkham.Event.Cards.Backstab where

import Arkham.Prelude

import Arkham.Action
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner

newtype Backstab = Backstab EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backstab :: EventCard Backstab
backstab = event Backstab Cards.backstab

instance HasModifiersFor Backstab where
  getModifiersFor (InvestigatorTarget _) (Backstab attrs) = do
    mSource <- getSkillTestSource
    mAction <- getSkillTestAction
    pure $ case (mAction, mSource) of
      (Just Fight, Just (isSource attrs -> True)) -> do
        toModifiers attrs [DamageDealt 2]
      _ -> []
  getModifiersFor _ _ = pure []

instance RunMessage Backstab where
  runMessage msg e@(Backstab attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      push $ chooseFightEnemy iid (toSource eid) #agility
      pure e
    _ -> Backstab <$> runMessage msg attrs
