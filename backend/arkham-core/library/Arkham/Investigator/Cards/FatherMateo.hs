module Arkham.Investigator.Cards.FatherMateo
  ( fatherMateo
  , FatherMateo(..)
  )
where

import Arkham.Prelude

import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner

newtype FatherMateo = FatherMateo InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fatherMateo :: InvestigatorCard FatherMateo
fatherMateo = investigator
  FatherMateo
  Cards.fatherMateo
  Stats
    { health = 6
    , sanity = 8
    , willpower = 4
    , intellect = 3
    , combat = 2
    , agility = 3
    }

instance HasAbilities FatherMateo where
  getAbilities (FatherMateo _) = []

instance HasTokenValue FatherMateo where
  getTokenValue iid ElderSign (FatherMateo attrs) | iid == toId attrs = do
    pure $ TokenValue ElderSign AutoSuccessModifier
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance RunMessage FatherMateo where
  runMessage msg (FatherMateo attrs) = case msg of
    ResolveToken _drawnToken token _iid | token == ElderSign ->
      -- TODO: This should not need to resolve, we should instead handle the modifier
      -- See also Scenario/Runner.hs:191-193
      a <$ push PassSkillTest
    _ -> FatherMateo <$> runMessage msg attrs
