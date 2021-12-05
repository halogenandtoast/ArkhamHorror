module Arkham.Types.Investigator.Cards.SkidsOToole
  ( SkidsOToole(..)
  , skidsOToole
  ) where

import Arkham.Prelude

import Arkham.Investigator.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Matcher hiding (DuringTurn)
import Arkham.Types.Message
import Arkham.Types.Target

newtype SkidsOToole = SkidsOToole InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

skidsOToole :: InvestigatorCard SkidsOToole
skidsOToole = investigator
  SkidsOToole
  Cards.skidsOToole
  Stats
    { health = 8
    , sanity = 6
    , willpower = 2
    , intellect = 3
    , combat = 3
    , agility = 4
    }

instance HasAbilities SkidsOToole where
  getAbilities (SkidsOToole a) =
    [ restrictedAbility
          a
          1
          (Self <> DuringTurn You)
          (FastAbility $ ResourceCost 2)
        & (abilityLimitL .~ PlayerLimit PerTurn 1)
    ]

instance HasTokenValue env SkidsOToole where
  getTokenValue (SkidsOToole attrs) iid ElderSign
    | iid == investigatorId attrs = pure
    $ TokenValue ElderSign (PositiveModifier 2)
  getTokenValue _ _ token = pure $ TokenValue token mempty

instance InvestigatorRunner env => RunMessage env SkidsOToole where
  runMessage msg i@(SkidsOToole attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      i <$ push (GainActions (toId attrs) source 1)
    PassedSkillTest iid _ _ (TokenTarget token) _ _
      | iid == toId attrs && tokenFace token == ElderSign -> i
      <$ push (TakeResources iid 2 False)
    _ -> SkidsOToole <$> runMessage msg attrs
