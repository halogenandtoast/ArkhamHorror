module Arkham.Types.Investigator.Cards.SkidsOToole
  ( SkidsOToole(..)
  , skidsOToole
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Matcher hiding (DuringTurn)
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait

newtype SkidsOToole = SkidsOToole InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

skidsOToole :: SkidsOToole
skidsOToole = SkidsOToole $ baseAttrs
  "01003"
  "\"Skids\" O'Toole"
  Rogue
  Stats
    { health = 8
    , sanity = 6
    , willpower = 2
    , intellect = 3
    , combat = 3
    , agility = 4
    }
  [Criminal]

instance HasAbilities env SkidsOToole where
  getAbilities _ _ (SkidsOToole a) = pure
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
  getTokenValue (SkidsOToole attrs) iid token = getTokenValue attrs iid token

instance InvestigatorRunner env => RunMessage env SkidsOToole where
  runMessage msg i@(SkidsOToole attrs@InvestigatorAttrs {..}) = case msg of
    UseCardAbility _ (InvestigatorSource iid) _ 1 _ | iid == investigatorId ->
      pure . SkidsOToole $ attrs & remainingActionsL +~ 1
    PassedSkillTest iid _ _ (TokenTarget token) _ _
      | iid == investigatorId && tokenFace token == ElderSign -> i
      <$ push (TakeResources iid 2 False)
    _ -> SkidsOToole <$> runMessage msg attrs
