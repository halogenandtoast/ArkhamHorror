module Arkham.Types.Investigator.Cards.SkidsOToole
  ( SkidsOToole(..)
  , skidsOToole
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.Window

newtype SkidsOToole = SkidsOToole InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env SkidsOToole where
  getModifiersFor source target (SkidsOToole attrs) =
    getModifiersFor source target attrs

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

ability :: InvestigatorAttrs -> Ability
ability attrs = base { abilityLimit = PlayerLimit PerTurn 1 }
  where base = mkAbility (toSource attrs) 1 (FastAbility $ ResourceCost 2)

instance HasAbilities env SkidsOToole where
  getAbilities iid (DuringTurn who) (SkidsOToole a@InvestigatorAttrs {..})
    | iid == investigatorId && iid == who = pure [ability a]
  getAbilities _ _ _ = pure []

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
