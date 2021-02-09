module Arkham.Types.Investigator.Cards.SkidsOToole
  ( SkidsOToole(..)
  , skidsOToole
  ) where


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

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

instance ActionRunner env => HasActions env SkidsOToole where
  getActions iid (DuringTurn You) (SkidsOToole a@InvestigatorAttrs {..})
    | iid == investigatorId = pure [ActivateCardAbilityAction iid (ability a)]
  getActions _ _ _ = pure []

instance HasTokenValue env SkidsOToole where
  getTokenValue (SkidsOToole attrs) iid ElderSign
    | iid == investigatorId attrs = pure
    $ TokenValue ElderSign (PositiveModifier 2)
  getTokenValue (SkidsOToole attrs) iid token = getTokenValue attrs iid token

instance (InvestigatorRunner env) => RunMessage env SkidsOToole where
  runMessage msg i@(SkidsOToole attrs@InvestigatorAttrs {..}) = case msg of
    UseCardAbility _ (InvestigatorSource iid) _ 1 _ | iid == investigatorId ->
      pure . SkidsOToole $ attrs & remainingActionsL +~ 1
    PassedSkillTest iid _ _ (DrawnTokenTarget token) _ _
      | iid == investigatorId && drawnTokenFace token == ElderSign -> i
      <$ unshiftMessage (TakeResources iid 2 False)
    _ -> SkidsOToole <$> runMessage msg attrs
