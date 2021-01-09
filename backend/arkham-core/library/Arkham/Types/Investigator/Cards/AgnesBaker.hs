module Arkham.Types.Investigator.Cards.AgnesBaker
  ( AgnesBaker(..)
  , agnesBaker
  ) where

import Arkham.Import

import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype AgnesBaker = AgnesBaker Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance HasModifiersFor env AgnesBaker where
  getModifiersFor source target (AgnesBaker attrs) =
    getModifiersFor source target attrs

agnesBaker :: AgnesBaker
agnesBaker = AgnesBaker
  $ baseAttrs "01004" "Agnes Baker" Mystic stats [Sorcerer]
 where
  stats = Stats
    { health = 6
    , sanity = 8
    , willpower = 5
    , intellect = 2
    , combat = 2
    , agility = 3
    }

ability :: Attrs -> Ability
ability attrs = base { abilityLimit = PlayerLimit PerPhase 1 }
  where base = mkAbility (toSource attrs) 1 (ReactionAbility Free)

instance ActionRunner env => HasActions env AgnesBaker where
  getActions iid (AfterAssignedHorror You) (AgnesBaker attrs)
    | iid == toId attrs = do
      enemyIds <- getSet @EnemyId $ investigatorLocation attrs
      pure
        [ ActivateCardAbilityAction iid (ability attrs) | not (null enemyIds) ]
  getActions i window (AgnesBaker attrs) = getActions i window attrs

instance HasTokenValue env AgnesBaker where
  getTokenValue (AgnesBaker attrs) iid ElderSign | iid == toId attrs = do
    let tokenValue' = PositiveModifier $ investigatorSanityDamage attrs
    pure $ TokenValue ElderSign tokenValue'
  getTokenValue (AgnesBaker attrs) iid token = getTokenValue attrs iid token

instance (InvestigatorRunner env) => RunMessage env AgnesBaker where
  runMessage msg i@(AgnesBaker attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      enemyIds <- getSetList $ investigatorLocation attrs
      i <$ unshiftMessage
        (chooseOne
          (toId attrs)
          [ EnemyDamage eid (toId attrs) source 1 | eid <- enemyIds ]
        )
    _ -> AgnesBaker <$> runMessage msg attrs
