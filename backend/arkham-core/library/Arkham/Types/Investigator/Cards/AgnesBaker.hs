{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.AgnesBaker where

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

instance ActionRunner env => HasActions env AgnesBaker where
  getActions iid (AfterAssignedHorror You) (AgnesBaker Attrs {..})
    | iid == investigatorId = do
      locationEnemyIds <-
        asks $ setToList . getSet @EnemyId investigatorLocation
      let
        ability = (mkAbility
                    (InvestigatorSource investigatorId)
                    1
                    (ReactionAbility (AfterAssignedHorror You))
                  )
          { abilityLimit = PerPhase
          }
      usedAbilities <- asks $ map unUsedAbility . getList ()
      pure
        [ ActivateCardAbilityAction investigatorId ability
        | (investigatorId, ability) `notElem` usedAbilities && not
          (null locationEnemyIds)
        ]
  getActions i window (AgnesBaker attrs) = getActions i window attrs

instance HasTokenValue env AgnesBaker where
  getTokenValue (AgnesBaker attrs) iid ElderSign | iid == investigatorId attrs =
    pure $ TokenValue
      ElderSign
      (PositiveModifier $ investigatorSanityDamage attrs)
  getTokenValue (AgnesBaker attrs) iid token = getTokenValue attrs iid token

instance (InvestigatorRunner env) => RunMessage env AgnesBaker where
  runMessage msg i@(AgnesBaker attrs@Attrs {..}) = case msg of
    UseCardAbility _ (InvestigatorSource iid) _ 1 | iid == investigatorId -> do
      lid <- asks (getId @LocationId investigatorId)
      locationEnemyIds <- asks $ setToList . getSet lid
      i <$ unshiftMessage
        (Ask iid $ ChooseOne
          [ EnemyDamage eid iid (InvestigatorSource investigatorId) 1
          | eid <- locationEnemyIds
          ]
        )
    _ -> AgnesBaker <$> runMessage msg attrs
