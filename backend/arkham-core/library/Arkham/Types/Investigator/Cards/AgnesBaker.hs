{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.AgnesBaker where

import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.EnemyId
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.Window (Who(..))
import qualified Arkham.Types.Window as Fast
import ClassyPrelude
import Data.Aeson
import qualified Data.HashSet as HashSet

newtype AgnesBaker = AgnesBaker Attrs
  deriving newtype (Show, ToJSON, FromJSON)

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

instance (ActionRunner env investigator) => HasActions env investigator AgnesBaker where
  getActions i (Fast.AfterAssignedHorror You) (AgnesBaker Attrs {..})
    | getId () i == investigatorId = do
      locationEnemyIds <- HashSet.toList
        <$> asks (getSet @EnemyId (locationOf i))
      let
        ability = (mkAbility
                    (InvestigatorSource investigatorId)
                    1
                    (ReactionAbility (Fast.AfterAssignedHorror You))
                  )
          { abilityLimit = PerPhase
          }
      usedAbilities <- map unUsedAbility <$> asks (getList ())
      pure
        [ ActivateCardAbilityAction investigatorId ability
        | (investigatorId, ability) `notElem` usedAbilities && not
          (null locationEnemyIds)
        ]
  getActions i window (AgnesBaker attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env AgnesBaker where
  runMessage msg i@(AgnesBaker attrs@Attrs {..}) = case msg of
    UseCardAbility _ _ (InvestigatorSource iid) _ 1 | iid == investigatorId ->
      do
        lid <- asks (getId @LocationId investigatorId)
        locationEnemyIds <- HashSet.toList <$> asks (getSet lid)
        i <$ unshiftMessage
          (Ask iid $ ChooseOne
            [ EnemyDamage eid iid (InvestigatorSource investigatorId) 1
            | eid <- locationEnemyIds
            ]
          )
    ResolveToken ElderSign iid skillValue | iid == investigatorId ->
      i <$ runTest skillValue investigatorSanityDamage
    _ -> AgnesBaker <$> runMessage msg attrs
