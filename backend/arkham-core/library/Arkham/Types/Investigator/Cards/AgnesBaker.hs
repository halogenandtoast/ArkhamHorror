{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.AgnesBaker where

import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.FastWindow (Who(..))
import qualified Arkham.Types.FastWindow as Fast
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype AgnesBaker = AgnesBaker Attrs
  deriving newtype (Show, ToJSON, FromJSON)

agnesBaker :: AgnesBaker
agnesBaker = AgnesBaker $ (baseAttrs
                            "01004"
                            "Agnes Baker"
                            Mystic
                            stats
                            [Sorcerer]
                          )
  { investigatorAbilities =
    [ (mkAbility
        (InvestigatorSource "01004")
        1
        (ReactionAbility (Fast.WhenAssignedHorror You))
      )
        { abilityLimit = OncePerPhase
        }
    ]
  }
 where
  stats = Stats
    { health = 6
    , sanity = 8
    , willpower = 5
    , intellect = 2
    , combat = 2
    , agility = 3
    }

instance HasActions env investigator AgnesBaker where
  getActions i window (AgnesBaker attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env AgnesBaker where
  runMessage msg i@(AgnesBaker attrs@Attrs {..}) = case msg of
    UseCardAbility _ _ (InvestigatorSource iid) 1 | iid == investigatorId ->
      locationId <- asks (getId @LocationId (getInvestigator attrs))
      locationEnemyIds <- HashSet.toList <$> asks (getSet locationId)
      unshiftMessage (Ask iid $ ChooseOne
        [ EnemyDamage eid iid (AssetSource assetId) 1
        | eid <- locationEnemyIds
        ])
      pure i
    ResolveToken ElderSign iid _skillValue | iid == investigatorId ->
      i <$ runTest skillValue investigatorSanityDamage
      pure i
    _ -> AgnesBaker <$> runMessage msg attrs
