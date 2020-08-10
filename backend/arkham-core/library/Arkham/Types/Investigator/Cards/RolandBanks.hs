{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.RolandBanks where

import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.FastWindow (Who(..))
import qualified Arkham.Types.FastWindow as Fast
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype RolandBanks = RolandBanks Attrs
  deriving newtype (Show, ToJSON, FromJSON)

rolandBanks :: RolandBanks
rolandBanks = RolandBanks $ (baseAttrs
                              "01001"
                              "Roland Banks"
                              Guardian
                              stats
                              [Agency, Detective]
                            )
  { investigatorAbilities =
    [ ( InvestigatorSource "01001"
      , InvestigatorSource "01001"
      , 1
      , ReactionAbility (Fast.WhenEnemyDefeated You)
      , OncePerRound
      )
    ]
  }
 where
  stats = Stats
    { health = 9
    , sanity = 5
    , willpower = 3
    , intellect = 3
    , combat = 4
    , agility = 2
    }

instance (InvestigatorRunner env) => RunMessage env RolandBanks where
  runMessage msg rb@(RolandBanks attrs@Attrs {..}) = case msg of
    UseCardAbility _ (InvestigatorSource iid, _, 1, _, _)
      | iid == investigatorId -> rb <$ unshiftMessage
        (DiscoverCluesAtLocation investigatorId investigatorLocation 1)
    ResolveToken ElderSign iid skillValue | iid == investigatorId -> do
      clueCount <- unClueCount <$> asks (getCount investigatorLocation)
      rb <$ runTest (skillValue + clueCount)
    _ -> RolandBanks <$> runMessage msg attrs
