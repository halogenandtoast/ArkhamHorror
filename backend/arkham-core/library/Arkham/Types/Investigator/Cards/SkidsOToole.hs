{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.SkidsOToole where

import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.FastWindow (Who(..))
import qualified Arkham.Types.FastWindow as Fast
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson
import Lens.Micro

newtype SkidsOToole = SkidsOToole Attrs
  deriving newtype (Show, ToJSON, FromJSON)

skidsOToole :: SkidsOToole
skidsOToole = SkidsOToole $ (baseAttrs
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
                            )
  { investigatorAbilities =
    [ (mkAbility
        (InvestigatorSource "01003")
        1
        (FastAbility (Fast.DuringTurn You))
      )
        { abilityCost = Just $ ResourceCost 2
        }
    ]
  }

instance (InvestigatorRunner env) => RunMessage env SkidsOToole where
  runMessage msg i@(SkidsOToole attrs@Attrs {..}) = case msg of
    UseCardAbility _ _ (InvestigatorSource iid) 1 | iid == investigatorId -> do
      pure . SkidsOToole $ attrs & resources -~ 2 & remainingActions +~ 1
    ResolveToken ElderSign iid skillValue | iid == investigatorId -> do
      runTest skillValue 2
      i <$ unshiftMessage (AddOnSuccess (TakeResources iid 2 False))
    _ -> SkidsOToole <$> runMessage msg attrs
