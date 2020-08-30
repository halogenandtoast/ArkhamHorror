{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.SkidsOToole where

import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Window
import qualified Arkham.Types.Window as Fast
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

instance (ActionRunner env investigator) => HasActions env investigator SkidsOToole where
  getActions i (DuringTurn You) (SkidsOToole Attrs {..})
    | getId () i == investigatorId = do
      let
        ability = mkAbility
          (InvestigatorSource "01003")
          1
          (FastAbility (Fast.DuringTurn You))
      usedAbilities <- map unUsedAbility <$> asks (getList ())
      pure
        [ ActivateCardAbilityAction investigatorId ability
        | resourceCount i
          >= 2
          && (investigatorId, ability)
          `notElem` usedAbilities
        ]
  getActions _ _ _ = pure []

instance (InvestigatorRunner Attrs env) => RunMessage env SkidsOToole where
  runMessage msg i@(SkidsOToole attrs@Attrs {..}) = case msg of
    UseCardAbility _ _ (InvestigatorSource iid) 1 | iid == investigatorId -> do
      pure . SkidsOToole $ attrs & resources -~ 2 & remainingActions +~ 1
    ResolveToken ElderSign iid skillValue | iid == investigatorId -> do
      runTest skillValue 2
      i <$ unshiftMessage (AddOnSuccess (TakeResources iid 2 False))
    _ -> SkidsOToole <$> runMessage msg attrs
