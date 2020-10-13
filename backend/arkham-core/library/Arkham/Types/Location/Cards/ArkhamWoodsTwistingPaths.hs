{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.ArkhamWoodsTwistingPaths where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype ArkhamWoodsTwistingPaths = ArkhamWoodsTwistingPaths Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arkhamWoodsTwistingPaths :: ArkhamWoodsTwistingPaths
arkhamWoodsTwistingPaths =
  ArkhamWoodsTwistingPaths $ (baseAttrs
                               "01151"
                               "Arkham Woods: Twisting Paths"
                               3
                               (PerPlayer 1)
                               Square
                               [Squiggle]
                               [Woods]
                             )
    { locationRevealedConnectedSymbols = HashSet.fromList
      [Squiggle, Diamond, Equals]
    , locationRevealedSymbol = T
    }

instance (IsInvestigator investigator) => HasActions env investigator ArkhamWoodsTwistingPaths where
  getActions i window (ArkhamWoodsTwistingPaths attrs) =
    getActions i window attrs

instance (LocationRunner env) => RunMessage env ArkhamWoodsTwistingPaths where
  runMessage msg l@(ArkhamWoodsTwistingPaths attrs@Attrs {..}) = case msg of
    Will (MoveTo iid lid)
      | iid `elem` locationInvestigators && lid /= locationId -> do
        moveFrom <- popMessage -- MoveFrom
        moveTo <- popMessage -- MoveTo
        l <$ unshiftMessage
          (BeginSkillTest
            iid
            (LocationSource "01151")
            (InvestigatorTarget iid)
            Nothing
            SkillIntellect
            3
            (catMaybes [moveFrom, moveTo])
            []
            mempty
            mempty
          )
    _ -> ArkhamWoodsTwistingPaths <$> runMessage msg attrs
