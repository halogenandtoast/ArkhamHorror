{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.ArkhamWoodsTwistingPaths where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ArkhamWoodsTwistingPaths = ArkhamWoodsTwistingPaths Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arkhamWoodsTwistingPaths :: ArkhamWoodsTwistingPaths
arkhamWoodsTwistingPaths = ArkhamWoodsTwistingPaths $ base
  { locationRevealedConnectedSymbols = setFromList [Squiggle, Diamond, Equals]
  , locationRevealedSymbol = T
  }
 where
  base = baseAttrs
    "01151"
    "Arkham Woods: Twisting Paths"
    3
    (PerPlayer 1)
    Square
    [Squiggle]
    [Woods]

instance HasModifiersFor env investigator ArkhamWoodsTwistingPaths where
  getModifiersFor _ _ _ = pure []

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
            mempty
            mempty
            mempty
          )
    _ -> ArkhamWoodsTwistingPaths <$> runMessage msg attrs
