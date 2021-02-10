module Arkham.Types.Location.Cards.ArkhamWoodsTwistingPaths where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EffectMetadata
import Arkham.Types.GameValue
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ArkhamWoodsTwistingPaths = ArkhamWoodsTwistingPaths LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsTwistingPaths :: ArkhamWoodsTwistingPaths
arkhamWoodsTwistingPaths = ArkhamWoodsTwistingPaths $ base
  { locationRevealedConnectedSymbols = setFromList [Squiggle, Diamond, Equals]
  , locationRevealedSymbol = T
  }
 where
  base = baseAttrs
    "01151"
    (Name "Arkham Woods" (Just "Twisting Paths"))
    EncounterSet.TheDevourerBelow
    3
    (PerPlayer 1)
    Square
    [Squiggle]
    [Woods]

instance HasModifiersFor env ArkhamWoodsTwistingPaths where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ArkhamWoodsTwistingPaths where
  getActions i window (ArkhamWoodsTwistingPaths attrs) =
    getActions i window attrs

instance (LocationRunner env) => RunMessage env ArkhamWoodsTwistingPaths where
  runMessage msg l@(ArkhamWoodsTwistingPaths attrs@LocationAttrs {..}) = case msg of
    Will (MoveTo iid lid)
      | iid `elem` locationInvestigators && lid /= locationId -> do
        moveFrom <- popMessage -- MoveFrom
        moveTo <- popMessage -- MoveTo
        l <$ unshiftMessages
          [ CreateEffect
            "01151"
            (Just $ EffectMessages (catMaybes [moveFrom, moveTo]))
            (toSource attrs)
            (InvestigatorTarget iid)
          , BeginSkillTest
            iid
            (LocationSource "01151")
            (InvestigatorTarget iid)
            Nothing
            SkillIntellect
            3
          ]
    _ -> ArkhamWoodsTwistingPaths <$> runMessage msg attrs
