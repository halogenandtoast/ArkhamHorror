module Arkham.Types.Location.Cards.ArkhamWoodsTwistingPaths where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (arkhamWoodsTwistingPaths)
import Arkham.Types.Classes
import Arkham.Types.EffectMetadata
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype ArkhamWoodsTwistingPaths = ArkhamWoodsTwistingPaths LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsTwistingPaths :: LocationId -> ArkhamWoodsTwistingPaths
arkhamWoodsTwistingPaths =
  ArkhamWoodsTwistingPaths
    . (revealedConnectedSymbolsL .~ setFromList [Squiggle, Diamond, Equals])
    . (revealedSymbolL .~ T)
    . baseAttrs
        Cards.arkhamWoodsTwistingPaths
        3
        (PerPlayer 1)
        Square
        [Squiggle]

instance HasModifiersFor env ArkhamWoodsTwistingPaths where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ArkhamWoodsTwistingPaths where
  getActions i window (ArkhamWoodsTwistingPaths attrs) =
    getActions i window attrs

instance (LocationRunner env) => RunMessage env ArkhamWoodsTwistingPaths where
  runMessage msg l@(ArkhamWoodsTwistingPaths attrs@LocationAttrs {..}) =
    case msg of
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
              (toSource attrs)
              (InvestigatorTarget iid)
              Nothing
              SkillIntellect
              3
            ]
      _ -> ArkhamWoodsTwistingPaths <$> runMessage msg attrs
