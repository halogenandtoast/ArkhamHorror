module Arkham.Types.Location.Cards.ArkhamWoodsGreatWillow where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (arkhamWoodsGreatWillow)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Source

newtype ArkhamWoodsGreatWillow = ArkhamWoodsGreatWillow LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsGreatWillow :: LocationId -> ArkhamWoodsGreatWillow
arkhamWoodsGreatWillow =
  ArkhamWoodsGreatWillow
    . (revealedConnectedSymbolsL .~ setFromList [Squiggle, Star])
    . (revealedSymbolL .~ Heart)
    . baseAttrs
        Cards.arkhamWoodsGreatWillow
        4
        (PerPlayer 1)
        Square
        [Squiggle]

instance HasModifiersFor env ArkhamWoodsGreatWillow where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ArkhamWoodsGreatWillow where
  getActions i window (ArkhamWoodsGreatWillow attrs) =
    getActions i window attrs

-- | Unused here is on a forced ability
instance LocationRunner env => RunMessage env ArkhamWoodsGreatWillow where
  runMessage msg l@(ArkhamWoodsGreatWillow attrs@LocationAttrs {..}) =
    case msg of
      PassedSkillTest iid _ source@(TreacherySource _) _ _ _
        | iid `elem` locationInvestigators -> do
          let
            ability = (mkAbility (toSource attrs) 0 ForcedAbility)
              { abilityLimit = GroupLimit PerRound 1
              }
          unused <- getGroupIsUnused ability
          l <$ when
            unused
            (unshiftMessages [UseLimitedAbility iid ability, Surge iid source])
      _ -> ArkhamWoodsGreatWillow <$> runMessage msg attrs
