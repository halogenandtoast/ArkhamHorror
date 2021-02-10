module Arkham.Types.Location.Cards.ArkhamWoodsGreatWillow where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Source
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ArkhamWoodsGreatWillow = ArkhamWoodsGreatWillow LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsGreatWillow :: ArkhamWoodsGreatWillow
arkhamWoodsGreatWillow = ArkhamWoodsGreatWillow $ base
  { locationRevealedConnectedSymbols = setFromList [Squiggle, Star]
  , locationRevealedSymbol = Heart
  }
 where
  base = baseAttrs
    "50033"
    (Name "Arkham Woods" $ Just "Great Willow")
    EncounterSet.ReturnToTheDevourerBelow
    4
    (PerPlayer 1)
    Square
    [Squiggle]
    [Woods]

instance HasModifiersFor env ArkhamWoodsGreatWillow where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ArkhamWoodsGreatWillow where
  getActions i window (ArkhamWoodsGreatWillow attrs) =
    getActions i window attrs

-- | Unused here is on a forced ability
instance LocationRunner env => RunMessage env ArkhamWoodsGreatWillow where
  runMessage msg l@(ArkhamWoodsGreatWillow attrs@LocationAttrs {..}) = case msg of
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
