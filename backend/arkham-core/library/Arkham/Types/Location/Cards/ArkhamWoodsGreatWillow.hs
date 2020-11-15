{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.ArkhamWoodsGreatWillow where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ArkhamWoodsGreatWillow = ArkhamWoodsGreatWillow Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arkhamWoodsGreatWillow :: ArkhamWoodsGreatWillow
arkhamWoodsGreatWillow = ArkhamWoodsGreatWillow $ base
  { locationRevealedConnectedSymbols = setFromList [Squiggle, Star]
  , locationRevealedSymbol = Heart
  }
 where
  base = baseAttrs
    "50033"
    "Arkham Woods: Great Willow"
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

instance LocationRunner env => RunMessage env ArkhamWoodsGreatWillow where
  runMessage msg l@(ArkhamWoodsGreatWillow attrs@Attrs {..}) = case msg of
    PassedSkillTest iid _ source@(TreacherySource _) _ _
      | iid `elem` locationInvestigators -> do
        let
          ability = (mkAbility (toSource attrs) 0 ForcedAbility)
            { abilityLimit = PerRound
            }
        unused <- getGroupIsUnused ability
        l <$ when
          unused
          (unshiftMessages [UseLimitedAbility iid ability, Surge iid source])
    _ -> ArkhamWoodsGreatWillow <$> runMessage msg attrs
