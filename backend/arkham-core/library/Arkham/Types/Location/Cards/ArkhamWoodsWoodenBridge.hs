module Arkham.Types.Location.Cards.ArkhamWoodsWoodenBridge where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Source
import qualified Arkham.Types.Action as Action
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ArkhamWoodsWoodenBridge = ArkhamWoodsWoodenBridge LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsWoodenBridge :: ArkhamWoodsWoodenBridge
arkhamWoodsWoodenBridge = ArkhamWoodsWoodenBridge $ base
  { locationRevealedConnectedSymbols = setFromList [Squiggle, Droplet]
  , locationRevealedSymbol = Circle
  }
 where
  base = baseAttrs
    "50036"
    (Name "Arkham Woods" $ Just "Wooden Bridge")
    EncounterSet.ReturnToTheDevourerBelow
    3
    (PerPlayer 1)
    Square
    [Squiggle]
    [Woods]

instance HasModifiersFor env ArkhamWoodsWoodenBridge where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ArkhamWoodsWoodenBridge where
  getActions i window (ArkhamWoodsWoodenBridge attrs) =
    getActions i window attrs

instance (LocationRunner env) => RunMessage env ArkhamWoodsWoodenBridge where
  runMessage msg l@(ArkhamWoodsWoodenBridge attrs@LocationAttrs {..}) = case msg of
    RevealToken (SkillTestSource _ _ _ (Just Action.Evade)) iid _
      | iid `elem` locationInvestigators -> do
        let
          ability = (mkAbility (toSource attrs) 0 ForcedAbility)
            { abilityLimit = PlayerLimit PerTestOrAbility 1
            }
        unused <- getIsUnused' iid ability
        l <$ when
          unused
          (unshiftMessages [UseLimitedAbility iid ability, DrawAnotherToken iid]
          )
    _ -> ArkhamWoodsWoodenBridge <$> runMessage msg attrs
