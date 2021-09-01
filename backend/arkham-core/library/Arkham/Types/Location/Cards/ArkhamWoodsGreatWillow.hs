module Arkham.Types.Location.Cards.ArkhamWoodsGreatWillow where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (arkhamWoodsGreatWillow)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target

newtype ArkhamWoodsGreatWillow = ArkhamWoodsGreatWillow LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsGreatWillow :: LocationCard ArkhamWoodsGreatWillow
arkhamWoodsGreatWillow = locationWith
  ArkhamWoodsGreatWillow
  Cards.arkhamWoodsGreatWillow
  4
  (PerPlayer 1)
  Square
  [Squiggle]
  ((revealedConnectedSymbolsL .~ setFromList [Squiggle, Star])
  . (revealedSymbolL .~ Heart)
  )

instance HasAbilities env ArkhamWoodsGreatWillow where
  getAbilities i window (ArkhamWoodsGreatWillow attrs) =
    getAbilities i window attrs

-- | Unused here is on a forced ability
instance LocationRunner env => RunMessage env ArkhamWoodsGreatWillow where
  runMessage msg l@(ArkhamWoodsGreatWillow attrs@LocationAttrs {..}) =
    case msg of
      PassedSkillTest iid _ source@(TreacherySource _) SkillTestInitiatorTarget{} _ _
        | iid `elem` locationInvestigators
        -> do
          let
            ability = (mkAbility (toSource attrs) 0 LegacyForcedAbility)
              { abilityLimit = GroupLimit PerRound 1
              }
          unused <- getGroupIsUnused ability
          l <$ when
            unused
            (pushAll [UseLimitedAbility iid ability, Surge iid source])
      _ -> ArkhamWoodsGreatWillow <$> runMessage msg attrs
