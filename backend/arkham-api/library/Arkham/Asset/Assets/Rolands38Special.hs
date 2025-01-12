module Arkham.Asset.Assets.Rolands38Special (rolands38Special) where

import Arkham.Ability hiding (you)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (chooseFightEnemyWithModifiers)
import Arkham.Asset.Uses
import Arkham.Modifier
import Arkham.Script

newtype Rolands38Special = Rolands38Special AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rolands38Special :: AssetCard Rolands38Special
rolands38Special = asset Rolands38Special Cards.rolands38Special

instance HasAbilities Rolands38Special where
  getAbilities (Rolands38Special x) = [restricted x 1 ControlsThis $ fightAction $ assetUseCost x Ammo 1]

instance RunMessage Rolands38Special where
  runMessage = script do
    onAbility 1 do
      sid <- getRandom
      chooseFightEnemyWithModifiers
        sid
        ability
        [ DamageDealt 1
        , CalculatedSkillModifier #combat
            $ IfLocationExistsCalculation (yourLocation <> withClues) (Fixed 3) (Fixed 1)
        ]
