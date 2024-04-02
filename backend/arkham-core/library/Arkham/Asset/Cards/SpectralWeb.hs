module Arkham.Asset.Cards.SpectralWeb (spectralWeb, SpectralWeb (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait (Trait (Geist))

newtype SpectralWeb = SpectralWeb AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spectralWeb :: AssetCard SpectralWeb
spectralWeb = asset SpectralWeb Cards.spectralWeb

instance HasAbilities SpectralWeb where
  getAbilities (SpectralWeb attrs) =
    [ controlledAbility attrs 1 (exists $ CanFightEnemy (toSource attrs) <> EnemyWithTrait Geist)
        $ fightAction
        $ GroupClueCostRange (1, 3) YourLocation
    ]

toSpentClues :: Payment -> Int
toSpentClues (CluePayment _ x) = x
toSpentClues (Payments xs) = sum $ map toSpentClues xs
toSpentClues _ = 0

instance RunMessage SpectralWeb where
  runMessage msg a@(SpectralWeb attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (toSpentClues -> x) -> do
      player <- getPlayer iid
      let source = attrs.ability 1
      choices <- for [#willpower, #combat] \sType -> do
        chooseFight <- toMessage . withSkillType sType <$> mkChooseFight iid source
        pure
          $ SkillLabel sType [skillTestModifiers source iid [AnySkillValue x, DamageDealt x], chooseFight]
      push $ chooseOne player choices
      pure a
    _ -> SpectralWeb <$> runMessage msg attrs
