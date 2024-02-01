module Arkham.Asset.Cards.SpectralWeb (
  spectralWeb,
  SpectralWeb (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Trait (Trait (Geist))

newtype SpectralWeb = SpectralWeb AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

spectralWeb :: AssetCard SpectralWeb
spectralWeb =
  asset SpectralWeb Cards.spectralWeb

instance HasAbilities SpectralWeb where
  getAbilities (SpectralWeb attrs) =
    [ controlledAbility attrs 1 (exists $ CanFightEnemy (toSource attrs) <> EnemyWithTrait Geist)
        $ ActionAbility ([Action.Fight])
        $ ActionCost 1
        <> GroupClueCostRange (1, 3) YourLocation
    ]

toSpentClues :: Payment -> Int
toSpentClues (CluePayment _ x) = x
toSpentClues (Payments xs) = sum $ map toSpentClues xs
toSpentClues _ = 0

instance RunMessage SpectralWeb where
  runMessage msg a@(SpectralWeb attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (toSpentClues -> x) -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ SkillLabel
            sType
            [ skillTestModifiers attrs iid [AnySkillValue x, DamageDealt x]
            , ChooseFightEnemy iid (toSource attrs) Nothing sType mempty False
            ]
          | sType <- [SkillWillpower, SkillCombat]
          ]
      pure a
    _ -> SpectralWeb <$> runMessage msg attrs
