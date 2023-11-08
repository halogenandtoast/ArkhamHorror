module Arkham.Asset.Cards.JennysTwin45s (
  JennysTwin45s (..),
  jennysTwin45s,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.SkillType

newtype JennysTwin45s = JennysTwin45s AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jennysTwin45s :: AssetCard JennysTwin45s
jennysTwin45s = asset JennysTwin45s Cards.jennysTwin45s

instance HasAbilities JennysTwin45s where
  getAbilities (JennysTwin45s a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility
          ([Action.Fight])
          (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Ammo 1])
    ]

instance RunMessage JennysTwin45s where
  runMessage msg a@(JennysTwin45s attrs) = case msg of
    PaidForCardCost _ card payment | toCardId card == toCardId attrs -> do
      let n = totalResourcePayment payment
      JennysTwin45s <$> runMessage msg (attrs & usesL .~ Uses Ammo n)
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          a
            <$ pushAll
              [ skillTestModifiers
                  attrs
                  (InvestigatorTarget iid)
                  [DamageDealt 1, SkillModifier SkillCombat 2]
              , ChooseFightEnemy iid source Nothing SkillCombat mempty False
              ]
    _ -> JennysTwin45s <$> runMessage msg attrs
