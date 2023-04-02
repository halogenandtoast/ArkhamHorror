module Arkham.Asset.Cards.EnchantedBladeGuardian3
  ( enchantedBladeGuardian3
  , EnchantedBladeGuardian3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher hiding ( EnemyDefeated )
import Arkham.SkillType

newtype Metadata = Metadata { empowered :: Bool }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype EnchantedBladeGuardian3 = EnchantedBladeGuardian3 (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantedBladeGuardian3 :: AssetCard EnchantedBladeGuardian3
enchantedBladeGuardian3 = asset
  (EnchantedBladeGuardian3 . (`with` Metadata False))
  Cards.enchantedBladeGuardian3

getPaidUse :: Payment -> Bool
getPaidUse (UsesPayment _) = True
getPaidUse (Payments ps) = any getPaidUse ps
getPaidUse _ = False

instance HasAbilities EnchantedBladeGuardian3 where
  getAbilities (EnchantedBladeGuardian3 (attrs `With` _)) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ActionAbility (Just Action.Fight)
        $ ActionCost 1
    ]

instance RunMessage EnchantedBladeGuardian3 where
  runMessage msg a@(EnchantedBladeGuardian3 (attrs `With` meta)) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ skillTestModifier attrs iid (SkillModifier SkillCombat 2)
        , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
        ]
      pure a
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        when (useCount (assetUses attrs) > 0) $ do
          push $ chooseOne
            iid
            [ Label
              "Spend 1 use to empower"
              [DoStep 1 (SpendUses (toTarget attrs) Charge 1)]
            , Label "Do not spend use" []
            ]
        pure a
    DoStep 1 msg'@(SpendUses (isTarget attrs -> True) _ _) -> do
      for_ (assetController attrs)
        $ \iid -> pushAll [msg', skillTestModifier attrs iid (DamageDealt 1)]
      pure . EnchantedBladeGuardian3 $ attrs `with` Metadata True
    EnemyDefeated _ _ (isSource attrs -> True) _ -> do
      when (empowered meta) $ for_ (assetController attrs) $ \iid -> do
        drawing <- drawCards iid (toSource attrs) 1
        pushAll [drawing, HealHorror (toTarget iid) (toSource attrs) 1]
      pure a
    SkillTestEnds _ _ -> do
      pure . EnchantedBladeGuardian3 $ attrs `with` Metadata False
    _ -> EnchantedBladeGuardian3 . (`with` meta) <$> runMessage msg attrs
