module Arkham.Asset.Assets.RubyStandishMasterThief (rubyStandish) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype RubyStandishMasterThief = RubyStandishMasterThief AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rubyStandish :: AssetCard RubyStandishMasterThief
rubyStandish = ally RubyStandishMasterThief Cards.rubyStandish (2, 2)

instance HasModifiersFor RubyStandishMasterThief where
  getModifiersFor (RubyStandishMasterThief a) = controllerGets a [SkillModifier #agility 1]

instance HasAbilities RubyStandishMasterThief where
  getAbilities (RubyStandishMasterThief a) =
    [ controlled
        a
        1
        (not_ $ exists $ ReadyEnemy <> EnemyAt YourLocation)
        $ FastAbility (exhaust a)
    ]

instance RunMessage RubyStandishMasterThief where
  runMessage msg a@(RubyStandishMasterThief attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      connected <- getAccessibleLocations iid (attrs.ability 1)
      chooseTargetM iid connected $ moveTo (attrs.ability 1) iid
      pure a
    _ -> RubyStandishMasterThief <$> liftRunMessage msg attrs
