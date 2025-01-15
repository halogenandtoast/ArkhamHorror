module Arkham.Asset.Assets.Machete (machete) where

import Arkham.Ability.Scripted.Builder
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Builder
import Arkham.Card
import Arkham.Matcher

newtype Machete = Machete AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable, Sourceable, HasCardCode)
  deriving HasAbilities via Scripted Machete
  deriving RunMessage via Scripted Machete

machete :: AssetCard Machete
machete = asset Machete Cards.machete

instance ScriptedAbilities Machete where
  scriptedAbilities = abilities do
    fightAction $ fight $ effect you do
      combat (Fixed 1)
      damageDealt 1 `whenAttackedEnemy` onlyEnemyEngagedWith you
