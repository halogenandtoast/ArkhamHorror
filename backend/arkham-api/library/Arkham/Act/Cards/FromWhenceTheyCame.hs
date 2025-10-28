module Arkham.Act.Cards.FromWhenceTheyCame (fromWhenceTheyCame) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Uses
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword (Keyword (Elusive))
import Arkham.Matcher
import Arkham.Trait (Trait (Saturnite))

newtype FromWhenceTheyCame = FromWhenceTheyCame ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fromWhenceTheyCame :: ActCard FromWhenceTheyCame
fromWhenceTheyCame = act (3, A) FromWhenceTheyCame Cards.fromWhenceTheyCame Nothing

instance HasModifiersFor FromWhenceTheyCame where
  getModifiersFor (FromWhenceTheyCame a) = do
    modifySelect
      a
      (EnemyWithTitle "Possessed Extra")
      [HealthModifier 2, EnemyFight 1, EnemyEvade 1, AddKeyword Elusive, AddTrait Saturnite]

instance HasAbilities FromWhenceTheyCame where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1 $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) Anywhere)
    , restricted
        a
        2
        (exists $ assetIs Assets.heliosTelescopeGateToTheCosmos <> AssetWithUseCount Shard (static 0))
        $ forced AnyWindow
    , mkAbility a 3
        $ Objective
        $ forced
        $ ifEnemyDefeated Enemies.saturniteMonarchInAnAlienLand
    ]

instance RunMessage FromWhenceTheyCame where
  runMessage msg a@(FromWhenceTheyCame attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      heliosTelescope <- selectJust $ assetIs Assets.heliosTelescopeGateToTheCosmos
      placeTokens (attrs.ability 1) heliosTelescope Shard 1
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    UseThisAbility _iid (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      viaForced <-
        selectAny $ assetIs Assets.heliosTelescopeGateToTheCosmos <> AssetWithUseCount Shard (static 0)
      unless viaForced do
        heliosTelescope <- selectJust $ assetIs Assets.heliosTelescopeGateToTheCosmos
        addToVictory heliosTelescope

      push $ if viaForced then R1 else R2
      pure a
    _ -> FromWhenceTheyCame <$> liftRunMessage msg attrs
