module Arkham.Asset.Assets.KhopeshOfTheAbyss (khopeshOfTheAbyss) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher hiding (EnemyDefeated)
import Arkham.Message.Lifted.Choose
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Move
import Arkham.Modifier

newtype KhopeshOfTheAbyss = KhopeshOfTheAbyss AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

khopeshOfTheAbyss :: AssetCard KhopeshOfTheAbyss
khopeshOfTheAbyss = asset KhopeshOfTheAbyss Cards.khopeshOfTheAbyss

instance HasAbilities KhopeshOfTheAbyss where
  getAbilities (KhopeshOfTheAbyss a) =
    [ restricted a 1 ControlsThis fightAction_
    , restricted a 2 (ControlsThis <> exists (EnemyAt $ RevealedLocation <> NotYourLocation))
        $ triggered
          (Matcher.EnemyDefeated #after You (BySource $ SourceIs $ toSource $ a.ability 1) AnyEnemy)
          (exhaust a)
    ]

instance RunMessage KhopeshOfTheAbyss where
  runMessage msg a@(KhopeshOfTheAbyss attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #combat 3, DamageDealt 1]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      enemies <- select $ EnemyAt $ RevealedLocation <> NotYourLocation
      chooseTargetM iid enemies \enemy -> handleTarget iid (attrs.ability 2) enemy
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 2 -> True) (EnemyTarget enemy) -> do
      selectEach (enemyEngagedWith iid <> not_ (EnemyWithId enemy)) (disengageEnemy iid)
      whenJustM (selectOne $ LocationWithEnemy $ EnemyWithId enemy) \loc -> do
        moveTo (attrs.ability 2) iid loc
        engageEnemy iid enemy
      pure a
    _ -> KhopeshOfTheAbyss <$> liftRunMessage msg attrs
