module Arkham.Asset.Assets.SummonedNightgaunt (summonedNightgaunt) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Nightgaunt))

newtype SummonedNightgaunt = SummonedNightgaunt AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

summonedNightgaunt :: AssetCard SummonedNightgaunt
summonedNightgaunt = assetWith SummonedNightgaunt Cards.summonedNightgaunt $ whenNoUsesL ?~ NotifySelfOfNoUses

instance HasModifiersFor SummonedNightgaunt where
  getModifiersFor (SummonedNightgaunt a) = modifySelf a [RemoveFromGameInsteadOfDiscard]

instance HasAbilities SummonedNightgaunt where
  getAbilities (SummonedNightgaunt a) =
    [ controlled a 1 NoRestriction
        $ ActionAbility #evade Nothing (ActionCost 1 <> assetUseCost a Whistle 1 <> HorrorCost (toSource a) YouTarget 1)
    ]

instance RunMessage SummonedNightgaunt where
  runMessage msg a@(SummonedNightgaunt attrs) = runQueueT $ case msg of
    SpentAllUses (isTarget attrs -> True) -> do
      removeFromGame attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (enemyEngagedWith iid <> not_ (EnemyWithTrait Nightgaunt)) (automaticallyEvadeEnemy iid)
      locations <- select RevealedLocation
      chooseTargetM iid locations \loc -> moveTo (attrs.ability 1) iid loc
      pure a
    _ -> SummonedNightgaunt <$> liftRunMessage msg attrs
