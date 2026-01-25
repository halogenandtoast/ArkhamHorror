module Arkham.Treachery.Cards.CaveIn (caveIn) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Direction
import Arkham.ForMovement
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CaveIn = CaveIn TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

caveIn :: TreacheryCard CaveIn
caveIn = treachery CaveIn Cards.caveIn

instance HasAbilities CaveIn where
  getAbilities (CaveIn a) =
    [ mkAbility a 1
        $ forced
        $ VehicleWouldEnter #when (assetIs Assets.mineCartReliableButBroken) (locationWithTreachery a)
    , skillTestAbility
        $ mkAbility a 2 actionAbility
        & restrict (youExist $ InvestigatorAt (orConnected NotForMovement (locationWithTreachery a)))
    ]

instance RunMessage CaveIn where
  runMessage msg t@(CaveIn attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      mineCart <- selectJust (assetIs Assets.mineCartReliableButBroken)
      meta <- field AssetMeta mineCart
      let dir = toResultDefault East meta
      loc <- selectJust (locationWithAsset mineCart)
      pos <- (`updatePosition` dir) <$> fieldJust LocationPosition loc
      selectOne (LocationInPosition pos) >>= \case
        Nothing -> gainSurge attrs
        Just lid -> attachTreachery attrs lid
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      cancelWindowBatch ws
      investigators <- select $ InVehicleMatching $ assetIs Assets.mineCartReliableButBroken
      for_ investigators $ assignDamageTo (attrs.ability 1) 2
      selectEach (AssetControlledBy $ mapOneOf InvestigatorWithId investigators) \a ->
        dealAssetDirectDamage a (attrs.ability 1) 2
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 2) attrs [#combat, #agility] (Fixed 2)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> CaveIn <$> liftRunMessage msg attrs
