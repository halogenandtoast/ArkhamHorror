module Arkham.Treachery.Cards.DraggedUnderDevilReef (
  draggedUnderDevilReef,
  DraggedUnderDevilReef (..),
)
where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DraggedUnderDevilReef = DraggedUnderDevilReef TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

draggedUnderDevilReef :: TreacheryCard DraggedUnderDevilReef
draggedUnderDevilReef = treachery DraggedUnderDevilReef Cards.draggedUnderDevilReef

instance HasModifiersFor DraggedUnderDevilReef where
  getModifiersFor (DraggedUnderDevilReef a) = inThreatAreaGets a [CannotEnterVehicle AnyAsset]

instance HasAbilities DraggedUnderDevilReef where
  getAbilities (DraggedUnderDevilReef a) = [restricted a 1 OnSameLocation actionAbility]

instance RunMessage DraggedUnderDevilReef where
  runMessage msg t@(DraggedUnderDevilReef attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      field InvestigatorPlacement iid >>= \case
        InVehicle aid ->
          field AssetLocation aid >>= \case
            Just loc -> place iid (AtLocation loc)
            Nothing -> pure ()
        _ -> pure ()
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#combat, #agility] \skillType -> do
          skillLabeled skillType $ beginSkillTest sid iid (attrs.ability 1) iid skillType (Fixed 3)
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> DraggedUnderDevilReef <$> liftRunMessage msg attrs
