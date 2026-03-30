module Arkham.Asset.Assets.UniversityArchivist (universityArchivist) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Slot
import Arkham.Strategy
import Arkham.Trait

newtype UniversityArchivist = UniversityArchivist AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

universityArchivist :: AssetCard UniversityArchivist
universityArchivist = ally UniversityArchivist Cards.universityArchivist (1, 1)

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Tome []

instance HasAbilities UniversityArchivist where
  getAbilities (UniversityArchivist a) =
    [controlled_ a 1 $ freeReaction $ AssetEntersPlay #after (be a)]

instance RunMessage UniversityArchivist where
  runMessage msg a@(UniversityArchivist attrs) = runQueueT $ case msg of
    CardIsEnteringPlay iid card | card.id == attrs.cardId -> do
      push $ AddSlot iid HandSlot (slot attrs)
      UniversityArchivist <$> liftRunMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search
        iid
        (attrs.ability 1)
        iid
        [fromTopOfDeck 6]
        (basic $ #asset <> withTrait Tome)
        (DrawFound iid 1)
      pure a
    _ -> UniversityArchivist <$> liftRunMessage msg attrs
