module Arkham.Asset.Assets.CrystallizerOfDreams (crystallizerOfDreams) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (PlayCard)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.Window (getPlayedEvent)
import Arkham.Matcher hiding (EventCard, PlaceUnderneath)

newtype CrystallizerOfDreams = CrystallizerOfDreams AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystallizerOfDreams :: AssetCard CrystallizerOfDreams
crystallizerOfDreams = asset CrystallizerOfDreams Cards.crystallizerOfDreams

instance HasModifiersFor CrystallizerOfDreams where
  getModifiersFor (CrystallizerOfDreams a) = for_ a.controller \iid -> do
    modified_ a iid $ map CanCommitToSkillTestsAsIfInHand (assetCardsUnderneath a)

instance HasAbilities CrystallizerOfDreams where
  getAbilities (CrystallizerOfDreams a) =
    [ controlled a 1 cardRestriction
        $ freeReaction
        $ PlayEventDiscarding #after You (EventWithoutModifier RemoveFromGameInsteadOfDiscard)
    ]
   where
    cardRestriction = mwhen (length (assetCardsUnderneath a) >= 5) Never

instance RunMessage CrystallizerOfDreams where
  runMessage msg a@(CrystallizerOfDreams attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getPlayedEvent -> event) _ -> do
      card <- fetchCard event
      push $ RemoveEvent event
      placeUnderneath attrs [card]
      pure a
    _ -> CrystallizerOfDreams <$> liftRunMessage msg attrs
