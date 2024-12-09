module Arkham.Asset.Assets.CrystallizerOfDreams (crystallizerOfDreams, CrystallizerOfDreams (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (PlayCard)
import Arkham.Event.Types (Field (..))
import Arkham.Id
import Arkham.Matcher hiding (EventCard, PlaceUnderneath)
import Arkham.Prelude
import Arkham.Projection
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype CrystallizerOfDreams = CrystallizerOfDreams AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystallizerOfDreams :: AssetCard CrystallizerOfDreams
crystallizerOfDreams = asset CrystallizerOfDreams Cards.crystallizerOfDreams

instance HasModifiersFor CrystallizerOfDreams where
  getModifiersFor (CrystallizerOfDreams a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> modified_ a iid $ map CanCommitToSkillTestsAsIfInHand (assetCardsUnderneath a)

instance HasAbilities CrystallizerOfDreams where
  getAbilities (CrystallizerOfDreams a) =
    [ controlledAbility a 1 cardRestriction
        $ freeReaction
        $ PlayEventDiscarding #after You (EventWithoutModifier RemoveFromGameInsteadOfDiscard)
    ]
   where
    cardRestriction = mwhen (length (assetCardsUnderneath a) >= 5) Never

getPlayedEvent :: [Window] -> EventId
getPlayedEvent = \case
  [] -> error "impossible"
  ((windowType -> Window.PlayEventDiscarding _ eventId) : _) -> eventId
  (_ : rest) -> getPlayedEvent rest

instance RunMessage CrystallizerOfDreams where
  runMessage msg a@(CrystallizerOfDreams attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getPlayedEvent -> event) _ -> do
      card <- field EventCard event
      pushAll [RemoveEvent event, PlaceUnderneath (toTarget attrs) [card]]
      pure a
    _ -> CrystallizerOfDreams <$> runMessage msg attrs
