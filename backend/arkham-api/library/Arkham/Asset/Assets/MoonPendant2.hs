module Arkham.Asset.Assets.MoonPendant2 (moonPendant2, MoonPendant2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype MoonPendant2 = MoonPendant2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonPendant2 :: AssetCard MoonPendant2
moonPendant2 = asset MoonPendant2 Cards.moonPendant2

instance HasModifiersFor MoonPendant2 where
  getModifiersFor (MoonPendant2 a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> do
      hand <- select $ basic (NonWeakness <> #tarot) <> inHandOf iid
      committed <- fieldMap InvestigatorCommittedCards (filterCards (NonWeakness <> #tarot)) iid
      modifyEach a (hand <> committed) [AddSkillIcons [#wild, #wild]]

instance HasAbilities MoonPendant2 where
  getAbilities (MoonPendant2 a) = [restrictedAbility a 1 InYourHand $ freeReaction (GameBegins #when)]

instance RunMessage MoonPendant2 where
  runMessage msg a@(MoonPendant2 attrs) = case msg of
    InHand _ (UseThisAbility iid (isSource attrs -> True) 1) -> do
      push $ putCardIntoPlay iid attrs
      pure a
    CardIsEnteringPlay iid card | toCardId card == toCardId attrs -> do
      push $ AddSlot iid TarotSlot (Slot (toSource attrs) [])
      MoonPendant2 <$> runMessage msg attrs
    _ -> MoonPendant2 <$> runMessage msg attrs
