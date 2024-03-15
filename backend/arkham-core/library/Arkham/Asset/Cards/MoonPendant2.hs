module Arkham.Asset.Cards.MoonPendant2 (moonPendant2, MoonPendant2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Trait (Trait (Tarot))

newtype MoonPendant2 = MoonPendant2 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonPendant2 :: AssetCard MoonPendant2
moonPendant2 = asset MoonPendant2 Cards.moonPendant2

instance HasModifiersFor MoonPendant2 where
  getModifiersFor (CardIdTarget cid) (MoonPendant2 attrs) | Just iid <- assetController attrs = do
    mcard <- selectOne $ basic (CardWithId cid) <> inHandOf iid
    committed <- fieldMap InvestigatorCommittedCards (elem cid . map toCardId) iid

    if isJust mcard || committed
      then do
        card <- getCard cid
        let valid = cardMatch card (NonWeakness <> CardWithTrait Tarot)
        pure $ toModifiers attrs [AddSkillIcons [#wild, #wild] | valid]
      else pure []
  getModifiersFor _ _ = pure []

instance HasAbilities MoonPendant2 where
  getAbilities (MoonPendant2 a) = [restrictedAbility a 1 InYourHand $ freeReaction (GameBegins #when)]

instance RunMessage MoonPendant2 where
  runMessage msg a@(MoonPendant2 attrs) = case msg of
    InHand _ (UseThisAbility iid (isSource attrs -> True) 1) -> do
      push $ putCardIntoPlay iid attrs
      pure a
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      push $ AddSlot iid TarotSlot (Slot (toSource attrs) [])
      MoonPendant2 <$> runMessage msg attrs
    _ -> MoonPendant2 <$> runMessage msg attrs
