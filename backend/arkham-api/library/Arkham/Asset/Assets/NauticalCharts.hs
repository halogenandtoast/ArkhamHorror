module Arkham.Asset.Assets.NauticalCharts (nauticalCharts) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype NauticalCharts = NauticalCharts AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nauticalCharts :: AssetCard NauticalCharts
nauticalCharts = asset NauticalCharts Cards.nauticalCharts

instance HasAbilities NauticalCharts where
  getAbilities (NauticalCharts x) =
    [ restricted x 1 InYourHand $ investigateAction (DiscardCardCost (toCard x))
    , controlled x 1 (not_ InYourHand) $ investigateAction (exhaust x)
    ]

-- TODO: We need a way to know if additional clues can be discovered
instance RunMessage NauticalCharts where
  runMessage msg a@(NauticalCharts attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 1)
      investigate sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      void $ runMaybeT do
        hand <- lift $ select $ InHandOf NotForPlay (be iid) <> basic DiscardableCard
        guard $ notNull hand
        lift $ withSkillTest \sid -> do
          chooseOneM iid do
            questionLabeled "Discard 1 card from your hand to discover 1 additional clue"
            labeled "Do not discard card" nothing
            targets hand \card -> do
              discardCard iid (attrs.ability 1) card
              skillTestModifier sid (attrs.ability 1) iid (DiscoveredClues 1)
      pure a
    _ -> NauticalCharts <$> liftRunMessage msg attrs
