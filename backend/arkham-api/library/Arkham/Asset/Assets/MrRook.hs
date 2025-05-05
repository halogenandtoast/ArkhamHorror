module Arkham.Asset.Assets.MrRook (mrRook) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Deck
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers (getAdditionalSearchTargets)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Strategy
import Arkham.Taboo

newtype Metadata = Metadata {chosenCards :: [Card]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype MrRook = MrRook (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mrRook :: AssetCard MrRook
mrRook = ally (MrRook . (`with` Metadata [])) Cards.mrRook (2, 2)

instance HasAbilities MrRook where
  getAbilities (MrRook (a `With` _)) =
    [ restricted a 1 ControlsThis
        $ (if tabooed TabooList20 a then actionAbilityWithCost else FastAbility)
          (exhaust a <> assetUseCost a Secret 1)
    ]

instance RunMessage MrRook where
  runMessage msg a@(MrRook (attrs `With` meta)) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid do
        for_ [3, 6, 9] \x ->
          labeled ("Top " <> tshow x) do
            search iid (attrs.ability 1) iid [fromTopOfDeck x] #any (defer attrs IsDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      focusCards cards do
        chooseOneM iid do
          targets cards \card -> do
            unfocusCards
            handleTarget iid attrs card
            doStep 1 msg
      pure a
    DoStep 1 msg'@(SearchFound iid (isTarget attrs -> True) _ cards) -> do
      additionalTargets <- getAdditionalSearchTargets iid

      let
        chosenWeakness = any (`cardMatch` WeaknessCard) (chosenCards meta)
        anyWeaknesses = any (`cardMatch` WeaknessCard) cards
        chosenNonWeakness = filter (not . (`cardMatch` WeaknessCard)) (chosenCards meta)
        canChooseMore = length chosenNonWeakness < additionalTargets + 1 && length (chosenCards meta) < length cards
        needsToChooseWeakness = not chosenWeakness && anyWeaknesses

      -- if we need to draw weakness, or we need to draw more, repeat step 1
      -- else we go to step 2
      if canChooseMore || needsToChooseWeakness
        then focusCards cards do
          chooseOneM iid do
            for_ cards \card -> do
              when (and [(card `cardMatch` WeaknessCard) || canChooseMore, card `notElem` chosenCards meta]) do
                targeting card do
                  unfocusCards
                  handleTarget iid attrs card
                  doStep 1 msg'
        else doStep 2 msg'
      pure a
    DoStep 2 (SearchFound iid (isTarget attrs -> True) _ _) -> do
      push $ DrawToHandFrom iid (toDeck iid) (chosenCards meta)
      pure $ MrRook (attrs `with` Metadata [])
    HandleTargetChoice _ (isSource attrs -> True) (CardIdTarget cid) -> do
      card <- getCard cid
      pure $ MrRook $ attrs `with` Metadata {chosenCards = card : chosenCards meta}
    _ -> MrRook . (`with` meta) <$> liftRunMessage msg attrs
