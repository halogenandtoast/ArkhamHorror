module Arkham.Asset.Assets.DrElliHorowitz (drElliHorowitz, DrElliHorowitz (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.ChaosBag.Base
import Arkham.Helpers.Matchers
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Scenario
import Arkham.Keyword (Sealing (..))
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenario.Types (Field (..))
import Arkham.Strategy
import Arkham.Trait

newtype DrElliHorowitz = DrElliHorowitz AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drElliHorowitz :: AssetCard DrElliHorowitz
drElliHorowitz = ally DrElliHorowitz Cards.drElliHorowitz (1, 2)

instance HasModifiersFor DrElliHorowitz where
  getModifiersFor (DrElliHorowitz a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> modifySelect a (AssetAttachedToAsset (be a)) [AsIfUnderControlOf iid]

instance HasAbilities DrElliHorowitz where
  getAbilities (DrElliHorowitz a) = [controlledAbility a 1 CanManipulateDeck $ freeReaction (AssetEntersPlay #when $ be a)]

instance RunMessage DrElliHorowitz where
  runMessage msg a@(DrElliHorowitz attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- no filter because we need to handle game logic
      search iid (attrs.ability 1) iid [fromTopOfDeck 9] #any $ defer attrs IsNotDraw
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      let validCards = filterCards (CardWithType AssetType <> CardWithTrait Relic) cards
      tokens <- scenarioFieldMap ScenarioChaosBag chaosBagChaosTokens
      let
        validAfterSeal c = do
          let
            sealingToMatcher = \case
              Sealing matcher -> Just matcher
              SealUpTo _ matcher -> Just matcher
              SealOneOf (m1 :| rest) -> Just $ oneOf $ mapMaybe sealingToMatcher (m1 : rest)
              SealUpToX _ -> Nothing
            sealChaosTokenMatchers = flip mapMaybe (setToList c.keywords) \case
              Keyword.Seal sealing -> sealingToMatcher sealing
              _ -> Nothing
          allM (\matcher -> anyM (\t -> matchChaosToken iid t matcher) tokens) sealChaosTokenMatchers
      validCardsAfterSeal <- filterM validAfterSeal validCards
      if null validCardsAfterSeal
        then chooseOne iid [Label "No Cards Found" []]
        else do
          assetId <- getRandom
          additionalTargets <- getAdditionalSearchTargets iid
          chooseN
            iid
            (min (length validCardsAfterSeal) (1 + additionalTargets))
            [ targetLabel c [CreateAssetAt assetId c $ AttachedToAsset attrs.id (Just $ InPlayArea iid)]
            | c <- validCardsAfterSeal
            ]
      pure a
    SearchNoneFound iid (isTarget attrs -> True) -> do
      chooseOne iid [Label "No Cards Found" []]
      pure a
    _ -> DrElliHorowitz <$> liftRunMessage msg attrs
