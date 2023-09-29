module Arkham.Treachery.Cards.RealmOfMadness (
  realmOfMadness,
  RealmOfMadness (..),
) where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype RealmOfMadness = RealmOfMadness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

realmOfMadness :: TreacheryCard RealmOfMadness
realmOfMadness = treachery RealmOfMadness Cards.realmOfMadness

assetMatcher :: AssetMatcher
assetMatcher = DiscardableAsset <> AssetWithTrait Item <> AssetControlledBy You

instance RunMessage RealmOfMadness where
  runMessage msg t@(RealmOfMadness attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      horror <- field InvestigatorHorror iid
      hasAssets <- selectAny assetMatcher
      hasDiscardableCards <-
        fieldP
          InvestigatorHand
          (any (`cardMatch` NonWeakness))
          iid
      push
        $ if horror > 0 && (hasAssets || hasDiscardableCards)
          then RevelationChoice iid (toSource attrs) horror
          else InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2
      pure t
    RevelationChoice iid (isSource attrs -> True) n | n > 0 -> do
      assets <- selectWithField AssetCost assetMatcher
      handDiscardableCards <-
        fieldMap
          InvestigatorHand
          (filter (`cardMatch` NonWeakness))
          iid
      let
        discardAsset (asset, cost) =
          targetLabel
            asset
            [Discard (toSource attrs) (AssetTarget asset), RevelationChoice iid (toSource attrs) (n - cost)]
        discardHandCard card =
          TargetLabel
            (CardIdTarget $ toCardId card)
            [ Discard (toSource attrs) (CardIdTarget $ toCardId card)
            , RevelationChoice
                iid
                (toSource attrs)
                (n - maybe 0 toPrintedCost (cdCost $ toCardDef card))
            ]
      unless (null assets && null handDiscardableCards)
        $ push
        $ chooseOne iid
        $ map discardAsset assets
        <> map discardHandCard handDiscardableCards
      pure t
    _ -> RealmOfMadness <$> runMessage msg attrs
