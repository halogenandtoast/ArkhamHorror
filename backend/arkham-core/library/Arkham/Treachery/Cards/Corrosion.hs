module Arkham.Treachery.Cards.Corrosion (
  corrosion,
  Corrosion (..),
) where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Corrosion = Corrosion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corrosion :: TreacheryCard Corrosion
corrosion = treachery Corrosion Cards.corrosion

handMatcher :: CardMatcher
handMatcher = CardWithTrait Item <> CardWithType AssetType <> NonWeakness

assetMatcher :: AssetMatcher
assetMatcher = DiscardableAsset <> AssetWithTrait Item <> AssetControlledBy You

instance RunMessage Corrosion where
  runMessage msg t@(Corrosion attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      shroud <- maybe (pure 0) (fieldJust LocationShroud) =<< field InvestigatorLocation iid
      hasAssets <- selectAny assetMatcher
      hasHandAssets <-
        fieldP
          InvestigatorHand
          (any (`cardMatch` handMatcher))
          iid
      push
        $ if shroud > 0 && (hasAssets || hasHandAssets)
          then RevelationChoice iid source shroud
          else gainSurge attrs
      pure t
    RevelationChoice iid source n | n > 0 -> do
      assets <- selectWithField AssetCost assetMatcher
      handAssets <- fieldMap InvestigatorHand (filter (`cardMatch` handMatcher)) iid
      let
        discardAsset (asset, cost) =
          targetLabel
            asset
            [toDiscardBy iid attrs asset, RevelationChoice iid source (n - cost)]
        discardHandAsset card =
          TargetLabel
            (CardIdTarget $ toCardId card)
            [ toDiscardBy iid attrs (toCardId card)
            , RevelationChoice
                iid
                source
                (n - maybe 0 toPrintedCost (cdCost $ toCardDef card))
            ]
      player <- getPlayer iid
      unless (null assets && null handAssets)
        $ push
        $ chooseOne player
        $ map discardAsset assets
        <> map discardHandAsset handAssets
      pure t
    _ -> Corrosion <$> runMessage msg attrs
