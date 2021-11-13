module Arkham.Types.Treachery.Cards.Corrosion
  ( corrosion
  , Corrosion(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Corrosion = Corrosion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corrosion :: TreacheryCard Corrosion
corrosion = treachery Corrosion Cards.corrosion

instance TreacheryRunner env => RunMessage env Corrosion where
  runMessage msg t@(Corrosion attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId @LocationId iid
      shroud <- unShroud <$> getCount lid
      t <$ push (RevelationChoice iid source shroud)
    RevelationChoice iid source n | n > 0 -> do
      assets <- selectList $ DiscardableAsset <> AssetWithTrait Item
      assetsWithCosts <- traverse
        (traverseToSnd (fmap (maybe 0 toPrintedCost . cdCost) . getCardDef))
        assets
      handAssets <-
        selectList $ InHandOf (InvestigatorWithId iid) <> BasicCardMatch
          (CardWithTrait Item <> CardWithType AssetType <> NonWeakness)
      let
        discardAsset (asset, cost) = TargetLabel
          (AssetTarget asset)
          [Discard (AssetTarget asset), RevelationChoice iid source (n - cost)]
        discardHandAsset card = TargetLabel
          (CardIdTarget $ toCardId card)
          [ Discard (CardIdTarget $ toCardId card)
          , RevelationChoice
            iid
            source
            (n - maybe 0 toPrintedCost (cdCost $ toCardDef card))
          ]
      unless (null assets && null handAssets) $ do
        push
          (chooseOne iid
          $ map discardAsset assetsWithCosts
          <> map discardHandAsset handAssets
          )
      pure t
    _ -> Corrosion <$> runMessage msg attrs
