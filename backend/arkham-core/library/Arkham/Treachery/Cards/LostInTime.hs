module Arkham.Treachery.Cards.LostInTime
  ( lostInTime
  , LostInTime(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Types ( Field (..) )
import Arkham.Classes
import Arkham.Deck
import Arkham.Matcher
import Arkham.Message hiding ( AssetDamage )
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype LostInTime = LostInTime TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostInTime :: TreacheryCard LostInTime
lostInTime = treachery LostInTime Cards.lostInTime

instance RunMessage LostInTime where
  runMessage msg t@(LostInTime attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      assets <- selectList $ assetControlledBy iid <> AssetNonStory
      assetsWithDamageAndHorror <- for assets $ \asset -> do
        damage <- field AssetDamage asset
        horror <- field AssetHorror asset
        pure (asset, damage, horror)
      if notNull assetsWithDamageAndHorror
        then do
          push $ chooseOne
            iid
            [ targetLabel
                aid
                (ShuffleIntoDeck (InvestigatorDeck iid) (toTarget attrs)
                : [ InvestigatorDamage iid (toSource attrs) dmg hrr
                  | dmg > 0 || hrr > 0
                  ]
                )
            | (aid, dmg, hrr) <- assetsWithDamageAndHorror
            ]
        else pushAll $ replicate 3 (ChooseAndDiscardCard iid (toSource attrs))
      pure t
    _ -> LostInTime <$> runMessage msg attrs
