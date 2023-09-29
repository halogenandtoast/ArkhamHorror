module Arkham.Treachery.Cards.LostInTime (
  lostInTime,
  LostInTime (..),
) where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.Classes
import Arkham.Matcher
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
          push
            $ chooseOne
              iid
              [ targetLabel aid
                $ [ MovedDamage (toSource aid) (toTarget iid) dmg
                  | dmg > 0
                  ]
                <> [ MovedHorror (toSource aid) (toTarget iid) hrr
                   | hrr > 0
                   ]
                <> [shuffleIntoDeck iid aid, CheckDefeated (toSource attrs)]
              | (aid, dmg, hrr) <- assetsWithDamageAndHorror
              ]
        else pushAll $ replicate 3 $ toMessage $ chooseAndDiscardCard iid attrs
      pure t
    _ -> LostInTime <$> runMessage msg attrs
