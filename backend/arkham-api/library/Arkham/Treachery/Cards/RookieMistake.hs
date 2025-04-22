module Arkham.Treachery.Cards.RookieMistake (rookieMistake) where

import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RookieMistake = RookieMistake TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rookieMistake :: TreacheryCard RookieMistake
rookieMistake = treachery RookieMistake Cards.rookieMistake

instance RunMessage RookieMistake where
  runMessage msg t@(RookieMistake attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      assets <- select $ assetControlledBy iid <> oneOf [AssetWithDamage, AssetWithHorror]
      if null assets
        then shuffleIntoDeck iid attrs
        else for_ assets (toDiscardBy iid attrs)
      pure t
    _ -> RookieMistake <$> liftRunMessage msg attrs
