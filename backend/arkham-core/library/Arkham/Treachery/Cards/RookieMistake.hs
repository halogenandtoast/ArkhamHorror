module Arkham.Treachery.Cards.RookieMistake (
  rookieMistake,
  RookieMistake (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype RookieMistake = RookieMistake TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rookieMistake :: TreacheryCard RookieMistake
rookieMistake = treachery RookieMistake Cards.rookieMistake

instance RunMessage RookieMistake where
  runMessage msg t@(RookieMistake attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      assets <- selectList $ assetControlledBy iid <> AssetOneOf [AssetWithDamage, AssetWithHorror]
      if null assets
        then push $ ShuffleIntoDeck (Deck.InvestigatorDeck iid) (toTarget attrs)
        else pushAll $ map (Discard (toSource attrs) . toTarget) assets
      pure t
    _ -> RookieMistake <$> runMessage msg attrs
