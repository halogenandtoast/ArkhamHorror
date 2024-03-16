module Arkham.Treachery.Cards.TasteOfLifeblood
  ( tasteOfLifeblood
  , TasteOfLifeblood(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TasteOfLifeblood = TasteOfLifeblood TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tasteOfLifeblood :: TreacheryCard TasteOfLifeblood
tasteOfLifeblood = treachery TasteOfLifeblood Cards.tasteOfLifeblood

instance RunMessage TasteOfLifeblood where
  runMessage msg t@(TasteOfLifeblood attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> TasteOfLifeblood <$> runMessage msg attrs
