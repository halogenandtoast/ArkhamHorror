module Arkham.Location.Cards.SealedPassage (sealedPassage) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype SealedPassage = SealedPassage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sealedPassage :: LocationCard SealedPassage
sealedPassage = location SealedPassage Cards.sealedPassage 9 (Static 0)

instance HasAbilities SealedPassage where
  getAbilities (SealedPassage a) =
    extendRevealed
      a
      [ groupLimit PerGame $ restricted a 1 (Here <> HasSupply KeyOfEztli) actionAbility
      ]

instance RunMessage SealedPassage where
  runMessage msg l@(SealedPassage attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      mChamberOfTime <- findCard (`cardMatch` cardIs Cards.chamberOfTimeRearrangedByTime)
      for_ mChamberOfTime \card -> do
        obtainCard card
        chamberOfTime <- placeLocation card
        gameModifier (attrs.ability 1) chamberOfTime $ ConnectedToWhen Anywhere (LocationWithId attrs.id)
        gameModifier (attrs.ability 1) attrs $ ConnectedToWhen Anywhere (LocationWithId chamberOfTime)
      pure l
    _ -> SealedPassage <$> liftRunMessage msg attrs
