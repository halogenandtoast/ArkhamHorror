module Arkham.Location.Cards.SealedPassage (sealedPassage) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence qualified as ActSequence
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.Direction
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype SealedPassage = SealedPassage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sealedPassage :: LocationCard SealedPassage
sealedPassage =
  location SealedPassage Cards.sealedPassage 9 (Static 0)
    & setConnectsTo (setFromList [LeftOf, RightOf])

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
      n <- getCurrentActStep
      when (n == 1) $ advanceToAct' (attrs.ability 1) 1 Acts.magicAndScience ActSequence.A
      for_ mChamberOfTime \card -> do
        obtainCard card
        chamberOfTime <- placeLocation card
        actModifier (attrs.ability 1) chamberOfTime $ ConnectedToWhen Anywhere (LocationWithId attrs.id)
        actModifier (attrs.ability 1) attrs $ ConnectedToWhen Anywhere (LocationWithId chamberOfTime)
      pure l
    _ -> SealedPassage <$> liftRunMessage msg attrs
