module Arkham.Location.Cards.TearThroughSpace (tearThroughSpace, TearThroughSpace (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Label (mkLabel)
import Arkham.Location.Cards qualified as Cards (tearThroughSpace)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Name hiding (labeled)
import Control.Monad.Extra (findM)

newtype TearThroughSpace = TearThroughSpace LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tearThroughSpace :: LocationCard TearThroughSpace
tearThroughSpace = location TearThroughSpace Cards.tearThroughSpace 1 (Static 1)

instance HasAbilities TearThroughSpace where
  getAbilities (TearThroughSpace attrs) =
    extendRevealed attrs [mkAbility attrs 1 $ forced $ RoundEnds #when]

instance RunMessage TearThroughSpace where
  runMessage msg l@(TearThroughSpace attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid do
        labeled "Place 1 doom on Tear through Space" $ placeDoom (attrs.ability 1) attrs 1
        labeled "Discard Tear through Space" $ toDiscard (attrs.ability 1) attrs
      pure l
    Revelation _ (isSource attrs -> True) -> do
      let labels = [nameToLabel (toName attrs) <> tshow @Int n | n <- [1 .. 4]]
      availableLabel <- findM (selectNone . LocationWithLabel . mkLabel) labels
      case availableLabel of
        Just label -> pure . TearThroughSpace $ attrs & labelL .~ label
        Nothing -> error "could not find label"
    _ -> TearThroughSpace <$> liftRunMessage msg attrs
