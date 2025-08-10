module Arkham.Location.Cards.ReturnToXochimilco (returnToXochimilco) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ReturnToXochimilco = ReturnToXochimilco LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToXochimilco :: LocationCard ReturnToXochimilco
returnToXochimilco = symbolLabel $ location ReturnToXochimilco Cards.returnToXochimilco 3 (Static 0)

instance HasAbilities ReturnToXochimilco where
  getAbilities (ReturnToXochimilco a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ freeReaction
      $ SkillTestResult #after (You <> at_ (be a)) (oneOf [#fighting, #evading]) #success

instance RunMessage ReturnToXochimilco where
  runMessage msg l@(ReturnToXochimilco attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Explore iid (attrs.ability 1) $ CardWithPrintedLocationSymbol $ locationSymbol attrs
      pure l
    _ -> ReturnToXochimilco <$> liftRunMessage msg attrs
