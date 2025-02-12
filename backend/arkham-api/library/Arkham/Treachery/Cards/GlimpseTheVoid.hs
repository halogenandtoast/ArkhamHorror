module Arkham.Treachery.Cards.GlimpseTheVoid (glimpseTheVoid) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype GlimpseTheVoid = GlimpseTheVoid TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glimpseTheVoid :: TreacheryCard GlimpseTheVoid
glimpseTheVoid = treachery GlimpseTheVoid Cards.glimpseTheVoid

instance HasModifiersFor GlimpseTheVoid where
  getModifiersFor (GlimpseTheVoid a) = do
    inThreatAreaGets a [RevealAnotherChaosToken]

instance HasAbilities GlimpseTheVoid where
  getAbilities (GlimpseTheVoid a) =
    [restricted a 1 OnSameLocation actionAbility]

instance RunMessage GlimpseTheVoid where
  runMessage msg t@(GlimpseTheVoid attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      shuffleIntoDeck iid attrs
      pure t
    _ -> GlimpseTheVoid <$> liftRunMessage msg attrs
