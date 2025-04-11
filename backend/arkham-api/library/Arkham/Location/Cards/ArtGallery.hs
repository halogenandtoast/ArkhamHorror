module Arkham.Location.Cards.ArtGallery (artGallery) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (artGallery)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ArtGallery = ArtGallery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

artGallery :: LocationCard ArtGallery
artGallery = location ArtGallery Cards.artGallery 2 (PerPlayer 1)

instance HasAbilities ArtGallery where
  getAbilities (ArtGallery x) =
    extendRevealed1 x
      $ restricted x 1 Here
      $ forced
      $ SkillTestResult #after You (WhileInvestigating $ be x) #failure

instance RunMessage ArtGallery where
  runMessage msg l@(ArtGallery attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseResources iid (attrs.ability 1) 2
      pure l
    _ -> ArtGallery <$> liftRunMessage msg attrs
