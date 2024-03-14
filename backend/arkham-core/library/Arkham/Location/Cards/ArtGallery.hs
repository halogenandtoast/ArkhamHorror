module Arkham.Location.Cards.ArtGallery (artGallery, ArtGallery (..)) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (artGallery)
import Arkham.Location.Runner
import Arkham.Matcher

newtype ArtGallery = ArtGallery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

artGallery :: LocationCard ArtGallery
artGallery = location ArtGallery Cards.artGallery 2 (PerPlayer 1)

instance HasAbilities ArtGallery where
  getAbilities (ArtGallery x) =
    extendRevealed
      x
      [ restrictedAbility x 1 Here
          $ forced
          $ SkillTestResult #after You (WhileInvestigating $ be x) #failure
      ]

instance RunMessage ArtGallery where
  runMessage msg l@(ArtGallery attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ SpendResources iid 2
      pure l
    _ -> ArtGallery <$> runMessage msg attrs
