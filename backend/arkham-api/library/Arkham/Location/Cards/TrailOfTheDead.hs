module Arkham.Location.Cards.TrailOfTheDead (trailOfTheDead) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TrailOfTheDead = TrailOfTheDead LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trailOfTheDead :: LocationCard TrailOfTheDead
trailOfTheDead = symbolLabel $ location TrailOfTheDead Cards.trailOfTheDead 2 (PerPlayer 1)

instance HasAbilities TrailOfTheDead where
  getAbilities (TrailOfTheDead a) =
    extendRevealed1 a
      $ restricted a 1 (not_ $ HasSupply Binoculars)
      $ forced
      $ oneOf
        [ SkillTestResult #after You (WhileInvestigating (be a)) #any
        , Explored #after You (be a) AnyExplore
        ]

instance RunMessage TrailOfTheDead where
  runMessage msg l@(TrailOfTheDead attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    _ -> TrailOfTheDead <$> liftRunMessage msg attrs
