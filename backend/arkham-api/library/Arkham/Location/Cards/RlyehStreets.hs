module Arkham.Location.Cards.RlyehStreets (rlyehStreets) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Investigator.Types (Field (InvestigatorResources))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.ObsidianCanyons.Helpers (scenarioI18n)

newtype RlyehStreets = RlyehStreets LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rlyehStreets :: LocationCard RlyehStreets
rlyehStreets = location RlyehStreets Cards.rlyehStreets 2 (Static 3)

instance HasModifiersFor RlyehStreets where
  getModifiersFor (RlyehStreets a) = modifySelf a [CannotBeMoved, CannotLeavePlay]

instance HasAbilities RlyehStreets where
  getAbilities (RlyehStreets a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 Here actionAbility

instance RunMessage RlyehStreets where
  runMessage msg l@(RlyehStreets attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- Spend 1-3 resources, capped at what the investigator can actually pay.
      resources <- field InvestigatorResources iid
      scenarioI18n $ chooseAmount' iid "resourcesToSpend" "$resources" 1 (min 3 resources) attrs
      pure l
    ResolveAmounts iid (getChoiceAmount "$resources" -> n) (isTarget attrs -> True) | n > 0 -> do
      spendResources iid n
      revealedLocations <- select RevealedLocation
      chooseTargetM iid revealedLocations \lid ->
        placeClues (attrs.ability 1) lid n
      pure l
    _ -> RlyehStreets <$> liftRunMessage msg attrs
