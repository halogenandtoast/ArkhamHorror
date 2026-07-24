module Arkham.Location.Cards.CentralSpire (centralSpire) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Investigator.Types (Field (InvestigatorResources))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.ObsidianCanyons.Helpers (scenarioI18n)

newtype CentralSpire = CentralSpire LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

centralSpire :: LocationCard CentralSpire
centralSpire = location CentralSpire Cards.centralSpire 3 (Static 2)

instance HasModifiersFor CentralSpire where
  getModifiersFor (CentralSpire a) = modifySelf a [CannotBeMoved, CannotLeavePlay]

instance HasAbilities CentralSpire where
  getAbilities (CentralSpire a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 Here actionAbility

instance RunMessage CentralSpire where
  runMessage msg l@(CentralSpire attrs) = runQueueT $ case msg of
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
    -- TODO: The BACK (unrevealed) side's Obsidian-Claw forced entry effect
    -- ("When you would enter this location, if you do not control the Obsidian
    -- Claw: spend 1 clue or test agility(2), else cancel the move") belongs to
    -- the other side of this card and is not implemented here.
    _ -> CentralSpire <$> liftRunMessage msg attrs
