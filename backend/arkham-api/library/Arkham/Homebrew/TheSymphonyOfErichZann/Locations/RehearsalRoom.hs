module Arkham.Homebrew.TheSymphonyOfErichZann.Locations.RehearsalRoom (rehearsalRoom) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype RehearsalRoom = RehearsalRoom LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rehearsalRoom :: LocationCard RehearsalRoom
rehearsalRoom =
  locationWith RehearsalRoom Cards.rehearsalRoom 3 (PerPlayer 1)
    $ costToEnterUnrevealedL
    .~ GroupClueCost (PerPlayer 1) YourLocation

instance HasModifiersFor RehearsalRoom where
  getModifiersFor (RehearsalRoom a) = do
    -- "The door leading to this room is blocked. As an additional cost to move
    -- to Backstage Room, the investigators must spend 1 clue per investigator,
    -- as a group."
    modifySelfWhen a (not a.revealed) [AdditionalCostToEnter $ GroupClueCost (PerPlayer 1) Anywhere]

instance HasAbilities RehearsalRoom where
  getAbilities (RehearsalRoom a) =
    extend
      a
      [ -- "After you succeed by 2 or more during a skill test at the Rehearsal Room: Take 1 horror."
        mkAbility a 1 $ forced $ SkillTestResult #after You (SkillTestAt (be a)) (SuccessResult $ atLeast 2)
      , -- "[action]: Heal 3 horror. (Limit once per game)"
        playerLimit PerGame $ restricted a 2 Here actionAbility
      ]

instance RunMessage RehearsalRoom where
  runMessage msg l@(RehearsalRoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      healHorror iid (attrs.ability 2) 3
      pure l
    _ -> RehearsalRoom <$> liftRunMessage msg attrs
