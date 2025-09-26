module Arkham.Location.Cards.CourtOfTheGreatOldOnesANotTooDistantFuture (
  courtOfTheGreatOldOnesANotTooDistantFuture,
) where

import Arkham.Ability
import Arkham.Effect.Builder
import Arkham.Effect.Window
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Scenarios.TheSecretName.Helpers

newtype CourtOfTheGreatOldOnesANotTooDistantFuture
  = CourtOfTheGreatOldOnesANotTooDistantFuture LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

courtOfTheGreatOldOnesANotTooDistantFuture
  :: LocationCard CourtOfTheGreatOldOnesANotTooDistantFuture
courtOfTheGreatOldOnesANotTooDistantFuture =
  location
    CourtOfTheGreatOldOnesANotTooDistantFuture
    Cards.courtOfTheGreatOldOnesANotTooDistantFuture
    3
    (PerPlayer 2)

instance HasAbilities CourtOfTheGreatOldOnesANotTooDistantFuture where
  getAbilities (CourtOfTheGreatOldOnesANotTooDistantFuture a) =
    extendRevealed
      a
      [ skillTestAbility $ mkAbility a 1 $ forced $ Enters #after You (be a)
      , scenarioI18n $ hauntedI "courtOfTheGreatOldOnesANotTooDistantFuture.haunted" a 2
      ]

instance RunMessage CourtOfTheGreatOldOnesANotTooDistantFuture where
  runMessage msg l@(CourtOfTheGreatOldOnesANotTooDistantFuture attrs) = runQueueT $ case msg of
    Msg.RevealLocation _ lid | lid == toId attrs -> do
      CourtOfTheGreatOldOnesANotTooDistantFuture
        <$> liftRunMessage msg (attrs & labelL .~ "courtOfTheGreatOldOnesANotTooDistantFuture")
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      effectWithSource (attrs.ability 2) iid do
        removeOn $ firstWindow [#round, #nextAction]
        apply $ MustTakeAction #investigate
      pure l
    FailedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      assignHorror iid (attrs.ability 1) n
      pure l
    _ -> CourtOfTheGreatOldOnesANotTooDistantFuture <$> liftRunMessage msg attrs
