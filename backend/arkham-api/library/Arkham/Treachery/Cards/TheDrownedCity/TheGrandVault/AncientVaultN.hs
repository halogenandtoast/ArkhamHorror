module Arkham.Treachery.Cards.TheDrownedCity.TheGrandVault.AncientVaultN (ancientVaultN) where

import Arkham.Ability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Story (readStory)
import Arkham.I18n
import Arkham.Location.Types (Field (..))
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenarios.TheDrownedCity.TheGrandVault.Helpers (scenarioI18n)
import Arkham.Story.CardDefs.TheDrownedCity.TheGrandVault qualified as Stories
import Arkham.Treachery.CardDefs.TheDrownedCity.TheGrandVault qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AncientVaultN = AncientVaultN TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientVaultN :: TreacheryCard AncientVaultN
ancientVaultN = treachery AncientVaultN Cards.ancientVaultN

instance HasAbilities AncientVaultN where
  getAbilities (AncientVaultN a) = [restricted a 1 OnSameLocation actionAbility]

instance RunMessage AncientVaultN where
  runMessage msg t@(AncientVaultN attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- Attach to your location. Cannot be canceled (revelations are not
      -- cancelable here, so no extra handling is required).
      withLocationOf iid (attachTreachery attrs)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- Cost: take a combined total of X damage and/or horror, where X is the
      -- attached location's shroud. The player chooses how to split the total.
      x <- case attrs.placement of
        AttachedToLocation lid -> fieldWithDefault 0 LocationShroud lid
        _ -> pure 0
      scenarioI18n
        $ chooseAmounts
          iid
          (ikey' "label.ancientVault.takeDamageAndHorror")
          (TotalAmountTarget x)
          [("$damage", (0, x)), ("$horror", (0, x))]
          (toTarget attrs)
      pure t
    ResolveAmounts iid choices (isTarget attrs -> True) -> do
      let damage = getChoiceAmount "$damage" choices
      let horror = getChoiceAmount "$horror" choices
      assignDamageAndHorror iid (attrs.ability 1) damage horror
      flipOver iid attrs
      pure t
    Flip iid _ (isTarget attrs -> True) -> do
      -- The back (11609b) is a story card that translates the glyph and adds itself
      -- to the victory display. A treachery has no UI slot a story can replace, so
      -- the runner focuses the story card and waits for the player to click it.
      readStory iid attrs Stories.ancientVaultN
      pure t
    _ -> AncientVaultN <$> liftRunMessage msg attrs
