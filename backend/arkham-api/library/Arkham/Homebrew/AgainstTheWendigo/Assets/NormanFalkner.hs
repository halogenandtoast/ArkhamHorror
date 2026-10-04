module Arkham.Homebrew.AgainstTheWendigo.Assets.NormanFalkner (normanFalkner) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Matcher hiding (AssetDefeated)
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Log (record)

newtype NormanFalkner = NormanFalkner AssetAttrs
  -- "You cannot place horror on Norman Falkner" -- he is printed with no
  -- sanity at all, so there is nowhere for horror to go.
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

normanFalkner :: AssetCard NormanFalkner
normanFalkner = allyWith NormanFalkner Cards.normanFalkner (2, 0) noSlots

instance HasAbilities NormanFalkner where
  getAbilities (NormanFalkner a) =
    [ -- "At the beginning of the enemy phase: Place 1 doom on Norman Falkner."
      mkAbility a 1 $ forced $ PhaseBegins #when #enemy
    , -- "{action}: Parley. Test [intellect] (4). If you succeed, put Norman
      -- Falkner and the doom tokens on him out of play."
      restricted a 2 OnSameLocation parleyAction_
    , mkAbility a 3 $ forced $ oneOf [Matcher.AssetDefeated #when ByAny (be a), AssetLeavesPlay #when (be a)]
    ]

instance RunMessage NormanFalkner where
  runMessage msg a@(NormanFalkner attrs) = runQueueT $ case msg of
    -- "Revelation - An investigator in the Swamp takes control of Norman Falkner."
    Revelation _ (isSource attrs -> True) -> do
      selectForMaybeM (InvestigatorAt $ locationIs Locations.swamp) \iid ->
        takeControlOfAsset iid attrs.id
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 2) attrs #intellect (Fixed 4)
      pure a
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      removeFromGame attrs
      pure a
    -- "If Norman Falkner is defeated or discarded: Put him out of play, erase
    -- that Norman is alive and record that you let Norman die instead."
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      -- The scenario records "Norman is alive" only at the end, so there is
      -- nothing to erase here.
      record YouLetNormanDie
      removeFromGame attrs
      pure a
    _ -> NormanFalkner <$> liftRunMessage msg attrs
