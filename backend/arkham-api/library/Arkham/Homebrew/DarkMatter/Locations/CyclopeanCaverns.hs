module Arkham.Homebrew.DarkMatter.Locations.CyclopeanCaverns (cyclopeanCaverns) where

import Arkham.Ability
import Arkham.CampaignLogKey (toCampaignLogKey)
import Arkham.GameValue
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (
  caveOrCarcosaLocation,
  crossOffMemories,
  flipToOtherSide,
  getMemories,
 )
import Arkham.Homebrew.DarkMatter.Key (DarkMatterKey (Memories))
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype CyclopeanCaverns = CyclopeanCaverns LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cyclopeanCaverns :: LocationCard CyclopeanCaverns
cyclopeanCaverns =
  symbolLabel
    $ locationWith CyclopeanCaverns Cards.cyclopeanCaverns 4 (PerPlayer 2) (canBeFlippedL .~ True)

instance HasAbilities CyclopeanCaverns where
  getAbilities (CyclopeanCaverns a) =
    extendRevealed1 a
      $ restricted
        a
        1
        ( Here
            <> exists
              (investigatorAt a <> InvestigatorWithRecordCount (toCampaignLogKey Memories) (atLeast 1))
        )
        freeTrigger_

instance RunMessage CyclopeanCaverns where
  runMessage msg l@(CyclopeanCaverns attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      doStep n msg
      pure l
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      payers <- filterM (fmap (> 0) . getMemories) =<< select (investigatorAt attrs.id)
      if null payers
        then doStep 0 msg'
        else chooseOrRunOneM iid $ targets payers \payer -> do
          crossOffMemories payer 1
          doStep (n - 1) msg'
      pure l
    DoStep 0 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      locations <- select caveOrCarcosaLocation
      chooseTargetM iid locations $ flipOverBy iid (attrs.ability 1)
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      flipToOtherSide iid attrs
      pure l
    _ -> CyclopeanCaverns <$> liftRunMessage msg attrs
