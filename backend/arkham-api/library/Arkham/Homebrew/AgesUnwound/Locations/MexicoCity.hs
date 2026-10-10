module Arkham.Homebrew.AgesUnwound.Locations.MexicoCity (mexicoCity) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Item))

newtype MexicoCity = MexicoCity LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mexicoCity :: LocationCard MexicoCity
mexicoCity = symbolLabel $ location MexicoCity Cards.mexicoCity 3 (PerPlayer 2)

-- | "While you are in Mexico City, reduce the cost of each [[Item]] asset you play by 1."
instance HasModifiersFor MexicoCity where
  getModifiersFor (MexicoCity a) =
    whenRevealed a $ modifySelect a (investigatorAt a) [ReduceCostOf (#asset <> CardWithTrait Item) 1]

{- | "[action] Discard an [[Item]] asset you control: Gain 2 clues from the token
pool. (Limit once per game.)"

The printed limit names no group, so by the Rules Reference it is a limit per
investigator -- 'playerLimit' rather than 'groupLimit'.
-}
instance HasAbilities MexicoCity where
  getAbilities (MexicoCity a) =
    extendRevealed1 a
      $ campaignI18n
      $ withI18nTooltip "mexicoCity.gainClues"
      $ playerLimit PerGame
      $ restricted a 1 Here
      $ actionAbilityWithCost (DiscardAssetCost $ #item <> AssetControlledBy You)

instance RunMessage MexicoCity where
  runMessage msg l@(MexicoCity attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainClues iid (attrs.ability 1) 2
      pure l
    _ -> MexicoCity <$> liftRunMessage msg attrs
