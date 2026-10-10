module Arkham.Homebrew.AgesUnwound.Locations.SanFrancisco (sanFrancisco) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)

newtype SanFrancisco = SanFrancisco LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sanFrancisco :: LocationCard SanFrancisco
sanFrancisco = symbolLabel $ location SanFrancisco Cards.sanFrancisco 2 (PerPlayer 1)

{- | "During the upkeep phase, investigators in San Francisco gain 1 additional
resource."

'UpkeepResources' is the engine's handle on the upkeep collection itself (Jenny
Barnes' printed text), so the bonus rides the collection rather than being a
separate gain -- which matters for every "when you gain resources" rider and for
Dark Horse, who chooses not to collect at all.
-}
instance HasModifiersFor SanFrancisco where
  getModifiersFor (SanFrancisco a) =
    whenRevealed a $ modifySelect a (investigatorAt a) [UpkeepResources 1]

-- | "__Move__ to Shanghai or Sydney."
destinations :: LocationMatcher
destinations = mapOneOf locationIs [Cards.shanghai, Cards.sydney]

-- | "[action][action]: __Move__ to Shanghai or Sydney."
instance HasAbilities SanFrancisco where
  getAbilities (SanFrancisco a) =
    extendRevealed1 a
      $ campaignI18n
      $ withI18nTooltip "sanFrancisco.move"
      $ restricted a 1 (Here <> exists destinations)
      $ ActionAbility #move Nothing (ActionCost 2)

instance RunMessage SanFrancisco where
  runMessage msg l@(SanFrancisco attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lids <- select destinations
      chooseTargetM iid lids $ moveTo (attrs.ability 1) iid
      pure l
    _ -> SanFrancisco <$> liftRunMessage msg attrs
