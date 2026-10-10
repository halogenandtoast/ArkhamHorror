module Arkham.Homebrew.AgesUnwound.Locations.Istanbul (istanbul) where

import Arkham.Ability
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)

newtype Istanbul = Istanbul LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

istanbul :: LocationCard Istanbul
istanbul = symbolLabel $ location Istanbul Cards.istanbul 2 (PerPlayer 2)

{- | "[action] Spend 2 resources: Heal 1 damage and 1 horror. / [reaction] After
you move to Istanbul, but before enemies at your new location engage you, spend
2 resources: Move to a connecting location."

'MovedButBeforeEnemyEngagement' is the window Track Shoes prints the same
sentence against, so Istanbul is a free hop that outruns engagement. Its
location argument is the move's /destination/
(@Investigator/Runner/Movement.hs:555@), which already pins the trigger to "you
move to Istanbul" -- so ability 2 carries no @Here@ criterion, exactly as Track
Shoes carries none: the window is raised in the same batch that places the
investigator, so @Here@ cannot be relied on to hold yet. The onward move uses
'getAccessibleLocations' rather than a bare connection select, so a blocked or
unenterable neighbour is not offered.
-}
instance HasAbilities Istanbul where
  getAbilities (Istanbul a) =
    extendRevealed
      a
      [ campaignI18n
          $ withI18nTooltip "istanbul.heal"
          $ restricted
            a
            1
            (Here <> any_ [HealableInvestigator (a.ability 1) kind You | kind <- [#damage, #horror]])
          $ actionAbilityWithCost (ResourceCost 2)
      , campaignI18n
          $ withI18nTooltip "istanbul.move"
          $ mkAbility a 2
          $ triggered (MovedButBeforeEnemyEngagement #after You (be a)) (ResourceCost 2)
      ]

instance RunMessage Istanbul where
  runMessage msg l@(Istanbul attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      healDamageIfCan iid (attrs.ability 1) 1
      healHorrorIfCan iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      accessible <- getAccessibleLocations iid (attrs.ability 2)
      chooseTargetM iid accessible $ moveTo (attrs.ability 2) iid
      pure l
    _ -> Istanbul <$> liftRunMessage msg attrs
