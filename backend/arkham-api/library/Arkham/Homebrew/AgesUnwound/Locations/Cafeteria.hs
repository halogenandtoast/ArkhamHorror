module Arkham.Homebrew.AgesUnwound.Locations.Cafeteria (cafeteria) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Location.Import.Lifted
import Arkham.Message (pattern ResolveHauntedAbilities)

newtype Cafeteria = Cafeteria LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cafeteria :: LocationCard Cafeteria
cafeteria = symbolLabel $ location Cafeteria Cards.cafeteria 5 (PerPlayer 1)

{- | "Haunted - Lose all remaining actions and end your turn. /
[action][action]: Investigate. If you fail, do not trigger this location's
haunted ability."
-}
instance HasAbilities Cafeteria where
  getAbilities (Cafeteria a) =
    extendRevealed
      a
      [ campaignI18n $ withI18nTooltip "cafeteria.investigate" $ investigateAbility a 1 (ActionCost 1) Here
      , campaignI18n $ hauntedI "cafeteria.haunted" a 2
      ]

instance RunMessage Cafeteria where
  runMessage msg l@(Cafeteria attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigate sid iid (attrs.ability 1)
      pure l
    {- "If you fail, do not trigger this location's haunted ability."

    The skill test pushes its `When FailedSkillTest` messages immediately ahead
    of the `ResolveHauntedAbilities` it queues for the investigated location
    (`SkillTest/Runner.hs:1024`), so by the time this fires that message is
    still in the queue and can be dropped. A modifier cannot do this:
    `ResolveHauntedAbilities` selects `HauntedAbility` straight out of
    `getGameAbilities` and never consults criteria or
    `CannotTriggerAbilityMatching`. -}
    FailedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      matchingDon't \case
        ResolveHauntedAbilities _ lid -> lid == attrs.id
        _ -> False
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      setActions iid (attrs.ability 2) 0
      endYourTurn iid
      pure l
    _ -> Cafeteria <$> liftRunMessage msg attrs
