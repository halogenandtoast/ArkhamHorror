module Arkham.Homebrew.AgesUnwound.Locations.Cafeteria (cafeteria) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Cafeteria = Cafeteria LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cafeteria :: LocationCard Cafeteria
cafeteria = symbolLabel $ location Cafeteria Cards.cafeteria 5 (PerPlayer 1)

{- | "Haunted - Lose all remaining actions and end your turn. /
[action][action]: Investigate. If you fail, do not trigger this location's
haunted ability."

The exemption is a criterion on the haunted ability, asked while the failed test
is still the current one: @ResolveHauntedAbilities@ is queued in ST.6 ahead of
ST.7, which is where the bare @FailedSkillTest@ a card handler matches gets
pushed, so cancelling the haunted from a @FailedThisSkillTest@ case ran a step
too late and popped nothing.
-}
instance HasAbilities Cafeteria where
  getAbilities (Cafeteria a) =
    extendRevealed
      a
      [ campaignI18n $ withI18nTooltip "cafeteria.investigate" $ investigateAbility a 1 (ActionCost 2) Here
      , campaignI18n
          $ withI18nTooltip "cafeteria.haunted"
          $ restricted a 2 (not_ $ DuringSkillTest $ SkillTestSourceMatches $ SourceIs (a.ability 1)) Haunted
      ]

instance RunMessage Cafeteria where
  runMessage msg l@(Cafeteria attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigate sid iid (attrs.ability 1)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      setActions iid (attrs.ability 2) 0
      endYourTurn iid
      pure l
    _ -> Cafeteria <$> liftRunMessage msg attrs
