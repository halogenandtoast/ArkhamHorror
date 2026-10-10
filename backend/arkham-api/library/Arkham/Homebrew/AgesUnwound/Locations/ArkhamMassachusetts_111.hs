module Arkham.Homebrew.AgesUnwound.Locations.ArkhamMassachusetts_111 (
  arkhamMassachusetts_111,
) where

import Arkham.Ability hiding (resignAction)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Location.Helpers (resignAction)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Token qualified as Token

newtype ArkhamMassachusetts_111 = ArkhamMassachusetts_111 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | /Arkham, Massachusetts/ (@:ages-unwound:111@). The hub: every investigator
starts here, it is the only place that can resign, and it is the only way to
take a resource off /Helping Yourself/.
-}
arkhamMassachusetts_111 :: LocationCard ArkhamMassachusetts_111
arkhamMassachusetts_111 =
  symbolLabel $ location ArkhamMassachusetts_111 Cards.arkhamMassachusetts_111 3 (PerPlayer 1)

{- | "Remove 1 resource from Helping Yourself."

Named by def rather than by title: this scenario gathers exactly one copy and
/Helping Yourself/ is the Task whose resources are the @StrangeAssistance@
tally.
-}
helpingYourself :: TreacheryMatcher
helpingYourself = treacheryIs Treacheries.helpingYourself <> TreacheryWithToken Token.Resource

-- | "__Move__ to London or Paris."
destinations :: LocationMatcher
destinations = mapOneOf locationIs [Cards.london, Cards.paris]

{- | "[action] Investigators at this location spend 2[per_investigator] clues, as
a group: Remove 1 resource from Helping Yourself. / [action][action]: __Move__ to
London or Paris. / [action]: __Resign.__"

The clue cost is 'SameLocationGroupClueCost' because the printed text scopes the
group to "investigators at this location", not to the whole table. The
resource-bearing criterion keeps the action from being spent on a /Helping
Yourself/ that has already run dry (or been completed and removed from the
game) -- there would be nothing to remove.
-}
instance HasAbilities ArkhamMassachusetts_111 where
  getAbilities (ArkhamMassachusetts_111 a) =
    extendRevealed
      a
      [ campaignI18n
          $ withI18nTooltip "arkhamMassachusetts.helpingYourself"
          $ restricted a 1 (Here <> exists helpingYourself)
          $ actionAbilityWithCost (SameLocationGroupClueCost (PerPlayer 2) (be a))
      , campaignI18n
          $ withI18nTooltip "arkhamMassachusetts.move"
          $ restricted a 2 (Here <> exists destinations)
          $ ActionAbility #move Nothing (ActionCost 2)
      , campaignI18n $ withI18nTooltip "arkhamMassachusetts.resign" $ resignAction a
      ]

instance RunMessage ArkhamMassachusetts_111 where
  runMessage msg l@(ArkhamMassachusetts_111 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectForMaybeM helpingYourself \tid ->
        removeTokens (attrs.ability 1) tid Token.Resource 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      lids <- select destinations
      chooseTargetM iid lids $ moveTo (attrs.ability 2) iid
      pure l
    _ -> ArkhamMassachusetts_111 <$> liftRunMessage msg attrs
