module Arkham.Homebrew.AgesUnwound.Locations.London (london) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move (moveTo)

newtype London = London LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

london :: LocationCard London
london = symbolLabel $ location London Cards.london 4 (PerPlayer 1)

{- | "As an additional cost to investigate London, choose and discard a card from
hand."

'AdditionalCostToInvestigate' is the seam every printed "additional cost to
investigate" uses, so the discard is paid as part of the Investigate action and
an investigator with an empty hand simply cannot take it.
-}
instance HasModifiersFor London where
  getModifiersFor (London a) =
    whenRevealed a $ modifySelf a [AdditionalCostToInvestigate (HandDiscardCost 1 #any)]

-- | "[action][action]: __Move__ to Arkham, Massachusetts."
instance HasAbilities London where
  getAbilities (London a) =
    extendRevealed1 a
      $ campaignI18n
      $ withI18nTooltip "london.move"
      $ restricted a 1 (Here <> exists (locationIs Cards.arkhamMassachusetts_111))
      $ ActionAbility #move Nothing (ActionCost 2)

instance RunMessage London where
  runMessage msg l@(London attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectForMaybeM (locationIs Cards.arkhamMassachusetts_111) $ moveTo (attrs.ability 1) iid
      pure l
    _ -> London <$> liftRunMessage msg attrs
