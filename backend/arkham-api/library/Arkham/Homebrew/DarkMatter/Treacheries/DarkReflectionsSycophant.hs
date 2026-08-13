module Arkham.Homebrew.DarkMatter.Treacheries.DarkReflectionsSycophant (
  darkReflectionsSycophant,
) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted hiding (DeckHasNoCards)

{- | "Hidden. Peril.
Revelation - Secretly add this card to your hand.
Forced - After you reshuffle your deck because there are no cards in it: Discard
this card and take 3 damage.
[action] Choose an investigator at your location to spend 4 resources: Discard
this card."
-}
newtype DarkReflectionsSycophant = DarkReflectionsSycophant TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkReflectionsSycophant :: TreacheryCard DarkReflectionsSycophant
darkReflectionsSycophant = treachery DarkReflectionsSycophant Cards.darkReflectionsSycophant

solventInvestigators :: InvestigatorMatcher
solventInvestigators = InvestigatorAt YourLocation <> InvestigatorWithResources (atLeast 4)

instance HasAbilities DarkReflectionsSycophant where
  getAbilities (DarkReflectionsSycophant a) =
    [ mkAbility a 1 $ forced $ DeckHasNoCards #after You
    , restricted a 2 (exists solventInvestigators) actionAbility
    ]

instance RunMessage DarkReflectionsSycophant where
  runMessage msg t@(DarkReflectionsSycophant attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid attrs 3
      toDiscardBy iid attrs attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      payers <- select solventInvestigators
      chooseTargetM iid payers (`spendResources` 4)
      toDiscardBy iid attrs attrs
      pure t
    _ -> DarkReflectionsSycophant <$> liftRunMessage msg attrs
