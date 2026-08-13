module Arkham.Homebrew.DarkMatter.Treacheries.DarkReflectionsMurderer (
  darkReflectionsMurderer,
) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted hiding (DeckHasNoCards)

{- | "Hidden. Peril.
Revelation - Secretly add this card to your hand.
Forced - After you reshuffle your deck because there are no cards in it: Discard
this card and take 3 horror.
[action] Discard an [[Ally]] asset controlled by an investigator at your
location: Discard Dark Reflections (Murderer)."
-}
newtype DarkReflectionsMurderer = DarkReflectionsMurderer TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkReflectionsMurderer :: TreacheryCard DarkReflectionsMurderer
darkReflectionsMurderer = treachery DarkReflectionsMurderer Cards.darkReflectionsMurderer

discardableAllies :: AssetMatcher
discardableAllies = #ally <> DiscardableAsset <> AssetControlledBy (InvestigatorAt YourLocation)

instance HasAbilities DarkReflectionsMurderer where
  getAbilities (DarkReflectionsMurderer a) =
    [ mkAbility a 1 $ forced $ DeckHasNoCards #after You
    , restricted a 2 (exists discardableAllies) actionAbility
    ]

instance RunMessage DarkReflectionsMurderer where
  runMessage msg t@(DarkReflectionsMurderer attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid attrs 3
      toDiscardBy iid attrs attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      allies <- select discardableAllies
      chooseTargetM iid allies $ toDiscardBy iid (attrs.ability 2)
      toDiscardBy iid attrs attrs
      pure t
    _ -> DarkReflectionsMurderer <$> liftRunMessage msg attrs
