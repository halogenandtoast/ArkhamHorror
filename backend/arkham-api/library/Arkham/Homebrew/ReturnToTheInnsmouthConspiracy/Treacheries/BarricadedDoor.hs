module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Treacheries.BarricadedDoor (barricadedDoor) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype BarricadedDoor = BarricadedDoor TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barricadedDoor :: TreacheryCard BarricadedDoor
barricadedDoor = treachery BarricadedDoor Cards.barricadedDoor

{- | "Instead of discovering clues" is a standing ban on the attached location rather
than a replacement effect: while Barricaded Door is attached nobody can discover a
clue there at all, so a successful investigation yields only the Forced ability below.
-}
instance HasModifiersFor BarricadedDoor where
  getModifiersFor (BarricadedDoor a) = case a.placement of
    AttachedToLocation lid -> modifySelect a Anyone [CannotDiscoverCluesAt (LocationWithId lid)]
    _ -> pure mempty

instance HasAbilities BarricadedDoor where
  getAbilities (BarricadedDoor a) = case a.placement of
    AttachedToLocation lid ->
      [ mkAbility a 1 $ forced $ SuccessfulInvestigation #after Anyone (LocationWithId lid)
      , restricted a 2 OnSameLocation $ ActionAbility mempty Nothing $ ActionCost 2
      ]
    _ -> []

instance RunMessage BarricadedDoor where
  runMessage msg t@(BarricadedDoor attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      selectWhenNotNull
        (LocationWithMostClues $ LocationWithoutTreachery AnyTreachery)
        \locations -> chooseOrRunOneM iid $ targets locations $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      assignDamage iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> BarricadedDoor <$> liftRunMessage msg attrs
