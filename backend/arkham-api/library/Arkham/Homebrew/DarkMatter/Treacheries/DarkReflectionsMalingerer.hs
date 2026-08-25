module Arkham.Homebrew.DarkMatter.Treacheries.DarkReflectionsMalingerer (
  darkReflectionsMalingerer,
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
[reaction] At the start of the investigator phase, choose an investigator to lose
2 actions and draw the top card of the encounter deck: Discard Dark Reflections
(Malingerer)."
-}
newtype DarkReflectionsMalingerer = DarkReflectionsMalingerer TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkReflectionsMalingerer :: TreacheryCard DarkReflectionsMalingerer
darkReflectionsMalingerer =
  treachery DarkReflectionsMalingerer Cards.darkReflectionsMalingerer

instance HasAbilities DarkReflectionsMalingerer where
  getAbilities (DarkReflectionsMalingerer a) =
    [ restricted a 1 InYourHand $ forced $ DeckHasNoCards #after You
    , restricted a 2 InYourHand $ freeReaction $ PhaseBegins #when #investigation
    ]

instance RunMessage DarkReflectionsMalingerer where
  runMessage msg t@(DarkReflectionsMalingerer attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid attrs 3
      toDiscardBy iid attrs attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      investigators <- select UneliminatedInvestigator
      chooseTargetM iid investigators \victim -> do
        push $ LoseActions victim (attrs.ability 2) 2
        drawEncounterCard victim (attrs.ability 2)
      toDiscardBy iid attrs attrs
      pure t
    _ -> DarkReflectionsMalingerer <$> liftRunMessage msg attrs
