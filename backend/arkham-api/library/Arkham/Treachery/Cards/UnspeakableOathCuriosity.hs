module Arkham.Treachery.Cards.UnspeakableOathCuriosity (
  unspeakableOathCuriosity,
  UnspeakableOathCuriosity (..),
)
where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Matcher hiding (TreacheryInHandOf)
import Arkham.Matcher qualified as Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnspeakableOathCuriosity = UnspeakableOathCuriosity TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unspeakableOathCuriosity :: TreacheryCard UnspeakableOathCuriosity
unspeakableOathCuriosity = treachery UnspeakableOathCuriosity Cards.unspeakableOathCuriosity

instance HasAbilities UnspeakableOathCuriosity where
  getAbilities (UnspeakableOathCuriosity attrs) =
    [ restrictedAbility attrs 1 InYourHand
        $ forced
        $ oneOf [Matcher.GameEnds #when, Matcher.InvestigatorEliminated #when You]
    , restrictedAbility attrs 2 InYourHand
        $ freeReaction
        $ SuccessfullyInvestigatedWithNoClues #when You Anywhere
    ]

instance RunMessage UnspeakableOathCuriosity where
  runMessage msg t@(UnspeakableOathCuriosity attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeTreachery attrs (HiddenInHand iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gameModifier attrs iid (XPModifier (-2))
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> UnspeakableOathCuriosity <$> liftRunMessage msg attrs
