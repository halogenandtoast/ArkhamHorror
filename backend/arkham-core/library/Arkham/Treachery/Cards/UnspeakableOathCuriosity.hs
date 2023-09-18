module Arkham.Treachery.Cards.UnspeakableOathCuriosity (
  unspeakableOathCuriosity,
  UnspeakableOathCuriosity (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (TreacheryInHandOf)
import Arkham.Matcher qualified as Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype UnspeakableOathCuriosity = UnspeakableOathCuriosity TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unspeakableOathCuriosity :: TreacheryCard UnspeakableOathCuriosity
unspeakableOathCuriosity = treachery UnspeakableOathCuriosity Cards.unspeakableOathCuriosity

instance HasAbilities UnspeakableOathCuriosity where
  getAbilities (UnspeakableOathCuriosity attrs) =
    [ restrictedAbility attrs 1 InYourHand
        $ ForcedAbility
        $ OrWindowMatcher
          [ Matcher.GameEnds #when
          , Matcher.InvestigatorEliminated #when You
          ]
    , restrictedAbility attrs 2 InYourHand
        $ freeReaction
        $ SuccessfullyInvestigatedWithNoClues #when You Anywhere
    ]

instance RunMessage UnspeakableOathCuriosity where
  runMessage msg t@(UnspeakableOathCuriosity attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ PlaceTreachery (toId attrs) (TreacheryInHandOf iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ gameModifier attrs iid (XPModifier (-2))
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ Discard (toAbilitySource attrs 2) (toTarget attrs)
      pure t
    _ -> UnspeakableOathCuriosity <$> runMessage msg attrs
