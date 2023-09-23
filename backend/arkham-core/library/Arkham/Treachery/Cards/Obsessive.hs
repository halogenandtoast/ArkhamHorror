module Arkham.Treachery.Cards.Obsessive (
  obsessive,
  Obsessive (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Obsessive = Obsessive TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsessive :: TreacheryCard Obsessive
obsessive = treachery Obsessive Cards.obsessive

instance HasAbilities Obsessive where
  getAbilities (Obsessive a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ ForcedAbility
        $ TurnBegins
          Timing.When
          You
    , restrictedAbility a 2 (InThreatAreaOf $ InvestigatorAt YourLocation)
        $ ActionAbility Nothing
        $ ActionCost 2
    ]

instance RunMessage Obsessive where
  runMessage msg t@(Obsessive attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ AttachTreachery (toId attrs) $ InvestigatorTarget iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      cards <-
        selectList
          $ InHandOf (InvestigatorWithId iid)
          <> BasicCardMatch
            (NonWeakness <> CardWithoutKeyword Keyword.Hidden)
      for_ (nonEmpty cards) $ \cards' -> do
        card <- sample cards'
        push $ DiscardCard iid (toAbilitySource attrs 1) (toCardId card)
      pure t
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ Discard (toAbilitySource attrs 2) (toTarget attrs)
      pure t
    _ -> Obsessive <$> runMessage msg attrs
