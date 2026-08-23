module Arkham.Treachery.Cards.ChildrenOfBlood.NewHorizons.EchoingInDarkness (echoingInDarkness) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Matcher
import Arkham.Treachery.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EchoingInDarkness = EchoingInDarkness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

echoingInDarkness :: TreacheryCard EchoingInDarkness
echoingInDarkness = treachery EchoingInDarkness Cards.echoingInDarkness

instance HasAbilities EchoingInDarkness where
  getAbilities (EchoingInDarkness a) =
    [ restricted a 1 (InThreatAreaOf You)
        $ ForcedAbilityWithCost
          (ResolvesTreachery #after You (not_ (be a) <> TreacheryDrawnFromDeck Deck.EncounterDeck))
          (discardCost a)
    , skillTestAbility $ restricted a 2 (InThreatAreaOf You) actionAbility
    ]

instance RunMessage EchoingInDarkness where
  runMessage msg t@(EchoingInDarkness attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      other <- selectAny $ TreacheryInThreatAreaOf (be iid) <> treacheryIs Cards.echoingInDarkness
      if other then toDiscard attrs attrs else placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEncounterCard iid (attrs.ability 1)
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      toDiscard attrs attrs
      pure t
    _ -> EchoingInDarkness <$> liftRunMessage msg attrs
