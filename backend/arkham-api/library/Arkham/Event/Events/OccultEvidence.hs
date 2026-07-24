module Arkham.Event.Events.OccultEvidence (occultEvidence) where

import Arkham.Ability
import Arkham.Card
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Trait (Trait (Research))

newtype OccultEvidence = OccultEvidence EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

occultEvidence :: EventCard OccultEvidence
occultEvidence = event OccultEvidence Cards.occultEvidence

instance HasAbilities OccultEvidence where
  getAbilities (OccultEvidence x) =
    [ playerLimit (PerSearch Research)
        $ mkAbility x 1
        $ triggered (AmongSearchedCards You) (RevealCost $ toCardId x)
    ]

instance RunMessage OccultEvidence where
  runMessage msg e@(OccultEvidence attrs) = runQueueT $ case msg of
    InSearch (UseThisAbility iid (isSource attrs -> True) 1) -> do
      drawToHandFrom iid iid attrs
      withLocationOf iid $ discoverAt NotInvestigate iid (attrs.ability 1) 1
      pure e
    PlayThisEvent iid (is attrs -> True) -> do
      shuffleIntoDeck iid attrs
      pure e
    _ -> OccultEvidence <$> liftRunMessage msg attrs
