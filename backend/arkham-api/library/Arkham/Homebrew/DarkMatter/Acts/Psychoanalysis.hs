module Arkham.Homebrew.DarkMatter.Acts.Psychoanalysis (psychoanalysis) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern School)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Psychoanalysis = Psychoanalysis ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

psychoanalysis :: ActCard Psychoanalysis
psychoanalysis = act (2, A) Psychoanalysis Cards.psychoanalysis Nothing

{- | "[free] Spend 1[per_investigator] clues, as a group: Switch two adjacent
locations with each other."

TODO(homebrew): the objective — "At the end of the round, if the configuration
of [[School]] locations corresponds to the following schematic, advance" — is
not implemented. The schematic is a 2x3 arrangement of location icons
(circle/square/triangle over cross/slash/moon), but the Electric Nightmare
locations are declared with 'location_', which carries no location symbol, and
the icon-per-location mapping is not present in the card data. Advancing must be
done manually until those symbols are recorded on the card definitions.
-}
instance HasAbilities Psychoanalysis where
  getAbilities (Psychoanalysis a) =
    [mkAbility a 1 $ FastAbility (GroupClueCost (PerPlayer 1) Anywhere)]

instance RunMessage Psychoanalysis where
  runMessage msg a@(Psychoanalysis attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ LocationWithTrait School
      chooseOneM iid $ targets locations \first' -> do
        adjacent <- select $ connectedFrom (LocationWithId first') <> LocationWithTrait School
        chooseOneM iid $ targets adjacent \second' ->
          push $ ScenarioSpecific "switchLocations" (toJSON (first', second'))
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> Psychoanalysis <$> liftRunMessage msg attrs
